#' @import shiny
#' @importFrom shinyTree shinyTree
#'
#' @noRd
docUI <- function(id) {
  ns <- NS(id)

  # UI output
  tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "www/styles.css"
      )
    ),
    fluidRow(
      # original documentation
      tags$h5(
        "Original documentation is available ", 
        tags$a("here.", href="https://eml.ecoinformatics.org/schema/")
      ),
      # search sidebar
      column(
        5,
        wellPanel(
          shinyTree::shinyTree(
            outputId = NS(id, "tree"),
            search = TRUE,
            theme = "proton",
            wholerow = TRUE,
            multiple = FALSE
          )
        )
      ),
      # display main panel
      column(
        7,
        uiOutput(NS(id, "doc"))
      )
    )
  )
}

#' @import shiny
#' @importFrom shinyTree shinyTree renderTree get_selected
#' @importFrom jsonlite unserializeJSON read_json
#'
#' @noRd
documentation <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {

    local.rv <- reactiveValues(
      doc = c(),
      tree = c()
    )
    
    # Load data ====
    observe({
      req(main.env$current.tab() == "documentation")
      req(isFALSE("tree" %in% names(input)))
      
      withProgress({
        doc <- system.file("resources/doc_guideline.json", package = "MetaShARK")
        local.rv$doc <- jsonlite::unserializeJSON(jsonlite::read_json(doc)[[1]])
        incProgress(0.5)
        
        tree <- system.file("resources/tree_guideline.json", package = "MetaShARK")
        local.rv$tree <- jsonlite::unserializeJSON(jsonlite::read_json(tree)[[1]])
        incProgress(0.5)
      },
      message = "Rendering documentation.")
    })

    # UI render ====

    ## render tree ----
    output$tree <- shinyTree::renderTree({
      validate(
        need(isContentTruthy(local.rv$tree), "Documentation is being loaded.")
      )
      
      local.rv$tree
    })

    ## output selected node ----
    output$doc <- renderUI({
      # check tree presence
      validate(
        need(isContentTruthy(local.rv$tree), "Documentation is being loaded."),
        need("tree" %in% names(input), "Please select a node.")
      )
      
      # check tree selection
      tree.node <- shinyTree::get_selected(input$tree)
      validate(
        need(unlist(tree.node), "(Select an item first)")
      )
      
      # read path to selected node in tree
      path <- paste(
        c(
          attr(tree.node[[1]], "ancestry"),
          unlist(tree.node)
        ),
        collapse = "/"
      )
      # access the selected node
      doc.node <- followPath(local.rv$doc, path)
      # return curated display
      if ("annotation" %in% names(doc.node)) {
        doc.node$annotation
      } else {
        helpText("No content found at:", path)
      }
    })
  })
}
