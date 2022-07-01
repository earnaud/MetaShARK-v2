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
        tags$a("here.", href = "https://eml.ecoinformatics.org/schema/")
      ),
      # search sidebar
      column(
        5,
        wellPanel(
          shinyTree::shinyTree(
            outputId = ns("tree"),
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
        uiOutput(ns("doc"))
      )
    )
  )
}

#' @import shiny
#' @importFrom shinyTree shinyTree renderTree get_selected
#' @importFrom jsonlite unserializeJSON read_json
#'
#' @noRd
documentation <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    local_rv <- reactiveValues(
      doc = c(),
      tree = c()
    )

    # Load data ====
    observe({
      req(main_env$current_tab() == "documentation")
      req(isFALSE("tree" %in% names(input)))

      withProgress({
          doc <- app_sys("resources/doc_guideline.json")
          local_rv$doc <- jsonlite::read_json(doc)[[1]] |>
            jsonlite::unserializeJSON()
          incProgress(0.5)

          tree <- app_sys("resources/tree_guideline.json")
          local_rv$tree <- jsonlite::unserializeJSON(jsonlite::read_json(tree)[[1]])
          incProgress(0.5)
        },
        message = "Rendering documentation."
      )
    })

    # UI render ====

    ## render tree ----
    output$tree <- shinyTree::renderTree({
      validate(
        need(isContentTruthy(local_rv$tree), "Documentation is being loaded.")
      )

      local_rv$tree
    })

    ## output selected node ----
    output$doc <- renderUI({
      # check tree presence
      validate(
        need(isContentTruthy(local_rv$tree), "Documentation is being loaded."),
        need("tree" %in% names(input), "Please select a node.")
      )

      # check tree selection
      tree_node <- shinyTree::get_selected(input$tree)
      validate(
        need(unlist(tree_node), "(Select an item first)")
      )

      # read path to selected node in tree
      path <- paste(
        c(
          attr(tree_node[[1]], "ancestry"),
          unlist(tree_node)
        ),
        collapse = "/"
      )
      # access the selected node
      doc_node <- followPath(local_rv$doc, path)
      # return curated display
      if ("annotation" %in% names(doc_node)) {
        doc_node$annotation
      } else {
        helpText("No content found at:", path)
      }
    })
  })
}
