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
#' @importFrom shinyTree renderTree get_selected
#' @importFrom jsonlite read_json unserializeJSON
#'
#' @noRd
documentation <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Load data ====
    doc <- system.file("resources/doc_guideline.json", package = "MetaShARK")
    doc <- jsonlite::unserializeJSON(jsonlite::read_json(doc)[[1]])

    tree <- system.file("resources/tree_guideline.json", package = "MetaShARK")
    tree <- jsonlite::unserializeJSON(jsonlite::read_json(tree)[[1]])

    # UI render ====

    # render tree
    output$tree <- shinyTree::renderTree({
      tree
    })

    # output selected node
    output$doc <- renderUI({
      req("tree" %in% names(input))
      tree.node <- get_selected(input$tree)
      validate(
        need(unlist(tree.node), "(Select an item first)")
      )
      path <- paste(
        c(
          attr(tree.node[[1]], "ancestry"),
          unlist(tree.node)
        ),
        collapse = "/"
      )
      doc.node <- followPath(doc, path)
      if ("annotation" %in% names(doc.node)) {
        doc.node$annotation
      } else {
        helpText("No content found at:", path)
      }
    })
  })
}
