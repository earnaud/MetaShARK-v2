#' @title docUI
#'
#' @description UI part of the documentation module
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom shinyTree shinyTree
docUI <- function(id) {
  ns <- NS(id)

  require(shinyTree)

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
            outputId = ns("tree"),
            search = TRUE,
            theme = "proton"
          )
        )
      ),
      # display main panel
      column(
        7,
        div(
          id = "docPanel",
          uiOutput(ns("doc"))
        )
      )
    )
  )
}

#' @title documentation
#'
#' @description server part of the documentation module.
#'
#' @import shiny
#' @importFrom shinyTree renderTree get_selected
#' @importFrom jsonlite read_json unserializeJSON
documentation <- function(input, output, session) {
  ns <- session$ns

  # Load data ====
  withProgress(message = "Loading documentation.", {
    doc <- system.file("resources/doc_guideline.json", package = "MetaShARK") %>%
      jsonlite::read_json(simplifyVector = TRUE) %>%
      jsonlite::unserializeJSON()
    incProgress(0.5)
    tree <- system.file("resources/tree_guideline.json", package = "MetaShARK") %>%
      jsonlite::read_json(simplifyVector = TRUE) %>%
      jsonlite::unserializeJSON()
    incProgress(0.5)
  })

  # UI render ====

  # render tree
  output$tree <- shinyTree::renderTree(tree)

  # output selected node
  output$doc <- renderUI({
    tree.node <- shinyTree::get_selected(input$tree)
    validate(
      need(unlist(tree.node), "(Select an item first)")
    )
    path <- paste(c(attr(tree.node[[1]], "ancestry"), unlist(tree.node)), collapse = "/")
    doc.node <- followPath(doc, path)
    if ("annotation" %in% names(doc.node)) {
      doc.node$annotation
    } else {
      helpText("No content found at:", path)
    }
  })
}
