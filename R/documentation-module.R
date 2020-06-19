#' @title docUI
#'
#' @description UI part of the documentation module
#'
#' @importFrom shiny NS tagList tags fluidRow column selectInput div
#' @importFrom shinydashboard box
#' @importFrom shinyTree shinyTree
#' @importFrom shinycssloaders withSpinner
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
          shinyTree(
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
#' @importFrom shiny observeEvent renderText validate helpText
#' @importFrom shinyTree renderTree get_selected
#' @importFrom utils browseURL
#' @importFrom jsonlite read_json unserializeJSON
documentation <- function(input, output, session) {
  ns <- session$ns

  # Load data ====
  withProgress(message = "Loading documentation.", {
    doc <- system.file("resources/doc_guideline.json", package = "MetaShARK") %>%
      read_json(simplifyVector = TRUE) %>%
      unserializeJSON()
    incProgress(0.9)
    tree <- system.file("resources/tree_guideline.json", package = "MetaShARK") %>%
      read_json(simplifyVector = TRUE) %>%
      unserializeJSON()
  })

  # UI render ====

  # render tree
  output$tree <- renderTree(tree)

  # output selected node
  output$doc <- renderUI({
    tree.node <- get_selected(input$tree)
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
