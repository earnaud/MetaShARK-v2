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
  
  # var initialization
  # docGuideline <- tree
  # 
  # moduleNames <- sub("^[0-9]+_(.*)$", "\\1", names(docGuideline))
  # # avoid 404
  # moduleNames <- moduleNames[moduleNames != "eml-unit Type Definitions"]
  
  # UI output
  tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "www/styles.css"
      )
    ),
    # fluidRow(
    #   box(
    #     width = 12,
    #     title = "Check original documentation",
    #     HTML("<p>This documentation is brought to you from XSD files downloaded 
    #     from <a href='https://eml.ecoinformatics.org/schema/'>this site</a>.
    #     You can visit the original documentation by chosing a module
    #     name and clicking the 'GO' button below.</p>"),
    #     column(6,
    #       selectInput(ns("select-module"), NULL,
    #         moduleNames,
    #         selected = moduleNames[25],
    #         multiple = FALSE
    #       )
    #     ),
    #     column(6,
    #       actionButton(ns("visit-module"), "Go !",
    #         icon = icon("external-link-alt")
    #       )
    #     )
    #   )
    # ),
    fluidRow(
      # search sidebar
      column(5,
        wellPanel(
          shinyTree(
            outputId = ns("tree"),
            search = TRUE,
            theme = "proton"
          )
        )
      ),
      # display main panel
      column(7,
        # box(width = 12,
        div(
          uiOutput(ns("doc")),
          style = "position: fixed"
        )
        # )
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
documentation <- function(input, output, session, globals) {
  ns <- session$ns
  
  # Load data ====
  withProgress(message = "Loading documentation.", {
    doc.file <- system.file("data-raw/infoBuilder-v2/doc_guideline.json", package = "MetaShARK")
    doc <- read_json(doc.file)[[1]] %>% unserializeJSON
    incProgress(0.9)
      tree.file <- system.file("data-raw/infoBuilder-v2/tree_guideline.json", package = "MetaShARK")
    tree <- read_json(tree.file)[[1]] %>% unserializeJSON
  })
  
  
  # External links ====
  # observeEvent(input$`visit-module`, {
  #   url <- paste0(
  #     "https://nceas.github.io/eml/schema/",
  #     input$`select-module`,
  #     "_xsd.html"
  #   )
  #   url <- sub(" +", "", url)
  #   browseURL(url)
  # })
  
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
    if("annotation" %in% names(doc.node))
      doc.node$annotation
    else
      helpText("No content found at:", path)
  })
}
