#' @title docUI
#'
#' @description UI part of the documentation module
#'
#' @importFrom shiny NS tagList tags fluidRow column selectInput div
#' @importFrom shinydashboard box
#' @importFrom shinyTree shinyTree
docUI <- function(id) {
  ns <- NS(id)

  # var initialization
  docGuideline <- tree
  browser()
  moduleNames <- sub("^[0-9]+_(.*)$", "\\1", names(docGuideline))
  # avoid 404
  moduleNames <- moduleNames[moduleNames != "eml-unit Type Definitions"]

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
      box(
        width = 12,
        title = "Check original documentation",
        "This documentation is brought to you from XSD files downloaded from
          <a href=''>this git</a>. You can visit the original documentation by 
          chosing a module name and clicking the 'GO' button below:",
        column(
          6,
          selectInput(ns("select-module"), NULL,
            moduleNames,
            selected = moduleNames[25],
            multiple = FALSE
          )
        ),
        column(
          6,
          actionButton(ns("visit-module"), "Go !",
            icon = icon("external-link-alt")
          )
        )
      )
    ),
    fluidRow(
      # search sidebar
      column(5,
        box(shinyTree(
          outputId = ns("tree"), # render tree
          search = TRUE,
          theme = "proton"
        ),
        width = 12
        ),
        id = "docSidePanel"
      ),
      # display main panel
      column(
        7,
        div(box(uiOutput(ns("docPath")), # XPath
          uiOutput(ns("docSelect")), # Documentation
          width = 12
        ),
        id = "docMainPanel"
        )
      )
    )
  )
}

#' @title documentation
#'
#' @description server part of the documentation module.
#'
#' @importFrom shiny observeEvent renderText
#' @importFrom shinyTree renderTree get_selected
#' @importFrom utils browseURL
documentation <- function(input, output, session, globals) {
  ns <- session$ns

  require(shinyTree)

  # external links
  observeEvent(input$`visit-module`, {
    url <- paste0(
      "https://nceas.github.io/eml/schema/",
      input$`select-module`,
      "_xsd.html"
    )
    url <- sub(" +", "", url)
    browseURL(url)
  })

  # render tree
  output$tree <- renderTree(tree)

  # output selected node
  output$docSelect <- renderText({
    jstree <- input$tree
    if (is.null(jstree)) {
      "None"
    } else {
      node <- get_selected(tree = jstree)
      if (length(node) == 0) {
        return("(Select a node first)")
      }
      docPath <- gsub(
        "^/", "",
        paste(
          paste(attr(node[[1]], "ancestry"), collapse = "/"),
          unlist(node),
          sep = "/"
        )
      )
      output$docPath <- renderText(as.character(tags$h4(docPath)))

      # fetch the systemGuideLine path in the userGuideLine list
      systemPath <- followPath(tree, docPath)

      if (!is.character(systemPath)) {
        systemPath <- commonPath(systemPath, unlist(node))
      }

      # fetch the eml-xsd content in the systemGuideLine list
      systemContent <- followPath(systemGuideline, systemPath)
      out <- extractContent(systemContent, nsIndex = readRDS(globals$PATHS$nsIndex.Rdata))
      return(out)
    }
  })
}
