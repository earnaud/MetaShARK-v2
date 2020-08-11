#' @title URL_Input_UI
#'
#' @description URL Shiny input module.
#'
#' @param id (character) shiny module inputId.
#' @param label (character) display label for the control, or NULL for no label.
#' @param width	(character) the width of the input, e.g. '400px', or '100%'; see
#' validateCssUnit().
#'
#' @return (output of `callModule`) If input is a valid URL (regex-tested + curl-tested), returns input.
#' Else, returns NA.
#'
#' @import shiny
URL_Input_UI <- function(id, label = "URL", width = "100%") {
  ns <- NS(id)

  tagList(
    textInput(NS(id, "url"), label, placeholder = "https://github.com/earnaud/MetaShARK-v2"),
    textOutput(NS(id, "warnings"))
  )
}

#' @title URL_Input
#'
#' @describeIn URL_Input_UI
#'
#' @import shiny
#' @importFrom RCurl url.exists
URL_Input <- function(id) {
  moduleServer(id, function(input, output, session) {
    # variable initialization
    url <- reactiveVal(character())

    # actions
    observeEvent(input$url, {
      is.url <- RCurl::url.exists(input$url)

      output$warnings <- renderText({
        validate(
          need(is.url, "Invalid URL target.")
        )
        return(NULL)
      })

      url <- NA_character_
      req(is.url)
      url <- input$url
    })

    return(url)
  })
}
