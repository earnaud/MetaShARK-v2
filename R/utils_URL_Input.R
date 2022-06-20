#' URL Input
#'
#' URL Shiny input module. Implements elementary test to get a valid URL.
#'
#' @param id (character) shiny module inputId.
#' @param label (character) display label for the control, or NULL for no label.
#' @param width	(character) the width of the input, e.g. '400px', or '100%'; see
#' validateCssUnit().
#'
#' @return
#' (output of calling the module part)
#' If input is a valid URL (regex-tested + curl-tested), returns input.
#' Else, returns NA.
#'
#' @import shiny
urlInput_UI <- function(id, label = "URL", width = "100%") {
  ns <- NS(id)

  tagList(
    textInput(ns("url"), label,
      placeholder = "https://github.com/earnaud/MetaShARK-v2"
    ),
    textOutput(ns("warnings"))
  )
}

#' @title urlInput
#'
#' @describeIn urlInput_UI
#'
#' @import shiny
#' @importFrom RCurl url.exists
urlInput <- function(id) {
  moduleServer(id, function(input, output, session) {
    # variable initialization
    url <- reactiveVal(character())

    # actions
    observeEvent(input$url, {
      is_url <- RCurl::url.exists(input$url)

      output$warnings <- renderText({
        validate(
          need(is_url, "Invalid URL target.")
        )
        return(NULL)
      })

      url <- NA_character_
      req(is_url)
      url <- input$url
    })

    return(url)
  })
}
