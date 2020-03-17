#' @title URL_Input_UI
#'
#' @description URL Shiny input module.
#'
#' @return (output of `callModule`) If input is a valid URL (regex-tested + curl-tested), returns input.
#' Else, returns NA.
#'
#' @importFrom shiny textInput textOutput tagList NS
URL_Input_UI <- function(id, label = "URL", width = "100%") {
  ns <- NS(id)

  tagList(
    textInput(ns("url"), label, placeholder = "https://zenodo.org/record/3712913#.XnDcb3JCfcc"),
    textOutput(ns("warnings"))
  )
}

#' @title URL_Input
#'
#' @describeIn URL_Input_UI
#'
#' @importFrom shiny observeEvent reactiveVal renderText validate need req
#' @importFrom RCurl url.exists
URL_Input <- function(input, output, session) {
  # variable initialization
  url <- reactiveVal(character())

  # actions
  observeEvent(input$url, {
    is.url <- url.exists(input$url)

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
}
