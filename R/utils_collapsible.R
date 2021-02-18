#' @title collapsibleUI
#'
#' @description A shiny module to get a div collapsed by clicking on a link.
#'
#' @param id character.  An ID string that corresponds with the ID used to call 
#' the module's UI function.
#' @param label character. A label appearing on the clickable link.
#' @param .hidden logical. A flag to make the UI display as collapsed or not.
#' @param ... shiny UI elements. Any UI element displayed as core content.
#' @param class character. CSS class to apply to ... .
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs hidden
#' 
#' @export
collapsibleUI <- function(id, label, .hidden = TRUE, ..., class = NULL) {
  content <- tags$div(
    id = NS(id, "area"),
    tagList(...),
    class = class
  )

  tagList(
    shinyjs::useShinyjs(),
    actionLink(
      NS(id, "link"),
      label,
      icon = if (isTRUE(.hidden)) icon("chevron-right") else icon("chevron-down")
    ),
    if (isTRUE(.hidden)) {
      shinyjs::hidden(
        content
      )
    } else {
      content
    }
  )
}

#' @describeIn collapsibleUI
#' 
#' Server part for collapsible widget module.
#' 
#' @param id character. An ID string that corresponds with the ID used to call 
#' the module's UI function.
#' 
#' @import shiny
#' @importFrom shinyjs toggle
#'
#' @export
collapsible <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$link, {
      shinyjs::toggle(
        id = "area",
        anim = TRUE,
        animType = "slide",
        time = 0.25
      )

      if (input$link %% 2 == 1) {
        .tmp <- "chevron-down"
      } else {
        .tmp <- "chevron-right"
      }

      updateActionButton(session, "link", icon = icon(.tmp))
    })
  })
}
