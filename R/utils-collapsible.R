#' @title collapsibleUI
#'
#' @description A shiny module to get a div collapsed by clicking on a link.
#'
#' @param label character. A label appearing on the clickable link.
#' @param ... shiny UI elements. Any UI element displayed as core content.
#' @param class character. CSS class to apply to ... .
#'
#' @importFrom shiny NS tagList actionLink icon div
#' @importFrom shinyjs useShinyjs hidden
collapsibleUI <- function(id, label, hidden = TRUE, ..., class = NULL) {
  ns <- NS(id)

  content <- div(id = ns("area"), ..., class = class)

  tagList(
    useShinyjs(),
    actionLink(
      ns("link"),
      label,
      icon = if (hidden) icon("chevron-right") else NULL
    ),
    if (hidden) {
      hidden(
        content
      )
    } else {
      content
    }
  )
}

#' @describeIn collapsibleUI
#'
#' @importFrom shiny observeEvent updateActionButton icon
#' @importFrom shinyjs toggle
collapsible <- function(input, output, session) {
  observeEvent(input$link, {
    toggle(
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
}
