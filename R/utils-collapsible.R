#' @title collapsibleUI
#' 
#' @description A module to get a div collapsed by clicking on a link.
#' 
#' @param label A label appearing on the clickable link.
#' 
#' @importFrom shiny NS tagList actionLink icon div
#' @importFrom shinyjs useShinyjs hidden 
collapsibleUI <- function(id, label, ...) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    actionLink(
      ns("link"), 
      label, 
      icon = icon("chevron-right")
    ),
    hidden(
      div(
        id = ns("area"),
        ...,
        style = "margin-left: 20px"
      )
    )
  )
}

#' @title collapsible
#' 
#' @describeIn collapsibleUI
#' 
#' @importFrom shiny observeEvent updateActionButton icon
#' @importFrom shinyjs toggle
collapsible <- function(input, output, session) {
  observeEvent(input$link, {
    toggle(
      id = "area" ,
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
