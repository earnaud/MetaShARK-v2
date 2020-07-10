#' @import shiny
#' 
#' @noRd
fillUI <- function(id, dev = FALSE) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("tabs"),
    tabPanel("EAL", EMLALUI(ns("EAL"), dev)),
    tabPanel("MetaFIN", h1("Under Construction"))
  )
}

#' @import shiny
#' 
#' @noRd
fill <- function(input, output, session, main.env) {
  ns <- session$ns
  # variable initialization ====

  # save variable initialization
  save.variable <- initReactive(main.env = main.env$EAL)

  # action ====
  save.variable <- callModule(
    EMLAL, "EAL",
    save.variable, main.env
  )

  # Output ====
  return(save.variable)
}
