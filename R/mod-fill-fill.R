#' @title Fill
#'
#' @description server part of the fill module. Root script for whole module (comprise EML AL and MetaFIN)
#'
#' @importFrom shiny NS tabsetPanel tabPanel h1
fillUI <- function(id, dev = FALSE) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("tabs"),
    tabPanel("EMLAL", EMLALUI(ns("EMLAL"), dev)),
    tabPanel("MetaFIN", h1("Under Construction"))
  )
}

#' @describeIn fillUI
#'
#' @importFrom shiny observeEvent callModule
fill <- function(input, output, session, globals, server) {
  ns <- session$ns
  # variable initialization ----

  # save variable
  savevar <- initReactive()
  
  # action ----
  observeEvent(globals$EMLAL$NAVIGATE, {
    # resume where it was saved
    savevar$emlal$step <- globals$EMLAL$NAVIGATE
  })
  observeEvent(globals$EMLAL$HISTORY, {
    # resume where it was saved
    savevar$emlal$history <- globals$EMLAL$HISTORY
  })

  savevar <- callModule(
    EMLAL, "EMLAL",
    savevar, globals, server
  )

  # Output ----
  return(savevar)
}
