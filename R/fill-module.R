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
fill <- function(input, output, session, globals) {
  ns <- session$ns
  # variable initialization ----------------------------------------------------

  # save variable
  savevar <- initReactive(glob = globals$EMLAL)

  # action -----------------------------------------------------
  savevar <- callModule(
    EMLAL, "EMLAL",
    savevar, globals
  )

  # Output -----------------------------------------------------
  return(savevar)
}
