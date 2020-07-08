#' @title Fill
#'
#' @description server part of the fill module. Root script for whole module (comprise EML AL and MetaFIN)
#'
#' @import shiny
fillUI <- function(id, dev = FALSE) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("tabs"),
    tabPanel("EAL", EMLALUI(ns("EAL"), dev)),
    tabPanel("MetaFIN", h1("Under Construction"))
  )
}

#' @describeIn fillUI
#'
#' @import shiny
fill <- function(input, output, session, main.env) {
  ns <- session$ns
  # variable initialization ====

  # save variable initialization
  savevar <- initReactive(main.env = main.env$EMLAL)

  # action ====
  savevar <- callModule(
    EMLAL, "EAL",
    savevar, main.env
  )

  # Output ====
  return(savevar)
}
