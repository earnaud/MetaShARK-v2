#' @title fillUI
#'
#' @description UI part of the fill module
#'
#' @param dev logical. Shall the dev items appear?
#'
#' @importFrom shiny NS tabsetPanel tabPanel tags
fillUI <- function(id, dev = FALSE) {
  ns <- NS(id)

  # h1("Under construction.")
  tabsetPanel(
    id = ns("tabs"),
    tabPanel("EMLAL", EMLALUI(ns("EMLAL"), dev)),
    tabPanel("MetaFIN", tags$h1("Under Construction"))
  )
}

#' @title fill
#'
#' @description server part of the fill module
#'
#' @param globals global list containing fixed setting values for the
#' app.
#'
#' @importFrom shiny observeEvent callModule
fill <- function(input, output, session,
                 globals) {
  ns <- session$ns

  # variable initialization ----
  # save variable
  savevar <- initReactive()

  # action
  observeEvent(globals$EMLAL$NAVIGATE, {
    savevar$emlal$step <- max(
      globals$EMLAL$NAVIGATE,
      savevar$emlal$step
    )
    if (savevar$emlal$step > globals$EMLAL$MAX) {
      savevar$emlal$step <- globals$EMLAL$MAX
    }
  })

  savevar <- callModule(
    EMLAL, "EMLAL",
    savevar, globals
  )

  return(savevar)
}
