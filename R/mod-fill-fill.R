#' @title Fill
#' 
#' @description server part of the fill module. Root script for whole module (comprise EML AL and MetaFIN)
#' 
#' @importFrom shiny NS tabsetPanel tabPanel h1
fillUI <- function(id, dev = FALSE){
  ns <- NS(id)
  tabsetPanel(id = ns("tabs"),
    tabPanel("EMLAL", EMLALUI(ns("EMLAL"), dev)),
    tabPanel("MetaFIN", h1("Under Construction"))
  )

}

#' @describeIn fillUI
#' 
#' @importFrom shiny observeEvent callModule
fill <- function(input, output, session, globals){
  ns <- session$ns
  # variable initialization ----

  # save variable
  savevar <- initReactive()

  # action
  observeEvent(globals$EMLAL$NAVIGATE,{
    savevar$emlal$step <- max(
      globals$EMLAL$NAVIGATE,
      savevar$emlal$step
    )
    if(savevar$emlal$step > globals$EMLAL$MAX)
      savevar$emlal$step <- globals$EMLAL$MAX
  })

  savevar <- callModule(EMLAL,"EMLAL",
                        savevar, globals)

  return(savevar) # useful? yes for side browser() to be able to reach savevar
}
