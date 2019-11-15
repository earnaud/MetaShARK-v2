# fill.R

### UI ###
fillUI <- function(id) {
  ns <- NS(id)
  # submodules sourcing
  source("R/modules/fill/EMLAL/EMLAL.R")

  # h1("Under construction.")
  tabsetPanel(
    id = ns("tabs"),
    tabPanel("EMLAL", EMLALUI(ns("EMLAL"))),
    tabPanel("MetaFIN", h1("Under Construction"))
  )
}



### SERVER ###
fill <- function(input, output, session, globals) {
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

  return(savevar) # useful? yes for side browser() to be able to reach savevar
}
