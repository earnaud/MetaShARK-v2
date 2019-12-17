#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @importFrom shiny NS fluidPage fluidRow column tagList tags actionButton
geocovUI <- function(id, title, dev) {
  ns <- NS(id)

  return(
    fluidPage(
      # Features UI ----
      column(
        10,
        fluidRow(
          title
        )
      ), # end of column1
      # Navigation UI ----
      column(
        2,
        navSidebar(ns("nav"),
          ... = tagList(
            if (dev) actionButton(ns("check"), "Dev Check")
          )
        )
      ) # end of column2
    ) # end of fluidPage
  ) # end of return
}

#' @title Geographic coverage
#'
#' @description server part for the Geographic Coverage module
#'
#' @importFrom shiny observeEvent callModule
geocov <- function(input, output, session, savevar, globals) {
  ns <- session$ns

  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }

  # Navigation buttons ----
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    nextTab, "nav",
    globals, "geocov"
  )
  callModule(
    prevTab, "nav",
    globals
  )

  # Process data ----
  
  observeEvent(input[["nav-prevTab"]], {
    if(tail(globals$EMLAL$HISTORY, 1) == "customUnits")
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-1
    else if(tail(globals$EMLAL$HISTORY, 1) == "template")
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-1
  })
  observeEvent(input[["nav-nextTab"]], {
    
  })
  
  # Output ----
  return(savevar)
}
