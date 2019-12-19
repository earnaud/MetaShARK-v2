#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @importFrom shiny NS fluidPage fluidRow column tagList tags actionButton
geocovUI <- function(id, title, dev) {
  ns <- NS(id)

  # browser()
  
  return(
    fluidPage(
      # Features UI ----
      column(
        10,
        fluidRow(
          tags$h4(title),
          # tags$p("help"),
          bsCollapse(
            id = ns("method"),
            bsCollapsePanel(
              title = "Use dataset's geographic variables",
              value = 1,
              tagList(
                selectInput("")
              )
            ),
            bsCollapsePanel(
              title = "Fill geographic template",
              value = 2
            )
          )
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
#' @importFrom shinyBS updateCollapse
geocov <- function(input, output, session, savevar, globals) {
  ns <- session$ns

  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }

  # Navigation buttons ----
  
  # NSB
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

  # Method ----
  observeEvent(input$method, {
    
  })
  
  # Process data ----
  
  observeEvent(input[["nav-prevTab"]], {
    if(tail(globals$EMLAL$HISTORY, 1) == "customUnits")
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-1
    else if(tail(globals$EMLAL$HISTORY, 1) == "template")
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-2
  })
  observeEvent(input[["nav-nextTab"]], {
    
  })
  
  # Output ----
  return(savevar)
}
