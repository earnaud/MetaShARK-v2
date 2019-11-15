## 0. Sample template
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
    globals, globals$EMLAL$PREVIOUS[1]
  )
  callModule(
    prevTab, "nav",
    globals, globals$EMLAL$PREVIOUS[1]
  )

  # Process data ----
  observeEvent(input[["nav-nextTab"]],{
    globals$EMLAL$PREVIOUS <- "geocov"
  })
  
  observeEvent(input[["nav-prevTab"]],{
    globals$EMLAL$PREVIOUS[1] <- "geocov"
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-globals$EMLAL$PREVIOUS[3]
  })
  
  # Output ----
  return(savevar)
}
