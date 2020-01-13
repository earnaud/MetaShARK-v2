taxcovUI <- function(id, title, dev) {
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
        navSidebar(
          ns("nav"),
          ... = tagList(
            if (dev) actionButton(ns("check"), "Dev Check")
          )
        )
      ) # end of column2
    ) # end of fluidPage
  ) # end of return
}

taxcov <- function(input, output, session, savevar, globals) {
  ns <- session

  if (globals) {
    observeEvent(input, {
      browser()
    })
  }

  # Navigation buttons ----
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar,
    savevar,
    savevar
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar,
    savevar,
    savevar
  )
  callModule(
    nextTab, "nav",
    globals, "taxcov"
  )
  callModule(
    prevTab, "nav",
    globals, "taxcov"
  )

  # Output ----
  return(savevar)
}