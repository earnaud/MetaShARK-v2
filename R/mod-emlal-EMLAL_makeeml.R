MakeEMLUI <- function(id, title, dev) {
  ns <- NS(id)

  return(
    fluidPage(
      # Features UI ----
      column(
        10,
        fluidRow(
          style = "text-align: center;",
          h2("We're almost there !"),
          p("By clicking this button, you will process your metadata into
            a EML-valid xml file."),
          actionButton(ns("make_eml"), "Make EML"),
          tags$p("(NOTE: you will be able to edit this data package furtherly)"),
          hidden(
            div(
              id = ns("process"),
              "RUNNING"
            )
          )
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

#' @title MakeEML
#' 
#' @description server part of the make EML module
#' 
#' @importFrom EMLassemblyline make_eml
MakeEML <- function(input, output, session, savevar, globals) {
  ns <- session$ns

  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }

  # Make eml ----
  observeEvent(input$make_eml, {
    . <- savevar$emlal
    
    browser()
    stop("Not further")
    
    make_eml(
      path = .$SelectDP$dp_path,
      data.path = unique(dirname(.$DataFiles$dp_data_files$datapath)),
      eml.path = .$SelectDP$dp_path, 
      dataset.title = .$SelectDP$dp_title,
      temporal.coverage,
      maintenance.description = "ongoing",
      # TODO in data selection step, add entity differenciation
      data.table = basename(.$DataFiles$dp_data_files$datapath), 
      # other.entity = NULL,
      # other.entity.description = NULL,
      # TODO in data selection step, add data url
      # data.url = NULL,
      # TODO add EDI data repository ID
      # provenance = NULL,
      # TODO add user.id from options
      user.id = 'Test',
      user.domain = 'EDI',
      # TODO check how to get a lsid?
      package.id = NULL
    )
  })
  
  # Navigation buttons ----
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar,
    savevar$emlal$SelectDP$dp_path,
    savevar$emlal$SelectDP$dp_name
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar,
    savevar$emlal$SelectDP$dp_path,
    savevar$emlal$SelectDP$dp_name
  )
  callModule(
    nextTab, "nav",
    globals, "MakeEML"
  )
  callModule(
    prevTab, "nav",
    globals
  )

  # Output ----
  return(savevar)
}