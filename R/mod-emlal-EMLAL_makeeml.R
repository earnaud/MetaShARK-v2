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
          .next = FALSE,
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
  
  # Make eml ----
  observeEvent(input$make_eml, {
    . <- savevar$emlal
    
    try(
      make_eml(
        path = paste(
          .$SelectDP$dp_path,
          .$SelectDP$dp_name,
          "metadata_templates",
          sep = "/"
        ),
        data.path = paste(
          .$SelectDP$dp_path,
          .$SelectDP$dp_name,
          "data_objects",
          sep = "/"
        ),
        eml.path = paste(
          .$SelectDP$dp_path,
          .$SelectDP$dp_name,
          "eml",
          sep = "/"
        ),
        dataset.title = .$SelectDP$dp_title,
        temporal.coverage = .$Misc$temporal_coverage,
        maintenance.description = "ongoing",
        # geographic.description = checkTruth(.$GeoCov$geographicDescription),
        # geographic.coordinates = checkTruth(.$GeoCov[,c("northBoundingCoordinate","southBoundingCoordinate","eastBoundingCoordinate","westBoundingCoordinate")]),
        data.table = .$DataFiles$dp_data_files$name,
        data.table.name = .$DataFiles$dp_data_files$name,
        # TODO add description inputs
        data.table.description = .$DataFiles$dp_data_files$name,
        # TODO in data selection step, add data url
        # data.url = NULL,
        # TODO in data selection step, add entity differenciation
        # other.entity = NULL,
        # other.entity.description = NULL,
        # TODO add EDI data repository ID
        # provenance = NULL,
        # TODO add user.id from options
        user.id = 'Test',
        user.domain = 'EDI'
        # TODO check how to get a lsid?
        # ,package.id = NULL
      )
    )
    
    browser()
    
  })
  # Output ----
  return(savevar)
}