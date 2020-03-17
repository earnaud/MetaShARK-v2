MakeEMLUI <- function(id, title, dev) {
  ns <- NS(id)
  
  return(
    fluidPage(
      # Features UI -----------------------------------------------------
      column(
        10,
        fluidRow(
          style = "text-align: center;",
          h2("We're almost there !"),
          p("By clicking this button, you will process your metadata into
            a EML-valid xml file."),
          tags$p("(NOTE: you will be able to edit this data package furtherly)"),
          actionButton(
            ns("make_eml"), 
            "Make EML", 
            icon("edit"), 
            width = "50%"
          ),
          textOutput(ns("warnings")),
          tags$br(),
          hidden(
            actionButton(
              ns("publish"), 
              "Publish", 
              icon("share-alt"), 
              width = "50%"
            )
          )
        )
      ), # end of column1
      # Navigation UI -----------------------------------------------------
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
#' @importFrom shiny showNotification renderText validate need req isTruthy observeEvent
#' @importFrom shinyjs show
#' @importFrom EMLassemblyline make_eml template_arguments
MakeEML <- function(input, output, session, savevar, globals) {
  ns <- session$ns
  
  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }
  
  # NSB -----------------------------------------------------
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar
  )
  # callModule(
  #   nextTab, "nav",
  #   globals, "MakeEML"
  # )
  callModule(
    prevTab, "nav",
    globals
  )
  
  # Make eml -----------------------------------------------------
  observeEvent(input$make_eml, {
    req(input$make_eml)
    
    . <- savevar$emlal
    
    x <- template_arguments(
      path = .$SelectDP$dp_metadata_path,
      data.path = .$SelectDP$dp_data_path,
      data.table = dir(.$SelectDP$dp_data_path)
    )
    
    x$path <- .$SelectDP$dp_metadata_path
    x$data.path <- .$SelectDP$dp_data_path
    x$eml.path <- .$SelectDP$dp_eml_path
    x$data.table <- dir(x$data.path)
    x$data.table.name <- .$DataFiles$table_name
    x$data.table.description <- .$DataFiles$description
    x$dataset.title <- .$SelectDP$dp_title
    x$maintenance.description <- "Ongoing"
    # TODO package.id
    x$package.id <- "localid"
    x$return.obj <- TRUE
    x$temporal.coverage <- .$Misc$temporal_coverage
    # TODO user domain (pndb?)
    x$user.domain <- "UserDomain"
    # TODO user id (orcid?)
    x$user.id <- "UserID"
    x$write.file <- TRUE
    
    # Yet written in the files then used in make_eml
    x$geographic.coordinates <- NULL
    x$geographic.description <- NULL
    
    out <- try(
      do.call(
        make_eml,
        x[names(x) %in% names(formals(make_eml))]
      )
    )
    
    output$warnings <- renderText({
      validate(
        need(class(out) != "try-error", out[1])
      )
      showNotification("EML written !", type = "message")
      return(NULL)
    })
    
    if(class(out) != "try-error")
      show("publish", anim = TRUE, time = 0.25)
    
  })
  
  # Publish -----------------------------------------------------
  if(isTruthy(dir(savevar$emlal$SelectDP$dp_eml_path)))
    show("publish", anim = TRUE, time = 0.25)
  
  observeEvent(input$publish, {
    req(input$publish)
    
    # updateTabItems(session, "side_menu", "upload")
    globals$EMLAL$NAVIGATE <- 1
  })
  
  # Output -----------------------------------------------------
  return(savevar)
}
