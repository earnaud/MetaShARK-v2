MakeEMLUI <- function(id, title, dev) {
  ns <- NS(id)
  
  return(
    fluidPage(
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
        fluidRow(
          column(
            6,
            tags$div(
              id = "publish",
              tags$b("Publish data package"),
              "You can head to the Upload tab and publish 
                your data package to a metacat repository."
            )
          ),
          column(
            6,
            tags$b("Generate a summary of your data package."),
            tags$i("(clicking on the below button will open a preview)"),
            # radioButtons(
            #   inputId = ns("format"),
            #   label = "Chose your output format",
            #   choices = c("HTML", "PDF"),
            #   selected = "HTML",
            #   inline = TRUE,
            #   width = "50%"
            # ),
            disabled(
              actionButton(
                ns("emldown"),
                "Write emldown",
                icon("file-code"),
                width = "50%"
              )
            ),
            disabled(
              downloadButton(
                ns("download_emldown"),
                "Download emldown",
                width = "50%"
              )
            )
          ) # End of emldown
        )
      )
    ) # end of fluidPage
  ) # end of return
}

#' @title MakeEML
#'
#' @description server part of the make EML module
#'
#' @importFrom shiny showNotification renderText validate need req isTruthy observeEvent
#' @importFrom shinyjs show enable onclick
#' @importFrom EMLassemblyline make_eml template_arguments
#' @importFrom emldown render_eml
MakeEML <- function(input, output, session, savevar,
  globals) {
  ns <- session$ns
  
  if(globals$dev)
    onclick("dev", {
      req(globals$EMLAL$NAVIGATE == 9)
      browser()
    }, asis=TRUE)
  
  # Variable initialization -----------------------------------------------------
  outFile <- paste0(
    savevar$emlal$SelectDP$dp_path,
    "/emldown/emldown.html"
  )
  
  # Make eml -----------------------------------------------------
  onclick("make_eml", {
    req(input$make_eml)
    withProgress({
      . <- savevar$emlal
      fileName <- "localid"
      
      x <- try(
        template_arguments(
          path = .$SelectDP$dp_metadata_path,
          data.path = .$SelectDP$dp_data_path,
          data.table = dir(.$SelectDP$dp_data_path)
        )
      )
      
      if(class(x) == "try-error") {
        out <- x
        incProgress(0.9)
      } else {
        incProgress(0.3)
        
        x$path <- .$SelectDP$dp_metadata_path
        x$data.path <- .$SelectDP$dp_data_path
        x$eml.path <- .$SelectDP$dp_eml_path
        x$data.table <- dir(x$data.path)
        x$data.table.name <- .$DataFiles$table_name
        x$data.table.description <- .$DataFiles$description
        x$dataset.title <- .$SelectDP$dp_title
        x$maintenance.description <- "Ongoing"
        # TODO better package.id
        x$package.id <- fileName
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
        
        incProgress(0.2)
        
        test <- 0
        out <-try(
          do.call(
            make_eml,
            x[names(x) %in% names(formals(make_eml))]
          )
        )
        
        incProgress(0.4)
      }
    },
      message = "Writing EML ...",
      value = 0.1
    )
    
    browser()
    
    output$warnings <- renderText({
      validate(
        need(
          class(out) != "try-error",
          out[1]
        )
      )
      showNotification("EML written !", type = "message")
      return(NULL)
    })
    
    if (class(out) != "try-error") {
      enable("publish")
      enable("emldown")
    } else {
      disable("publish")
      disable("emldown")
    }
  })
  
  # emldown -----------------------------------------------------
  onclick("emldown", {
    disable("emldown")
    
    emlFile <- dir(
      savevar$emlal$SelectDP$dp_eml_path,
      full.names = TRUE,
      pattern = "localid"
    )
    
    enable("emldown")
    req(file.exists(emlFile) && 
        isTruthy(emlFile))
    disable("emldown")
    
    dir.create(dirname(outFile), recursive = TRUE)
    
    old.wd <- getwd()
    setwd(dirname(outFile))
    out <- render_eml(
      file = emlFile,
      open = TRUE,
      outfile = outFile,
      publish_mode = TRUE
    )
    setwd(old.wd)
    
    # if(input$format != "HTML"){
    #   rmarkdown::pandoc_convert(
    #     input = outFile,
    #     to = tolower(input$format),
    #     from = "html",
    #     output = gsub("html$","pdf", outFile)
    #   )
    # }
    
    if (file.exists(outFile)) {
      enable("download_emldown")
      showNotification("emldown generated", type = "message")
    }
    
    enable("emldown")
  })
  
  output$download_emldown <- downloadHandler(
    filename = function() {
      paste(
        savevar$emlal$SelectDP$dp_name,
        "_emldown.zip"
      )
    },
    content = function(file) {
      zip(
        zipfile = file,
        files = dir(
          dirname(outFile),
          recursive = TRUE
        )
      )
    })
  
  # Output -----------------------------------------------------
  return(savevar)
}
