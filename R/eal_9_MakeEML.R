#' @import shiny
#'
#' @noRd
MakeEMLUI <- function(id, main.env) {
  return(
    fluidPage(
      fluidRow(
        style = "text-align: center;",
        tags$h2("We're almost there !"),
        tags$p("By clicking this button, you will process your metadata into
            a EML-valid xml file."),
        tags$p("(NOTE: you will be able to edit this data package furtherly)"),
        actionButton(
          NS(id, "make_eml"),
          "Make EML",
          icon("edit"),
          width = "50%"
        ),
        tags$br(),
        fluidRow(
          column(
            6,
            tags$div(
              id = "publish",
              tags$b("Publish data package"),
              "You can head to the Upload tab and publish 
                your data package to a metacat repository.",
              actionButton(
                NS(id, "publish"),
                "Publish",
                icon("file-export")
              )
            )
          ),
          column(
            6,
            tags$b("Generate a summary of your data package."),
            tags$i("(clicking on the below button will open a preview)"),
            downloadButton(
              NS(id, "download_emldown"),
              "Download emldown",
              width = "50%"
            )
          ) # End of emldown
        ),
        actionLink(
          NS(id, "bug_report"),
          span("Click here to report any bug.", icon("external-link-alt"))
        )
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom shinyjs hide toggleState
#' @importFrom EMLassemblyline template_arguments make_eml
#' @importFrom EML eml_validate
#' @importFrom emldown render_eml
#' @importFrom utils browseURL zip
#'
#' @noRd
MakeEML <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Variable initialization (deprecated)
    
    # Make eml ----
    observeEvent(input$make_eml, {
      shinyjs::hide("bug_report")
      req(input$make_eml)
      withProgress(
        {
          . <- main.env$save.variable
          fileName <- .$SelectDP$dp.title

          x <- try(
            EMLassemblyline::template_arguments(
              path = .$SelectDP$dp.metadata.path,
              data.path = .$SelectDP$dp.data.path,
              data.table = dir(.$SelectDP$dp.data.path)
            )
          )

          if (class(x) == "try-error") {
            out <- x
            out[1] <- paste("Upon templating arguments: ", x)
            incProgress(0.9)
          } else {
            incProgress(0.3)

            x$path <- .$SelectDP$dp.metadata.path
            x$data.path <- .$SelectDP$dp.data.path
            x$eml.path <- .$SelectDP$dp.eml.path
            x$dataset.title <- .$SelectDP$dp.title
            x$temporal.coverage <- .$Misc$temporal.coverage
            # IGNORED geographic.description
            # IGNORED geographic.coordinates
            x$maintenance.description <- "ongoing"
            x$data.table <- dir(x$data.path)
            x$data.table.name <- optional(.$DataFiles$table.name)
            x$data.table.description <- optional(.$DataFiles$description)
            # TODO data.table.quote.character
            x$data.table.url <- optional(.$DataFiles$url)
            # TODO other.entity
            # TODO other.entity.name
            # TODO other.entity.description
            # TODO other.entity.url
            # TODO provenance -- possible with DOIs ?
            x$user.id <- optional(
              if(main.env$SETTINGS$user != "public") 
                main.env$SETTINGS$user
            )
            # TODO user domain -- PNDB ? ORCID ?
            x$package.id = x$dataset.title # is set as UUID (default)
            x$write.file <- TRUE
            x$return.obj <- TRUE

            incProgress(0.2)

            # Remove potential file before re-writing
            file.remove(
              dir(
                main.env$save.variable$SelectDP$dp.eml.path,
                full.names = TRUE
              )
            )
            do.call(
              EMLassemblyline::make_eml,
              x[names(x) %in% names(formals(EMLassemblyline::make_eml))]
            )
            
            incProgress(0.4)
          }
        },
        message = "Writing EML ...",
        value = 0.1
      )
      
      showNotification("EML written.", type = "message")
      
      # emldown
      out.file <- paste0(
        isolate(main.env$save.variable$SelectDP$dp.path),
        "/emldown/emldown.html"
      )
      eml.file <- dir(
        main.env$save.variable$SelectDP$dp.eml.path,
        full.names = TRUE,
        pattern = main.env$save.variable$SelectDP$dp.title
      )
      dir.create(dirname(out.file), recursive = TRUE)
      old.wd <- getwd()
      setwd(dirname(out.file))
      emldown::render_eml(
        file = eml.file,
        open = TRUE,
        outfile = out.file,
        publish_mode = TRUE
      )
      setwd(old.wd)
      if (file.exists(out.file))
        showNotification("emldown generated", type = "message")
    },
    label = "EAL9: make eml"
    )

    # Bug report ----
    observeEvent(input$bug_report, {
      utils::browseURL("https://github.com/earnaud/MetaShARK-v2/issues/")
    },
    label = "EAL9: bug report"
    )

    # emldown ----
    output$download_emldown <- downloadHandler(
      filename = function() {
        paste(
          main.env$save.variable$SelectDP$dp.name,
          "_emldown.zip"
        )
      },
      content = function(file) {
        utils::zip(
          zipfile = file,
          files = dir(
            dirname(out.file),
            recursive = TRUE
          )
        )
      }
    )
    
    # Post-end ====
    observe({
      req(main.env$EAL$page == 9)
      invalidateLater(1000)
      
      validate(
        need(
          length(dir(main.env$save.variable$SelectDP$dp.eml.path)) > 0,
          "No EML file generated"
        )
      )
      
      # Allow to publish or not
      shinyjs::toggleState(
        "publish",
        EML::eml_validate(
          dir(
            main.env$save.variable$SelectDP$dp.eml.path,
            pattern = main.env$save.variable$SelectDP$dp.title,
            full.names = TRUE
          )
        )
      )
      
      # Allow to access emldown or not
      shinyjs::toggleState(
        "emldown",
        file.exists(
          paste0(
            isolate(main.env$save.variable$SelectDP$dp.path),
            "/emldown/emldown.html"
          )
        )
      )
      
    })
  })
}
