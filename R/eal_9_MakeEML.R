#' @import shiny
#'
#' @noRd
MakeEMLUI <- function(id) {
  return(
    fluidPage(
      fluidRow(
        style = "text-align: center;",
        tags$h2("We're almost there !"),
        tags$p("By clicking this button, you will process your metadata into
            a EML-valid xml file. Please note that you will be able to edit this
               data package furtherly."),
        tags$p(
          "In case of error, please ",
          actionLink(
            NS(id, "bug_report"),
            span("click here to report any bug.", icon("external-link-alt"))
          ),
          "(github login required)"
        ),
        actionButton(
          NS(id, "make_eml"),
          tags$h3(
            HTML("<i class='fa fa-edit' role='presentation' aria-label='edit icon'></i>"),
            "Make EML"
          ),
          # icon("edit"),
          width = "50%"
        ),
        tags$br(),
        fluidRow(
          column(
            6, offset = 3,
            tags$div(
              id = "publish",
              tags$h4("Publish data package"),
              "You can head to the Upload tab and publish 
                your data package to a metacat repository.",
              shinyjs::disabled(actionButton(
                NS(id, "publish"),
                "Publish",
                icon("file-export")
              ))
            )
          )
          # , column(
          #   6,
          #   tags$h4("Generate a summary"),
          #   tags$i("(clicking on the below button will open a preview)"),
          #   tags$br(),
          #   shinyjs::disabled(downloadButton(
          #     NS(id, "download_emldown"),
          #     "Download emldown",
          #     width = "50%"
          #   ))
          # ) # End of emldown
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
    if (main.env$dev){
      observeEvent(
        main.env$dev.browse(), 
        {
          if (main.env$current.tab() == "fill" &&
              main.env$EAL$page == 9) {
            browser()
          }
        }
      )
    }
    
    # Make eml ----
    observeEvent(input$make_eml, {
      # shinyjs::hide("bug_report")
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
            # FROM FILES geographic.description
            # FROM FILES geographic.coordinates
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
      main.env$local.rv$eml.written <- TRUE
      
      showNotification("EML written.", type = "message")
      
      # # emldown
      # out.file <- paste0(
      #   isolate(main.env$save.variable$SelectDP$dp.path),
      #   "/emldown/emldown.html"
      # )
      # eml.file <- dir(
      #   main.env$save.variable$SelectDP$dp.eml.path,
      #   full.names = TRUE,
      #   pattern = main.env$save.variable$SelectDP$dp.title
      # )
      # dir.create(dirname(out.file), recursive = TRUE)
      # old.wd <- getwd()
      # setwd(dirname(out.file))
      # emldown::render_eml(
      #   file = eml.file,
      #   open = TRUE,
      #   outfile = out.file,
      #   publish_mode = TRUE
      # )
      # setwd(old.wd)
      # if (file.exists(out.file))
      #   showNotification("emldown generated", type = "message")
    },
    label = "EAL9: make eml"
    )

    # already written ----
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page == 9)
      req(isTRUE(main.env$local.rv$eml.written))
      
      shinyjs::enable("publish")
      # shinyjs::enable("download_emldown")
    })
    
    # Bug report ----
    observeEvent(input$bug_report, {
      utils::browseURL("https://github.com/earnaud/MetaShARK-v2/issues/")
    },
    label = "EAL9: bug report"
    )

    # # emldown ----
    # output$download_emldown <- downloadHandler(
    #   filename = function() {
    #     paste0(
    #       main.env$save.variable$SelectDP$dp.name,
    #       "_emldown.zip"
    #     )
    #   },
    #   content = function(file) {
    #     old.wd <- getwd()
    #     setwd(paste0(main.env$save.variable$SelectDP$dp.path,"/emldown"))
    #     
    #     zip::zip(
    #       zipfile = file,
    #       files = dir(
    #         paste0(
    #           main.env$save.variable$SelectDP$dp.path,
    #           "/emldown"
    #         ),
    #         # full.names = TRUE,
    #         recursive = TRUE
    #       )
    #     )
    #     
    #     setwd(old.wd)
    #   }
    # )
    # 
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
      valid <- EML::eml_validate(
        dir(
          main.env$save.variable$SelectDP$dp.eml.path,
          pattern = main.env$save.variable$SelectDP$dp.title,
          full.names = TRUE
        )
      )
      shinyjs::toggleState("publish", valid)
      
      # # Allow to access emldown or not
      # shinyjs::toggleState(
      #   "download_emldown",
      #   valid && 
      #     file.exists(
      #       paste0(
      #         isolate(main.env$save.variable$SelectDP$dp.path),
      #         "/emldown/emldown.html"
      #       )
      #     )
      # )
      
    })
  })
}
