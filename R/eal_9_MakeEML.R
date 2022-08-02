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
            HTML("<i class='fa fa-edit' role='presentation' aria-label='edit
                 icon'></i>"),
            "Make EML"
          ),
          # icon("edit"),
          width = "50%"
        ),
        tags$br(),
        fluidRow(
          column(
            4,
            offset = 1,
            tags$div(
              id = "publish",
              tags$h4("Publish data package"),
              "You can head to the Upload tab and publish
                your data package to a metacat repository."
            ),
            shinyjs::disabled(
              actionButton(
                NS(id, "publish"),
                "Publish",
                icon("file-export")
              )
            )
          ),
          column(
            4,
            offset = 2,
            tags$div(
              tags$h4("Download your data package"),
              "Get a local version containing MetaShARK files, EML Assembly Line
              templates and the written metadata at xml format. Compressed in a
              zip archive."
            ),
            shinyjs::disabled(
              downloadButton(
                NS(id, "download_data_package"),
                "Download Data Package",
                width = "50%"
              )
            )
          ) # End of DP download
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
# @importFrom emldown render_eml
#' @importFrom utils browseURL zip
#'
#' @noRd
MakeEML <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    if (main_env$dev) .browse_dev(main_env, 9, input, output, session)

    # Make eml ----
    observeEvent(input$make_eml, {
        req(input$make_eml)

        withProgress({
          .x <- main_env$save_variable

          x <- try(
            EMLassemblyline::template_arguments(
              path = .x$SelectDP$dp_metadata_path,
              data.path = .x$SelectDP$dp_data_path,
              data.table = dir(.x$SelectDP$dp_data_path)
            )
          )

          if (class(x) == "try-error") {
            out <- x
            out[1] <- paste("Upon templating arguments: ", x)
            incProgress(0.9)
          } else {
            incProgress(0.3)

            x$path <- .x$SelectDP$dp_metadata_path
            x$data.path <- .x$SelectDP$dp_data_path
            x$eml.path <- .x$SelectDP$dp_eml_path
            x$dataset.title <- .x$SelectDP$dp_title
            x$temporal.coverage <- .x$Misc$temporal_coverage
            # FROM FILES geographic.description
            # FROM FILES geographic.coordinates
            x$maintenance.description <- "ongoing"
            # x$data.table <- dir(x$data.path)
            x$data.table.name <- optional(.x$DataFiles$table_name)
            x$data.table.description <- optional(.x$DataFiles$description)
            # TODO data.table.quote.character
            x$data.table.url <- optional(.x$DataFiles$url)
            # TODO other.entity
            # TODO other.entity.name
            # TODO other.entity.description
            # TODO other.entity.url
            # TODO provenance -- possible with DOIs ?
            x$user.id <- optional(
              if (main_env$SETTINGS$user != "public")
                main_env$SETTINGS$user
            )
            # TODO user domain -- PNDB ? ORCID ?
            x$package.id <- x$dataset_title # is set as UUID (default)
            x$write.file <- TRUE
            x$return.obj <- TRUE

            incProgress(0.2)

            # Remove potential file before re-writing
            file.remove(
              dir(
                main_env$save_variable$SelectDP$dp_eml_path,
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
        main_env$local_rv$eml_written <- TRUE

        showNotification("EML written.", type = "message")

        # # emldown
        # out.file <- paste0(
        #   isolate(main_env$save_variable$SelectDP$dp.path),
        #   "/emldown/emldown.html"
        # )
        # eml.file <- dir(
        #   main_env$save_variable$SelectDP$dp_eml_path,
        #   full.names = TRUE,
        #   pattern = main_env$save_variable$SelectDP$dp_title
        # )
        # dir.create(dirname(out.file), recursive = TRUE)
        # .old_wd <- getwd()
        # setwd(dirname(out.file))
        # emldown::render_eml(
        #   file = eml.file,
        #   open = TRUE,
        #   outfile = out.file,
        #   publish_mode = TRUE
        # )
        # setwd(.old_wd)
        # if (file.exists(out.file))
        #   showNotification("emldown generated", type = "message")
      },
      label = "EAL9: make eml"
    )

    # already written ----
    observeEvent(main_env$EAL$page, {
      req(main_env$EAL$page == 9)
      req(isTRUE(main_env$local_rv$eml_written))

      shinyjs::enable("publish")
      shinyjs::enable("download_data_package")
      # shinyjs::enable("download_emldown")
    })

    # Bug report ----
    observeEvent(input$bug_report, {
      utils::browseURL("https://github.com/earnaud/MetaShARK-v2/issues/")
    },
    label = "EAL9: bug report"
    )

    # Download Data Package ----
    output$download_data_package <- downloadHandler(
      filename = function() {
        paste0(main_env$save_variable$SelectDP$dp_name, ".zip")
      },
      content = function(file) {
        .old_wd <- getwd()
        setwd(main_env$save_variable$SelectDP$dp_path)

        zip::zip(
          zipfile = file,
          files = dir(
            main_env$save_variable$SelectDP$dp_path,
            # full.names = TRUE,
            recursive = TRUE
          )
        )

        setwd(.old_wd)
      }
    )

    # publish ----
    observeEvent(input$publish, {
      # Save work at this state
      saveReactive(main_env, main_env$EAL$page, do_template = FALSE)
      # Clean & reset variables
      main_env <- cleanModules(main_env)
      # Change side menu to upload tab
      browser()
    })

    # # emldown ----
    # output$download_emldown <- downloadHandler(
    #   filename = function() {
    #     paste0(
    #       main_env$save_variable$SelectDP$dp.name,
    #       "_emldown.zip"
    #     )
    #   },
    #   content = function(file) {
    #     .old_wd <- getwd()
    #     setwd(paste0(main_env$save_variable$SelectDP$dp.path,"/emldown"))
    #
    #     zip::zip(
    #       zipfile = file,
    #       files = dir(
    #         paste0(
    #           main_env$save_variable$SelectDP$dp.path,
    #           "/emldown"
    #         ),
    #         # full.names = TRUE,
    #         recursive = TRUE
    #       )
    #     )
    #
    #     setwd(.old_wd)
    #   }
    # )
    #
    # Display ====
    observe({
      req(main_env$EAL$page == 9)
      invalidateLater(1000)

      validate(
        need(
          length(dir(main_env$save_variable$SelectDP$dp_eml_path)) > 0,
          "No EML file generated"
        )
      )

      # Allow to publish or not
      browser()
      valid <- EML::eml_validate(
        dir(
          main_env$save_variable$SelectDP$dp_eml_path,
          pattern = main_env$save_variable$SelectDP$dp_title,
          full.names = TRUE
        )
      )
      shinyjs::toggleState("publish", valid)
      shinyjs::toggleState("download_data_package", valid)

      # # Allow to access emldown or not
      # shinyjs::toggleState(
      #   "download_emldown",
      #   valid &&
      #     file.exists(
      #       paste0(
      #         isolate(main_env$save_variable$SelectDP$dp.path),
      #         "/emldown/emldown.html"
      #       )
      #     )
      # )
    })
  })
}
