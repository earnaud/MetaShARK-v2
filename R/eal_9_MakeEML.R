#' @import shiny
#' @importFrom shinyjs hidden disabled
#'
#' @noRd
MakeEMLUI <- function(id, main.env) {
  ns <- NS(id)

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
        textOutput(NS(id, "warnings")),
        shinyjs::hidden(
          actionLink(
            NS(id, "bug_report"),
            span("Please report this to the dev", icon("external-link-alt"))
          )
        ),
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
            shinyjs::disabled(
              downloadButton(
                NS(id, "download_emldown"),
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

#' @import shiny
#' @importFrom shinyjs show enable onclick
#' @importFrom EMLassemblyline make_eml template_arguments
#' @importFrom emldown render_eml
#'
#' @noRd
MakeEML <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Variable initialization ----
    observe({
      req(main.env$EAL$page == 9)
      main.env$EAL$page.load$depend()
      
      out.file <- paste0(
        isolate(main.env$save.variable$SelectDP$dp.path),
        "/emldown/emldown.html"
      )
    },
    label = "EAL9: set values"
    )
    
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
            x$maintenance.description <- "Ongoing"
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
            x$user.id <- optional(main.env$SETTINGS$user)
            # TODO user domain -- PNDB ?
            # x$user.domain <- "UserDomain"
            # x$package.id is set as UUID (default)
            x$write.file <- TRUE
            x$return.obj <- TRUE

            # Yet written in the files then used in make_eml
            x$geographic.coordinates <- NULL
            x$geographic.description <- NULL

            incProgress(0.2)

            .test <- 0
            out <- try(
              do.call(
                EMLassemblyline::make_eml,
                x[names(x) %in% names(formals(EMLassemblyline::make_eml))]
              )
            )
            if (class(out) == "try-error") {
              out[1] <- paste("Upon writing EML:", out)
            }
            incProgress(0.4)
          }
        },
        message = "Writing EML ...",
        value = 0.1
      )
      browser()
      valid.eml <- EML::eml_validate(
        dir(
          main.env$save.variable$SelectDP$dp.eml.path,
          full.names = TRUE
        )
      )

      output$warnings <- renderText({
        shinyjs::disable("publish")
        shinyjs::disable("emldown")
        validate(
          need(
            class(out) != "try-error",
            out[1]
          ),
          need(
            !isFALSE(valid.eml),
            unique(attr(valid.eml, "errors"))
          )
        )
        shinyjs::enable("publish")
        shinyjs::enable("emldown")
        return(NULL)
      })

      if (class(out) == "try-error" ||
        isFALSE(valid.eml)) {
        shinyjs::show("bug_report")
        showNotification("EML invalid", type = "error", duration = NULL)
      } else {
        shinyjs::hide("bug_report")
        showNotification("EML written.", type = "message")

        # emldown
        eml.file <- dir(
          main.env$save.variable$SelectDP$dp.eml.path,
          full.names = TRUE,
          pattern = main.env$save.variable$SelectDP$dp.title
        )
        dir.create(dirname(out.file), recursive = TRUE)
        old.wd <- getwd()
        setwd(dirname(out.file))
        out <- emldown::render_eml(
          file = eml.file,
          open = TRUE,
          out.file = out.file,
          publish_mode = TRUE
        )
        setwd(old.wd)
        if (file.exists(out.file)) {
          showNotification("emldown generated", type = "message")
        }
      }
    },
    label = "EAL9: make eml"
    )

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
  })
}
