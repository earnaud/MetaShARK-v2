#' @title MakeEMLUI
#'
#' @description UI part for the Make EML module
#'
#' @import shiny
#' actionLink column
#' @importFrom shinyjs hidden disabled
MakeEMLUI <- function(id, title, dev) {
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
          ns("make_eml"),
          "Make EML",
          icon("edit"),
          width = "50%"
        ),
        textOutput(ns("warnings")),
        shinyjs::hidden(
          actionLink(
            ns("bug_report"),
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
            downloadButton(
              ns("download_emldown"),
              "Download emldown",
              width = "50%"
            )
          ) # End of emldown
        )
      )
    ) # end of fluidPage
  ) # end of return
}

#' @describeIn MakeEMLUI
#'
#' @description server part of the make EML module
#'
#' @import shiny
#' @importFrom shinyjs show enable onclick
#' @importFrom EMLassemblyline make_eml template_arguments
#' @importFrom emldown render_eml
MakeEML <- function(input, output, session, savevar, main.env) {
  ns <- session$ns

  if (main.env$DEV) {
    shinyjs::onclick("dev",
      {
        req(main.env$EAL$navigate == 9)
        browser()
      },
      asis = TRUE
    )
  }

  # Variable initialization -----------------------------------------------------
  out.file <- paste0(
    savevar$emlal$SelectDP$dp.path,
    "/emldown/emldown.html"
  )

  # Make eml -----------------------------------------------------
  observeEvent(input$make_eml, {
    shinyjs::hide("bug_report")
    req(input$make_eml)
    withProgress(
      {
        . <- savevar$emlal
        fileName <- .$SelectDP$dp_title

        x <- try(
          EMLassemblyline::template_arguments(
            path = .$SelectDP$dp_metadata_path,
            data.path = .$SelectDP$dp_data_path,
            data.table = dir(.$SelectDP$dp_data_path)
          )
        )

        if (class(x) == "try-error") {
          out <- x
          out[1] <- paste("Upon templating arguments:", x)
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

          .test <- 0
          out <- try(
            do.call(
              EMLassemblyline::make_eml,
              x[names(x) %in% names(formals(make_eml))]
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

    valid.eml <- EML::eml_validate(
      dir(
        savevar$emlal$SelectDP$dp.eml.path,
        full.names = TRUE
      )
    )

    output$warnings <- renderText({
      disable("publish")
      disable("emldown")
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
        savevar$emlal$SelectDP$dp.eml.path,
        full.names = TRUE,
        pattern = savevar$emlal$SelectDP$dp.title
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
  })

  observeEvent(input$bug_report, {
    utils::browseURL("https://github.com/earnaud/MetaShARK-v2/issues/26")
  })

  # emldown -----------------------------------------------------
  output$download_emldown <- downloadHandler(
    filename = function() {
      paste(
        savevar$emlal$SelectDP$dp.name,
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

  # Output -----------------------------------------------------
  return(savevar)
}
