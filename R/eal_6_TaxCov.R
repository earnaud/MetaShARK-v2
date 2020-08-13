#' @import shiny
#' @importFrom shinyjs hidden disabled
#'
#' @noRd
TaxCovUI <- function(id, main.env) {
  return(
    fluidPage(
      fluidRow(
        column(
          6,
          uiOutput(NS(id, "taxa.table")),
          uiOutput(NS(id, "taxa.col"))
        ),
        column(
          6,
          uiOutput(NS(id, "taxa.name.type")),
          uiOutput(NS(id, "taxa.authority"))
        )
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom stringr str_extract_all
#' @importFrom dplyr %>%
#' @importFrom EMLassemblyline template_taxonomic_coverage
#' @importFrom shinyjs onclick
#'
#' @noRd
TaxCov <- function(id, full.id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Variable Initialization ----
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page == 6)
      
      if (all(sapply(printReactiveValues(main.env$save.variable$TaxCov), checkTruth))) {
        main.env$local.rv$taxa.table <- main.env$save.variable$TaxCov$taxa.table
        main.env$local.rv$taxa.col <- main.env$save.variable$TaxCov$taxa.col
        main.env$local.rv$taxa.name.type <- main.env$save.variable$TaxCov$taxa.authority
        main.env$local.rv$taxa.authority <- main.env$save.variable$TaxCov$taxa.authority
      }
    },
    label = "EAL6: set value")

    # Set UI ====

    # * taxa.table ====
    output$taxa.table <- renderUI({
      isolate({
        data.files <- basename(main.env$save.variable$DataFiles$datapath)
        value <- main.env$local.rv$taxa.table
        if (isFALSE(value %in% data.files)) {
          value <- NULL
        }
      })

      selectInput(
        NS(full.id, "taxa.table"),
        "Files containing taxonomic references",
        choices = data.files,
        selected = value,
        multiple = FALSE
      )
    })

    # * taxa.col ====
    output$taxa.col <- renderUI({
      .ind <- match(main.env$local.rv$taxa.table, main.env$save.variable$DataFiles$name)
      if (isTruthy(.ind)) {
        choices <- isolate(main.env$save.variable$Attributes[[.ind]]$attributeName)
        value <- isolate(main.env$local.rv$taxa.col)
        if (isFALSE(value %in% choices)) {
          value <- NULL
        }
        .embed <- tagList
      }
      else {
        choices <- NULL
        value <- NULL
        .embed <- shinyjs::disabled
      }

      .embed(
        selectInput(
          NS(full.id, "taxa.col"),
          "Columns from selected files",
          choices = choices,
          selected = value
        )
      )
    })

    # * taxa.name.type ====
    output$taxa.name.type <- renderUI({
      isolate({
        value <- main.env$local.rv$taxa.name.type
        if (isTRUE(value == "both")) {
          value <- c("scientific", "common")
        } else if (isFALSE(value %in% c("scientific", "common"))) {
          value <- NULL
        }
      })

      checkboxGroupInput(
        NS(full.id, "taxa.name.type"),
        "Select one or both taxonomic name notation",
        c("scientific", "common"),
        selected = value
      )
    })

    # * taxa.authority ====
    output$taxa.authority <- renderUI({
      isolate({
        taxa.authority <- main.env$FORMATS$taxa.authorities
        choices <- taxa.authority$authority
        value <- if (isTruthy(main.env$local.rv$taxa.authority)) {
          taxa.authority %>%
            dplyr::filter(id == main.env$local.rv$taxa.authority) %>%
            dplyr::select(authority)
        }
      })

      selectInput(
        NS(full.id, "taxa.authority"),
        "Select taxonomic authority.ies",
        choices = choices,
        selected = value,
        multiple = TRUE
      )
    })

    # Taxonomic coverage input ----

    # * Taxa files ====
    observeEvent(input$taxa.table,
      {
        # invalid-selected/no value(s)
        if (!isTruthy(input$taxa.table)) {
          shinyjs::disable("taxa.col")

          updateSelectizeInput(
            session,
            "taxa.col",
            choices = list("(None selected)" = NULL)
          )
        }
        # valid-selected value(s)
        else {
          shinyjs::enable("taxa.col")

          taxa.col.list <- lapply(input$taxa.table, function(file) {
            all.files <- main.env$save.variable$DataFiles
            file <- all.files[all.files$name == file, "datapath"]
            df <- readDataTable(file, stringsAsFactors = FALSE)
            return(colnames(df))
          })
          names(taxa.col.list) <- input$taxa.table

          .update.var <- if (isTRUE(main.env$local.rv$taxa.col %in% taxa.col.list)) {
            main.env$local.rv$taxa.col
          } else {
            NULL
          }

          updateSelectizeInput(
            session,
            "taxa.col",
            choices = taxa.col.list,
            selected = .update.var
          )
        }

        # save
        main.env$local.rv$taxa.table <- list(input$taxa.table)
        isolate({
          main.env$save.variable$TaxCov$taxa.table <- main.env$local.rv$taxa.table
        })
      },
      label = "EAL6: input taxa table",
      priority = -1
    )

    # * Taxa columns to read ====
    observeEvent(input$taxa.col, {
      req(input$taxa.col)

      main.env$local.rv$taxa.col <- input$taxa.col
      isolate({
        main.env$save.variable$TaxCov$taxa.col <- main.env$local.rv$taxa.col
      })
    },
    label = "EAL6: input taxa column"
    )

    # * Taxa type ====
    observeEvent(input$taxa.name.type, {
      req(input$taxa.name.type)

      main.env$local.rv$taxa.name.type <- input$taxa.name.type

      if ("scientific" %in% main.env$local.rv$taxa.name.type &&
        "common" %in% main.env$local.rv$taxa.name.type) {
        main.env$local.rv$taxa.name.type <- "both"
      }
      main.env$save.variable$TaxCov$taxa.name.type <- main.env$local.rv$taxa.name.type
    },
    label = "EAL6: input taxa name type"
    )

    # * Taxa authority ====
    observeEvent(input$taxa.authority, {
      req(input$taxa.authority)

      main.env$local.rv$taxa.authority <- main.env$FORMATS$taxa.authorities %>%
        dplyr::filter(authority %in% input$taxa.authority) %>%
        dplyr::select(id) %>%
        unlist()
      main.env$save.variable$TaxCov$taxa.authority <- main.env$local.rv$taxa.authority
    },
    label = "EAL6: input taxa authority"
    )

    # Saves ----
    observe({
      req(main.env$EAL$current == "Taxonomic Coverage")

      main.env$EAL$completed <- all(
        length(main.env$local.rv$taxa.table) > 0 &&
          length(main.env$local.rv$taxa.col) > 0 &&
          length(main.env$local.rv$taxa.name.type) > 0 &&
          length(main.env$local.rv$taxa.authority) > 0
      )
    },
    label = "EAL6: set completed")

    # Process data ----
    observeEvent(main.env$EAL$.next, 
      {
        req(main.env$EAL$current == "Taxonomic Coverage")

        choices <- c(
          "Yes - Taxonomic coverage will be written as file" = if (main.env$EAL$completed) 1 else NULL,
          "No - Taxonomic coverage will be left blank" = 0
        )

        nextTabModal <- modalDialog(
          title = "Proceed Taxonomic Coverage",
          tagList(
            "You are getting ready to proceed.",
            radioButtons(
              NS(id, "filled"),
              "Is Taxonomic Coverage filled?",
              choices = choices
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(NS(id, "confirm"), "Proceed")
          )
        )

        showModal(nextTabModal)
      },
      label = "EAL6: process data",
      ignoreInit = TRUE
    )

    observeEvent(input$confirm, {
      removeModal()
      main.env$EAL$page <- 7
      main.env$EAL$tag.list <- tagList()

      # Write files
      if (input$filled == "1") {
        saveReactive(main.env)
        #   save.variable = main.env$save.variable,
        #   rv = list(TaxCov = rv)
        # )

        # Template coverage
        try(
          template_taxonomic_coverage(
            main.env$save.variable$SelectDP$dp.metadata.path,
            main.env$save.variable$SelectDP$dp.data.path,
            taxa.table = main.env$local.rv$taxa.table,
            taxa.col = main.env$local.rv$taxa.col,
            taxa.name.type = main.env$local.rv$taxa.name.type,
            taxa.authority = main.env$local.rv$taxa.authority
          )
        )
        showNotification(
          "Taxonomic Coverage has been written.",
          type = "message"
        )
      }
      else {
        showNotification(
          "Taxonomic Coverage has been skipped.",
          type = "message"
        )
      }
    },
    label = "EAL6: confirm process data"
    )
  })
}
