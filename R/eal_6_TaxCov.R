#' @import shiny
#'
#' @noRd
TaxCovUI <- function(id) {
  ns <- NS(id)

  return(
    fluidPage(
      fluidRow(
        column(
          4,
          tags$fieldset(
            tags$legend("Taxonomic attribute"),
            selectInput(
              ns("taxa_col"),
              helpLabel(
                "Select taxonomic attribute",
                "Select an attribute containing taxa names (either common
              or scientific)."
              ),
              choices = c(),
              selected = c(),
              multiple = FALSE
            ),
            tags$b("Column preview"),
            tableOutput(ns("preview"))
          )
        ),
        column(
          4,
          tags$fieldset(
            tags$legend("Taxa name style"),
            checkboxGroupInput(
              ns("taxa_name_type"),
              helpLabel(
                "Select taxonomic name notation",
                "Selection has to be made according to the content
              of the taxonomic attribute selected previously."
              ),
              c("scientific", "common")
            )
          )
        ),
        column(
          4,
          tags$fieldset(
            tags$legend("Taxa authority"),
            selectInput(
              ns("taxa_authority"),
              "Select taxonomic authority.ies",
              choices = c(),
              selected = c(),
              multiple = TRUE
            )
          )
        )
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom dplyr select filter
#'
#' @noRd
TaxCov <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    if (main_env$dev) .browse_dev(main_env, 6, input, output, session)

    # Set UI ====

    ## taxa_col ----
    # observeEvent(main_env$EAL$page, {
    #   req(main_env$EAL$page == 6)
    # 
    #   # shortcut for attributes content
    #   .att <- main_env$save_variable$Attributes$content
    # 
    #   # Set choices for selectInput -- reuse & filter Attributes
    #   .choice <- main_env$local_rv$taxa_choices <- list()
    #   sapply(names(.att), function(.md_file) {
    #     .data_file <- main_env$save_variable$DataFiles |>
    #       filter(grepl(.md_file, metadatapath)) |>
    #       select(datapath) |>
    #       unlist() |>
    #       basename()
    #     # Set sites
    #     .choice[[.data_file]] <<- .att[[.md_file]] |>
    #       as.data.frame() |>
    #       dplyr::filter(class %in% c("character", "categorical")) |>
    #       dplyr::select(attributeName) |>
    #       unlist()
    #     .choice[[.data_file]] <<- paste(
    #       .data_file, .choice[[.data_file]],
    #       sep = "/"
    #     ) |>
    #       setNames(nm = .choice[[.data_file]])
    #   })
    # 
    #   # Set value -- read from saved
    #   .value <- if (isContentTruthy(main_env$save_variable$TaxCov)) {
    #     paste(
    #       main_env$local_rv$taxa_col,
    #       main_env$local_rv$taxa_col,
    #       sep = "/"
    #     ) |>
    #       setNames(nm = main_env$local_rv$taxa_col)
    #   }
    # 
    #   # UI itself
    #   updateSelectInput(
    #     session,
    #     "taxa_col",
    #     choices = .choice,
    #     selected = .value
    #   )
    # })

    output$preview <- renderTable({
      validate(
        need(isTruthy(main_env$local_rv$taxa_col), "invalid taxa selection")
      )

      .file <- main_env$save_variable$DataFiles$datapath |>
        as.data.frame() |>
        setNames(nm = "filenames")
      .file <- dplyr::filter(
          .file,
          grepl(pattern = main_env$local_rv$taxa_table, filenames)
        ) |>
        unlist()

      .data <- data.table::fread(
        .file,
        nrows = 5, header = TRUE,
        data.table = FALSE
      )[main_env$local_rv$taxa_col]

      return(.data)
    },
    priority = -1
    )

    ## taxa_name_type ----
    # observeEvent(main_env$EAL$page, {
    #   req(main_env$EAL$page == 6)
    # 
    #   .value <- main_env$local_rv$taxa_name_type
    #   if (isTRUE(.value == "both")) {
    #     .value <- c("scientific", "common")
    #   } else if (isFALSE(.value %in% c("scientific", "common"))) {
    #     .value <- NULL
    #   }
    # 
    #   updateCheckboxGroupInput(
    #     session, "taxa_name_type",
    #     selected = .value
    #   )
    # },
    # priority = -1
    # )

    ## taxa_authority ----
    # observeEvent(main_env$EAL$page, {
    #   req(main_env$EAL$page == 6)
    # 
    #   taxa_authorities <- main_env$FORMATS$taxa_authorities
    #   choices <- taxa_authorities$authority
    #   value <- if (isTruthy(main_env$local_rv$taxa_authority)) {
    #     taxa_authorities |>
    #       dplyr::filter(id == main_env$local_rv$taxa_authority) |>
    #       dplyr::select(authority)
    #   }
    # 
    #   updateSelectInput(
    #     session,
    #     "taxa_authority",
    #     "Select taxonomic authority.ies",
    #     choices = choices,
    #     selected = value
    #   )
    # },
    # priority = -1
    # )

    # Taxonomic coverage input ====

    ## Taxa col ----
    observeEvent(input$taxa_col, {
      # save
      .tmp <- input$taxa_col |>
        strsplit(split = "/", fixed = TRUE) |>
        unlist()
      main_env$local_rv$taxa_table <- .tmp[1]
      main_env$local_rv$taxa_col <- .tmp[2]
    },
    label = "EAL6: input taxa_table",
    priority = -1
    )

    ## Taxa type ----
    observeEvent(input$taxa_name_type, {
      if (isTruthy(input$taxa_name_type)) {
        main_env$local_rv$taxa_name_type <- input$taxa_name_type

        if ("scientific" %in% main_env$local_rv$taxa_name_type &&
            "common" %in% main_env$local_rv$taxa_name_type) {
          main_env$local_rv$taxa_name_type <- "both"
        }
      } else {
        main_env$local_rv$taxa_name_type <- character()
      }
    },
    ignoreNULL = FALSE,
    label = "EAL6: input taxa name type"
    )

    ## Taxa authority ----
    observeEvent(input$taxa_authority, {
      if (isTruthy(input$taxa_authority)) {
        main_env$local_rv$taxa_authority <- main_env$FORMATS$taxa_authorities |>
          dplyr::filter(authority %in% input$taxa_authority) |>
          dplyr::select(id) |>
          unlist()
      } else {
        main_env$local_rv$taxa_authority <- character()
      }
    },
    ignoreNULL = FALSE,
    label = "EAL6: input taxa authority"
    )

    # Saves ====
    observeEvent({
      input$taxa_col
      input$taxa_name_type
      input$taxa_authority
    }, {
      req(main_env$EAL$page == 6)

      main_env$EAL$completed <- TRUE # step is facultative
      main_env$local_rv$complete <- all(
        length(main_env$local_rv$taxa_table) > 0 &&
          length(main_env$local_rv$taxa_col) > 0 &&
          length(main_env$local_rv$taxa_name_type) > 0 &&
          length(main_env$local_rv$taxa_authority) > 0
      )

      if (isFALSE(main_env$local_rv$complete)) {
        main_env$EAL$tag_list <- tagList(
          tags$b("Incomplete coverage !"),
          tags$p("Going to next step will skip taxonomic coverage.")
        )
      } else {
        main_env$EAL$tag_list <- tagList()
      }
    },
    label = "EAL6: set completed",
    priority = -3
    )
  })
}
