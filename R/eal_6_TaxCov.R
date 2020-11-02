#' @import shiny
#'
#' @noRd
TaxCovUI <- function(id, main.env) {
  return(
    fluidPage(
      fluidRow(
        column(4, uiOutput(NS(id, "taxa.table")) ),
        column(4, uiOutput(NS(id, "taxa.name.type")) ),
        column(4, uiOutput(NS(id, "taxa.authority")) )
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom dplyr %>% select filter
#'
#' @noRd
TaxCov <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Variable Initialization (deprecated)

    # Set UI ====

    # * taxa.table ----
    output$taxa.table <- renderUI({
      isolate({
        # Set choices for selectInput -- reuse & filter Attributes
        .att <- main.env$save.variable$Attributes
        .choice <- main.env$local.rv$.taxa.choices <- list()
        sapply(names(.att), function(.file) {
          # Set sites
          .choice[[.file]] <<- .att[[.file]] %>% 
            dplyr::filter(class %in% c("character", "categorical")) %>% 
            dplyr::select(attributeName) %>%
            unlist
          .choice[[.file]] <<- paste(.file, .choice[[.file]], sep="/") %>%
            setNames(nm = .choice[[.file]])
        })
        # Set value -- read from saved
        .value <- if(isContentTruthy(main.env$save.variable$TaxCov)){
          paste(
            main.env$local.rv$taxa.table,
            main.env$local.rv$taxa.col,
            sep = "/"
          ) %>% setNames(nm = main.env$local.rv$taxa.table)
        }
      })

      # UI itself
      selectInput(
        session$ns("taxa.table"),
        "Files containing taxonomic references",
        choices = .choice,
        selected = .value,
        multiple = FALSE
      )
    })

    # * taxa.name.type ----
    output$taxa.name.type <- renderUI({
      isolate({
        .value <- main.env$local.rv$taxa.name.type
        if (isTRUE(.value == "both")) {
          .value <- c("scientific", "common")
        } else if (isFALSE(.value %in% c("scientific", "common"))) {
          .value <- NULL
        }
      })
      
      checkboxGroupInput(
        session$ns("taxa.name.type"),
        "Select one or both taxonomic name notation",
        c("scientific", "common"),
        selected = .value
      )
    })

    # * taxa.authority ----
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
        session$ns("taxa.authority"),
        "Select taxonomic authority.ies",
        choices = choices,
        selected = value,
        multiple = TRUE
      )
    })

    # Taxonomic coverage input ====

    # * Taxa files ----
    observeEvent(input$taxa.table, 
      {
        # save
        .tmp <- strsplit(input$taxa.table, split = "/", fixed = TRUE) %>%
          unlist
        main.env$local.rv$taxa.table <- .tmp[1]
        main.env$local.rv$taxa.col <- .tmp[2]
      },
      label = "EAL6: input taxa table",
      priority = -1
    )

    # * Taxa type ----
    observeEvent(input$taxa.name.type, {
      if(isTruthy(input$taxa.name.type)) {
        main.env$local.rv$taxa.name.type <- input$taxa.name.type
        
        if ("scientific" %in% main.env$local.rv$taxa.name.type &&
          "common" %in% main.env$local.rv$taxa.name.type) {
          main.env$local.rv$taxa.name.type <- "both"
        }
      } else
        main.env$local.rv$taxa.name.type <- character()
    },
    ignoreNULL = FALSE,
    label = "EAL6: input taxa name type"
    )

    # * Taxa authority ----
    observeEvent(input$taxa.authority, {
      if(isTruthy(input$taxa.authority))
        main.env$local.rv$taxa.authority <- main.env$FORMATS$taxa.authorities %>%
          dplyr::filter(authority %in% input$taxa.authority) %>%
          dplyr::select(id) %>%
          unlist()
      else
        main.env$local.rv$taxa.authority <- character()
    },
    ignoreNULL = FALSE,
    label = "EAL6: input taxa authority"
    )

    # Saves ====
    observe({
      req(main.env$EAL$page == 6)
      invalidateLater(1000)
      
      main.env$EAL$completed <- TRUE
      main.env$local.rv$complete <- all(
        length(main.env$local.rv$taxa.table) > 0 &&
          length(main.env$local.rv$taxa.col) > 0 &&
          length(main.env$local.rv$taxa.name.type) > 0 &&
          length(main.env$local.rv$taxa.authority) > 0
      )
      
      if(isFALSE(main.env$local.rv$complete)) {
        main.env$EAL$tag.list <- tagList(
          tags$b("Incomplete coverage !"),
          tags$p("Going to next step will skip taxonomic coverage.")
        )
      }
      else
        main.env$EAL$tag.list <- tagList()
    },
    label = "EAL6: set completed"
    )

    # Process data (deprecated)
  })
}
