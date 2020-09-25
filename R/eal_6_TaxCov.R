#' @import shiny
#' @importFrom shinyjs hidden disabled
#'
#' @noRd
TaxCovUI <- function(id, main.env) {
  return(
    fluidPage(
      fluidRow(
        column(4, uiOutput(NS(id, "taxa.table")) ), # uiOutput(NS(id, "taxa.col")) ),
        column(4, uiOutput(NS(id, "taxa.name.type")) ),
        column(4, uiOutput(NS(id, "taxa.authority")) )
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
          ) %>% setNames(
            nm = main.env$local.rv$taxa.table
          )
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

    # # * taxa.col
    # output$taxa.col <- renderUI({
    #   .ind <- match(main.env$local.rv$taxa.table, main.env$save.variable$DataFiles$name)
    #   if (isTruthy(.ind)) {
    #     choices <- isolate(main.env$save.variable$Attributes[[.ind]]$attributeName)
    #     value <- isolate(main.env$local.rv$taxa.col)
    #     if (isFALSE(value %in% choices)) {
    #       value <- NULL
    #     }
    #     .embed <- tagList
    #   }
    #   else {
    #     choices <- NULL
    #     value <- NULL
    #     .embed <- shinyjs::disabled
    #   }
    # 
    #   .embed(
    #     selectInput(
    #       session$ns("taxa.col"),
    #       "Columns from selected files",
    #       choices = choices,
    #       selected = value
    #     )
    #   )
    # })

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
        # invalid-selected/no value(s)
        # if (!isTruthy(input$taxa.table)) {
        #   shinyjs::disable("taxa.col")
        # 
        #   updateSelectizeInput(
        #     session,
        #     "taxa.col",
        #     choices = list("(None selected)" = NULL)
        #   )
        # }
        # valid-selected value(s)
        # else {
          # shinyjs::enable("taxa.col")
          # 
          # taxa.col.list <- lapply(input$taxa.table, function(file) {
          #   all.files <- main.env$save.variable$DataFiles
          #   file <- all.files[all.files$name == file, "datapath"]
          #   df <- readDataTable(file, stringsAsFactors = FALSE)
          #   return(colnames(df))
          # })
          # names(taxa.col.list) <- input$taxa.table
# 
#           .update.var <- if (isTRUE(main.env$local.rv$taxa.col %in% taxa.col.list)) {
#             main.env$local.rv$taxa.col
#           } else {
#             NULL
#           }
# 
#           updateSelectizeInput(
#             session,
#             "taxa.col",
#             choices = taxa.col.list,
#             selected = .update.var
#           )
        # }

        # save
        .tmp <- strsplit(input$taxa.table, split = "/", fixed = TRUE) %>%
          unlist
        main.env$local.rv$taxa.table <- .tmp[1]
        main.env$local.rv$taxa.col <- .tmp[2]
        # isolate({
        #   main.env$save.variable$TaxCov$taxa.table <- main.env$local.rv$taxa.table
        #   main.env$save.variable$TaxCov$taxa.table <- main.env$local.rv$taxa.table
        # })
      },
      label = "EAL6: input taxa table",
      priority = -1
    )

    # # * Taxa columns to read
    # observeEvent(input$taxa.col, {
    #   req(input$taxa.col)
    # 
    #   main.env$local.rv$taxa.col <- input$taxa.col
    #   isolate({
    #     main.env$save.variable$TaxCov$taxa.col <- main.env$local.rv$taxa.col
    #   })
    # },
    # label = "EAL6: input taxa column"
    # )

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
      # main.env$save.variable$TaxCov$taxa.name.type <- main.env$local.rv$taxa.name.type
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
      # main.env$save.variable$TaxCov$taxa.authority <- main.env$local.rv$taxa.authority
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
    # observeEvent(main.env$EAL$page,
    #   {
    #     req(main.env$EAL$old.page == 6)
    #     message(NS(id, "proceed"))
    # 
    #     choices <- c(
    #       "Yes - Taxonomic coverage will be written as file" = if (main.env$EAL$completed) 1 else NULL,
    #       "No - Taxonomic coverage will be left blank" = 0
    #     )
    # 
    #     nextTabModal <- modalDialog(
    #       title = "Proceed Taxonomic Coverage",
    #       tagList(
    #         "You are getting ready to proceed.",
    #         radioButtons(
    #           NS(id, "filled"),
    #           "Is Taxonomic Coverage filled?",
    #           choices = choices
    #         )
    #       ),
    #       footer = tagList(
    #         modalButton("Cancel"),
    #         actionButton(NS(id, "confirm"), "Proceed")
    #       )
    #     )
    # 
    #     showModal(nextTabModal)
    #   },
    #   priority = 1,
    #   label = "EAL6: process data",
    #   ignoreInit = TRUE
    # )
    # 
    # observeEvent(input$confirm, {
    #   removeModal()
    #   main.env$EAL$old.page <- main.env$EAL$page
    #   main.env$EAL$page <- 7
    #   main.env$EAL$tag.list <- tagList()
    # 
    #   # Write files
    #   if (input$filled == "1") {
    #     # Save metadata
    #     saveReactive(main.env)
    # 
    #     # Template coverage
    #     x <- template(main.env, main.env$EAL$old.page)
    #     showNotification(
    #       "Taxonomic Coverage has been written.",
    #       type = "message"
    #     )
    #   }
    #   else {
    #     showNotification(
    #       "Taxonomic Coverage has been skipped.",
    #       type = "message"
    #     )
    #   }
    # },
    # label = "EAL6: confirm process data"
    # )
  })
}
