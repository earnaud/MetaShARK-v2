#' @title Data Package Template filling
#'
#' @description UI part of the Attributes module. Fill in the attributes of the data package
#'
#' @import shiny
#'
#' @noRd
AttributesUI <- function(id, main.env) {
  ns <- NS(id)

  return(
    fluidPage(
      tags$b("TEST:"),
      textOutput(NS(id, "testCU")),
      tagList(
        "Even if EML Assembly Line automatically infers most
        of your data's metadata, some steps need you to check
        out. Please check the following attribute, and fill
        in at least the", withRedStar("mandatory elements.")
      ),
      # Attributes ----
      fluidRow(
        column(1,
          actionButton(
            NS(id, "file_prev"), "",
            icon("chevron-left")
          )
        ),
        column(10,
          uiOutput(
            NS(id, "current_file"),
            inline = TRUE
          )
        ),
        column(1,
          actionButton(
            NS(id, "file_next"), "",
            icon("chevron-right")
          )
        )
      ),
      fluidRow(
        column(
          12,
          uiOutput(NS(id, "edit_attributes"))
        )
      ),
      # Custom Units ----
      tags$div(
        id = "custom_units",
        tags$hr(),
        tags$h4("Custom units"),
        fluidRow(
          tableOutput(NS(id, "CUUI"))
        )
      )
    ) # end fluidPage
  ) # end return
}

#' @importFrom data.table fwrite
#' @import shiny
#' @importFrom shinyjs hide show enable disable onclick
#' @importFrom EMLassemblyline template_categorical_variables template_geographic_coverage
#' @importFrom shinyBS bsCollapse bsCollapsePanel updateCollapse
#'
#' @noRd
Attributes <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    
    # Variable initialization (deprecated)
    
    # Navigation buttons ====
    # Previous file
    observeEvent(input$file_prev, {
      req(main.env$EAL$page == 3)
      req(main.env$local.rv$current$file > 1)
      
      # Change file
      main.env$local.rv$current$file <- main.env$local.rv$current$file - 1
    },
    label = "EAL3: prev file"
    )

    # Next file
    observeEvent(input$file_next, {
      req(main.env$EAL$page == 3)
      req(main.env$local.rv$current$file < length(main.env$local.rv$md.filenames))
      
      # Change file
      main.env$local.rv$current$file <- main.env$local.rv$current$file + 1
    },
    label = "EAL3: next file"
    )
    
    # En/disable buttons
    observe({
      req(main.env$EAL$page == 3)
      
      shinyjs::toggleState(
        "file_prev", 
        condition = main.env$local.rv$current$file > 1
      )
      shinyjs::toggleState(
        "file_next",
        condition = main.env$local.rv$current$file < length(main.env$local.rv$md.filenames)
      )
    })
    
    # update table
    observeEvent({
      input$file_next
      input$file_prev
      main.env$EAL$page
    }, {
      req(main.env$EAL$page == 3)
      req(main.env$local.rv$current$file > 0)
      
      # shortcut for read variable
      .file <- main.env$local.rv$current$file
      
      # Changes
      # - remove NA from current table
      .tab <- main.env$local.rv$md.tables[[.file]]
      .tab[is.na(.tab)] <- ""
      main.env$local.rv$md.tables[[.file]] <- .tab
      # - change preview table
      main.env$local.rv$current$preview <- readDataTable(
        main.env$local.rv$data.filepath[.file],
        stringsAsFactors = FALSE,
        nrows = 5
      )
      # - update toggled inputs
      main.env$local.rv$current$update.view$trigger()
    },
    ignoreNULL = FALSE,
    label = "EAL3: update table",
    priority = -1
    )

    # display
    output$current_file <- renderUI({
      req(main.env$EAL$page == 3)
      
      files <- main.env$local.rv$md.filenames
      current <- main.env$local.rv$current$file
      
      tags$div(
        files[current],
        class = "ellipsis",
        style = paste0(
          "display: inline-block;
          font-size:14pt;
          text-align:center;
          width:100%;
          background: linear-gradient(90deg, #3c8dbc ",
          round(100 * current / length(files)),
          "%, white ",
          round(100 * current / length(files)),
          "%);"
        )
      )
    })

    # Form ====
    
    # * UI ----
    output$edit_attributes <- renderUI({
      req(main.env$EAL$page == 3)
      
      current.file <- main.env$local.rv$current$file
      isolate({
        current.table <- main.env$local.rv$md.tables[[current.file]]
      })
      
      # validity check
      validate(
        need(
          isContentTruthy(current.table),
          "No valid table provided."
        )
      )
      message("renderui")
      # compute ui
      do.call(
        shinyBS::bsCollapse,
        args = c(
          lapply(
            seq(nrow(current.table)),
            function(row.index){
              isolate({
                attributeInputListUI(
                  id = session$ns(
                    paste(
                      current.file,
                      row.index,
                      sep = "-"
                    )
                  ),
                  row.index = row.index,
                  main.env = main.env
                )
              }) # end isolate
            }
          ),
          id = NS(id, "collapse")
        )
      ) # end do.call
    })
    
    # * Server ----
    observeEvent({
      input$file_next
      input$file_prev
      main.env$EAL$page
    }, {
      req(main.env$EAL$page == 3)
      req(isContentTruthy(main.env$local.rv$md.tables[[main.env$local.rv$current$file]]))
      
      sapply(
        seq(nrow(main.env$local.rv$md.tables[[main.env$local.rv$current$file]])),
        function(row.index) {
          attributeInputList(
            id = paste(
              isolate(main.env$local.rv$current$file),
              row.index,
              sep = "-"
            ),
            row.index = row.index,
            main.env = main.env
          )
        }
      ) # end of sapply : row.index
    },
    ignoreNULL = FALSE,
    label = "EAL3: set server",
    priority = -2
    ) # end of observeEvent

    # Custom units ====
    output$testCU <- renderText({
      main.env$local.rv$custom.units$modal.state
    })
    
    # * CU Input ----
    observeEvent({
      if(main.env$EAL$page == 3)
        main.env$local.rv$custom.units$trigger()
      else 
        FALSE
    }, {
    # observe({
      req(main.env$EAL$page == 3)
      req(main.env$local.rv$custom.units$modal.state == "open")
      
      .file <- main.env$local.rv$current$file
      .current.table <- main.env$local.rv$md.tables[[.file]]
      
      req(isContentTruthy(.current.table) && 
            any(.current.table$unit == "custom"))
      
      .row <- which(.current.table$unit == "custom")
      
      # Save input id being modified
      main.env$local.rv$custom.units$unit.id <- c(.file, .row, "unit")
      
      # Set values for custom units if existing
      main.env$local.rv$custom.units$values <- if(isContentTruthy(main.env$local.rv$custom.units$table))
        main.env$local.rv$custom.units$table %>%
        dplyr::filter(grepl(class, id))
      else
        rep(NA, 5)
      
      # Properly show modal 
      showModal(
        customUnitsUI(
          session$ns("customUnits"),
          values = main.env$local.rv$custom.units$values,
          cu.table = main.env$local.rv$custom.units$table
        )
      )
    },
    priority = -1,
    label = "EAL3: observe CU"
    )
    
    # * CU server ----
    customUnits(
      "customUnits",
      main.env = main.env
    )
    
    # * Post-input ----
    observeEvent({
      if(main.env$EAL$page == 3)
        main.env$local.rv$custom.units$trigger()
      else 
        FALSE
    }, {
    # observe({
      req(main.env$EAL$page == 3)
      req(main.env$local.rv$custom.units$modal.state == "closing")
      
      .file <- main.env$local.rv$current$file
      .current.table <- main.env$local.rv$md.tables[[.file]]
      .row <- main.env$local.rv$custom.units$unit.id[2]
      
      # Cancelled
      if("custom" %in% .current.table[.row, "unit"]) {
        updateSelectInput(
          session,
          paste(main.env$local.rv$custom.units$unit.id, collapse = "-"),
          choices = setUnitList(main.env),
          selected = main.env$FORMATS$units[2] # dimensionless
        )
      } else { # Submit
        updateSelectInput(
          session,
          paste(main.env$local.rv$custom.units$unit.id, collapse = "-"),
          choices = setUnitList(main.env),
          selected = main.env$local.rv$custom.units$values["id"]
        )
      }
      
      # Toggle custom units table render
      shinyjs::toggle(
        id = "custom_units",
        condition = isFALSE(isContentTruthy(main.env$local.rv$custom.units$table))
      )
      
      main.env$local.rv$custom.units$modal.state <- "closed"
    },
    priority = -1,
    label = "EAL3: post-fill CU"
    )
    
    # Verbose
    output$CUUI <- renderTable({
      req(main.env$EAL$page == 3)
      
      validate(
        need(
          isContentTruthy(main.env$local.rv$custom.units$table),
          "No custom units registered"
        )
      )
      
      main.env$local.rv$custom.units$table
    })
    
    # Saves ----
    observe({
      req(main.env$EAL$page == 3)
      
      invalidateLater(1000)
      
      req(
        length(main.env$local.rv$md.tables) != 0 &&
          !any(sapply(main.env$local.rv$md.tables, identical, y = data.frame()))
      )
      
      # * Feedback ----
      shinyBS::updateCollapse(
        session,
        "collapse",
        style = lapply(
          main.env$local.rv$md.filenames[main.env$local.rv$current$file],
          function(filename) {
            lapply(names(main.env$local.rv$completed[[filename]]), function(att){
              ifelse(
                main.env$local.rv$completed[[filename]][[att]] %>%
                  listReactiveValues() %>%
                  unlist() %>%
                  all() %>%
                  isTRUE(),
                "success",
                "warning"
              )
            }) %>%
              setNames(nm = names(main.env$local.rv$completed[[filename]]))
          }
        ) %>% unlist(recursive = F)
      )
      
      # * Check completeness ----
      main.env$EAL$completed <- all(
        unlist(
          listReactiveValues(
            main.env$local.rv$completed
          )
        )
      )
    },
    priority = -5,
    label = "EAL3: check completed"
    )

    # Process data (deprecated)
    # observeEvent(main.env$EAL$page, {
    #   req(main.env$EAL$old.page == 3)
    #   
    #   # * Templating
    #   # for each attribute data frame
    #   .do.template.catvars <- sapply(
    #     seq_along(main.env$local.rv$md.filenames),
    #     function(cur_ind) {
    #       # check for direction: CustomUnits or CatVars
    #       return(isTRUE("categorical" %in% main.env$local.rv$md.tables[[cur_ind]][, "class"]))
    #     }
    #   ) %>%
    #     unlist() %>%
    #     any()
    #   
    #   # EMLAL: template new fields if needed
    #   if (isTRUE(.do.template.catvars)) {
    #     try(
    #       EMLassemblyline::template_categorical_variables(
    #         path = main.env$save.variable$SelectDP$dp.metadata.path,
    #         data.path = main.env$save.variable$SelectDP$dp.data.path
    #       )
    #     )
    #   }
    #   
    #   try(
    #     EMLassemblyline::template_geographic_coverage(
    #       path = main.env$save.variable$SelectDP$dp.metadata.path,
    #       data.path = main.env$save.variable$SelectDP$dp.data.path,
    #       empty = TRUE,
    #       write.file = TRUE
    #     )
    #   )
    #   
    #   if (isFALSE(.do.template.catvars)) {
    #     isolate(main.env$EAL$page <- main.env$EAL$page + 1)
    #   }
    # },
    # priority = -1,
    # label = "EAL3: process data",
    # ignoreInit = TRUE
    # )
  })
}
