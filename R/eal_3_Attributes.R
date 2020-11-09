#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' 
#' @noRd
AttributesUI <- function(id, main.env) {
  ns <- NS(id)

  return(
    fluidPage(
      tags$p(
        "Even if EML Assembly Line automatically infers most
        of your data's metadata, some steps need you to check
        out. Please check the following attribute, and fill
        in at least the", withRedStar("mandatory elements.")
      ),
      # Attributes ----
      tags$div(
        tags$h4("Attributes"),
        uiOutput(NS(id, "edit_attributes")) %>%
          shinycssloaders::withSpinner()
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

#' @import shiny
#' @importFrom shinyBS bsCollapse updateCollapse
#' @importFrom dplyr filter
#' @importFrom shinyjs toggle
#'
#' @noRd
Attributes <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    
    # Variable initialization (deprecated)
    
    # Form ====
    # * UI ----
    output$edit_attributes <- renderUI({
      req(main.env$EAL$page == 3)
      
      isolate({
        # validity check
        validate(
          need(
            isContentTruthy(main.env$local.rv$md.tables),
            "No valid table provided."
          )
        )
        
        # compute ui
        ui <- do.call(
          tabsetPanel,
          args = c(
            id = session$ns("tabset"),
            lapply(
              names(main.env$local.rv$md.tables),
              main.env = main.env,
              # Table input - tab
              function(table.name, main.env) {
                # Set variables
                table <- main.env$local.rv$md.tables[[table.name]]
                .id <- session$ns(table.name)
                
                # Render UI
                tabPanel(
                  title = table.name,
                  value = table.name,
                  # Create a container of collapsibles
                  do.call(
                    shinyBS::bsCollapse,
                    args = c(
                      id = NS(.id, "collapse"),
                      multiple = FALSE,
                      # Create a collapsible per attribute
                      lapply(
                        seq(nrow(table)),
                        .attributeInputUI,
                        id = .id,
                        table.name = table.name,
                        main.env = main.env
                      )
                    )
                  ) # end of do.call:bsCollapse
                ) # end of tabPanelBody
              }
            ) # end of sapply
          )
        )
      })
      
      return(ui)
    })
    
    # * Server ----
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page == 3)
      validate(
        need(isContentTruthy(main.env$local.rv$md.tables), message = FALSE)
      )
      
      sapply(
        names(main.env$local.rv$md.tables), 
        main.env = main.env,
        id = id,
        # Table input - tab
        # not a module ! just multiple calls
        function(id, table.name, main.env) {
          # Set server
          sapply(
            seq(nrow(main.env$local.rv$md.tables[[table.name]])),
            .attributeInput,
            main.env = main.env,
            id = table.name
          )
        }
      ) # end of sapply
    },
    ignoreNULL = FALSE,
    label = "EAL3: set server",
    priority = -2
    ) # end of observeEvent

    # Custom units ====
    output$testCU <- renderText(main.env$local.rv$custom.units$modal.state)
    
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
      # Collapse
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
  })
}
