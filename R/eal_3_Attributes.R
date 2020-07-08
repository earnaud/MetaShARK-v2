#' @title Data Package Template filling
#'
#' @description UI part of the Attributes module. Fill in the attributes of the data package
#'
#' @import shiny
AttributesUI <- function(id, title, dev) {
  ns <- NS(id)
  
  return(
    fluidPage(
      tagList(
        "Even if EML Assembly Line automatically infers most
        of your data's metadata, some steps need you to check
        out. Please check the following attribute, and fill
        in at least the", with_red_star("mandatory elements.")
      ),
      # Attributes
      fluidRow(
        column(
          1,
          actionButton(
            ns("file_prev"),
            "",
            icon("chevron-left")
          )
        ),
        column(
          10,
          uiOutput(ns("current_file"),
            inline = TRUE
          )
        ),
        column(
          1,
          actionButton(
            ns("file_next"),
            "",
            icon("chevron-right")
          )
        )
      ),
      fluidRow(
        column(
          12,
          uiOutput(ns("edit_attributes")) %>% 
            shinycssloaders::withSpinner(color = "#599cd4")
        )
      ),
      # Custom Units
      tags$h4("Custom Units"),
      fluidRow(
        tableOutput(ns("CUUI"))
      )
    ) # end fluidPage
  ) # end return
}

#' @describeIn AttributesUI
#'
#' @description server part of the Attributes module.
#'
#' @importFrom data.table fwrite
#' @import shiny
#' observe eventReactive validate
#' @importFrom shinyjs hide show enable disable onclick
#' @importFrom EMLassemblyline template_categorical_variables template_geographic_coverage
#' @importFrom shinyBS bsCollapse bsCollapsePanel updateCollapse
Attributes <- function(input, output, session,
  savevar, main.env, NSB){
  ns <- session$ns
  
  if (main.env$DEV) {
    onclick("dev",
      {
        req(main.env$EAL$navigate == 3)
        browser()
      },
      asis = TRUE
    )
  }
  
  if (main.env$DEV || isTRUE(savevar$emlal$quick)) {
    .fill <- function(rv = rv) {
      lapply(seq(rv$tables), function(ind) {
        .table <- rv$tables[[ind]]
        sapply(colnames(.table), function(col) {
          # Set values
          if (col == "attributeDefinition") {
            rv$tables[[ind]][[col]] <- paste("Description for", rv$tables[[ind]][["attributeName"]])
          }
          if (col %in% c("missingValueCodeExplanation", "missingValueCode")) {
            rv$tables[[ind]][[col]] <- rep("LoremIpsum", dim(.table)[1])
          }
          if (col == "dateTimeFormatString") {
            .dat.row <- which(rv$tables[[ind]]$class == "Date")
            rv$tables[[ind]][[col]] <- rep("", dim(.table)[1])
            if(isTruthy(.dat.row))
              rv$tables[[ind]][.dat.row, col] <- rep(main.env$FORMATS$dates[3], length(.dat.row))
          }
          if (col == "unit") {
            .uni.row <- which(rv$tables[[ind]]$class == "numeric")
            rv$tables[[ind]][[col]] <- rep("", dim(.table)[1])
            if(isTruthy(.uni.row))
              rv$tables[[ind]][.uni.row, col] <- rep(main.env$FORMATS$dates[2], length(.uni.row))
          }
          
          # Update values
          if (ind == rv$current.file) {
            sapply(1:dim(rv$tables[[ind]])[1], function(item) {
              inputId <- paste(ind, item, col, sep = "-")
              if (inputId %in% names(input)) {
                if (col %in% c("unit", "dateTimeFormatString")) {
                  updateSelectInput(session, inputId, selected = rv$tables[[ind]][item, col])
                }
                if (col %in% c("attributeDefinition", "missingValueCode", "missingValueCodeExplanation")) {
                  updateTextAreaInput(session, inputId, value = rv$tables[[ind]][item, col])
                }
              }
            })
          }
        }) # end of sapply
        
        # Update current table
        if (ind == rv$current.file) {
          rv$current.table <- rv$tables[[ind]]
        }
      }) # end of lapply
      return(rv)
    } # end of .fill
  }
  
  # variable initialization -----------------------------------------------------
  rv <- reactiveValues(
    data.filepath = savevar$emlal$DataFiles$datapath,
    filepath = savevar$emlal$DataFiles$metadatapath,
    filenames = basename(savevar$emlal$DataFiles$metadatapath),
    current.file = 1,
    tables = NULL,
    current.table = NULL,
    current.preview = NULL,
    cu.table = data.frame(stringsAsFactors = FALSE),
    cu.values = rep(NA, 5),
    modal.on = FALSE,
    unit.id = character(),
    units.list = character(),
    annotations = reactiveValues(
      values = data.frame(stringsAsFactors = FALSE),
      count = 0
    )
  )
  rv$tables <- lapply(
    rv$filepath, readDataTable,
    data.table = FALSE, stringsAsFactors = FALSE
  )
  rv$current.table <- rv$tables[[rv$current.file]]
  rv$cu.table <- readDataTable(
    dir(
      savevar$emlal$SelectDP$dp.metadata.path,
      pattern = "ustom",
      full.names = TRUE
    ),
    stringsAsFactors = FALSE,
  )
  rv$units.list <- main.env$FORMATS$units
  if (checkTruth(savevar$emlal$Attributes$annotations)) {
    rv$annotations$values <- savevar$emlal$Attributes$annotations
    rv$annotations$count <- nrow(rv$annotations$values)
  }
  else {
    rv$annotations$values <- data.frame(
      id = character(),
      element = character(),
      context = character(),
      subject = character(),
      predicate_label = character(),
      predicate_uri = character(),
      object_label = character(),
      object_uri = character()
    )
  }
  
  if (isTRUE(savevar$emlal$quick)) {
    rv <- .fill(rv)
  }
  
  observe(
    {
      . <- rv$current.table # trigger
      
      .tmp <- unique(c(
        unlist(lapply(seq_along(rv$tables), function(t) {
          .tab <- if (t == rv$current.file) {
            rv$current.table
          } else {
            rv$tables[[t]]
          }
          .tab$unit
        })),
        main.env$FORMATS$units
      ))
      rv$units.list <- .tmp[.tmp != ""]
    },
    label = "EAL3: unit list"
  )
  
  # Reactive triggers
  curt <- makeReactiveTrigger()
  
  # List of observers
  obs <- reactiveValues()
  
  # Navigation buttons -----------------------------------------------------
  onclick("file_prev", {
    req(rv$current.file > 1)
    # Save
    if (!is.null(rv$current.table)) {
      rv$tables[[rv$current.file]] <- rv$current.table
    }
    # Change file
    rv$current.file <- rv$current.file - 1
  })
  
  onclick("file_next", {
    req(rv$current.file < length(rv$filenames))
    # Save
    if (!is.null(rv$current.table)) {
      rv$tables[[rv$current.file]] <- rv$current.table
    }
    # Change file
    rv$current.file <- rv$current.file + 1
  })
  
  # update table
  observeEvent(rv$current.file,
    {
      req(rv$current.file > 0)
      rv$current.table <- rv$tables[[rv$current.file]]
      rv$current.table[is.na(rv$current.table)] <- ""
      rv$current.preview <- readDataTable(
        rv$data.filepath[rv$current.file],
        stringsAsFactors = FALSE,
        nrows = 5
      )
    },
    priority = 1
  )
  
  # display
  output$current_file <- renderUI(
    tags$div(
      rv$filenames[rv$current.file],
      class = "ellipsis",
      style = paste0(
        "display: inline-block;
        font-size:14pt;
        text-align:center;
        width:100%;
        background: linear-gradient(90deg, #3c8dbc ",
        round(100 * rv$current.file / length(rv$filenames)),
        "%, white ",
        round(100 * rv$current.file / length(rv$filenames)),
        "%);"
      )
    )
  )
  
  # * UI -----------------------------------------------------
  observeEvent(rv$current.file, {
    req(checkTruth(rv$current.table))
    .current.table <- isolate(rv$current.table)
    
    output$edit_attributes <- renderUI({
      # validity check
      validate(
        need(
          !identical(.current.table, data.frame()) &&
            isTruthy(.current.table),
          "No valid attributes table."
        )
      )
      
      ui <- do.call(
        bsCollapse,
        args = c(
          lapply(
            seq(dim(.current.table)[1]), # rows
            fields = colnames(.current.table),
            function(row_index, fields) {
              # prepare variables
              attribute.row <- .current.table[row_index, ]
              
              return(
                bsCollapsePanel(
                  title = attribute.row[fields[1]],
                  tagList(
                    column(
                      9,
                      # Input ====
                      lapply(fields[-1], function(colname) {
                        # prepare var
                        saved.value <- .current.table[row_index, colname]
                        inputId <- paste(
                          isolate(rv$current.file),
                          row_index,
                          sep = "-"
                        )
                        
                        # GUI
                        attributeInputUI(
                          ns(inputId),
                          colname,
                          saved.value,
                          main.env$FORMATS,
                          rv
                        )
                      }) # end of lapply colname
                    ),
                    column(
                      3,
                      # Preview ====
                      h4("Preview:"),
                      tableOutput(
                        ns(paste0(
                          "preview-", 
                          colnames(rv$current.preview)[row_index]
                        ))
                      ),
                      tags$hr(),
                      # Annotate ====
                      # tags$div(
                      #   annotateUI(
                      #     ns(paste(
                      #       "annotate",
                      #       isolate(rv$current.file),
                      #       row_index,
                      #       sep = "-"
                      #     ))
                      #   ),
                      #   class = "inputbox wip"
                      # )
                    ) # end of column
                  )
                ) # end of bsCollapsePanel
              ) # end of return
            }
          ), # end of lapply : row_index
          id = ns("collapse")
        )
      )
      return(ui)
    })
  }) # end of observeEvent
  
  # * Server -----------------------------------------------------
  observeEvent(rv$current.file, {
    req(checkTruth(rv$current.table))
    
    sapply(
      seq(dim(rv$current.table)[1]),
      fields = colnames(rv$current.table)[-1], # not Attribute Name
      function(row.index, fields) {
        
        # TODO Update style: to correct
        #   updateCollapse(
        #     session = session,
        #     ns("collapse"),
        #     style = filled
        #   )
        
        # Preview ====
        preview.column <- colnames(rv$current.preview)[row_index]
        output[[paste0("preview-", preview.column)]] <- renderTable(rv$current.preview[[preview.column]])
        
        # Annotate ====
        # annotateId <- paste(
        #   "annotate",
        #   isolate(rv$current.file),
        #   row.index,
        #   sep = "-"
        # )
        # 
        # .tmp <- callModule(
        #   annotate, annotateId,
        #   savevar, main.env, rv, row.index
        # )
        
        # Input ====
        lapply(fields, function(colname) {
          inputId <- paste(
            isolate(rv$current.file),
            row.index,
            sep = "-"
          )
          
          obs <- callModule(
            attributeInput,
            inputId,
            rv,
            row.index,
            colname,
            obs,
            curt
          )
        }) # end of lapply colname
      } # end of *in situ* function
    ) # end of sapply : row_index
  }) # end of observeEvent
  
  # Custom units ----
  observe({
    curt$depend()
    .current.table <- isolate(rv$current.table)
    modal.on <- isolate(rv$modal.on)
    req(any(.current.table$unit == "custom"))
    
    rv$unit.id <- c(
      rv$current.file,
      which(.current.table$unit == "custom"),
      "unit"
    )
    
    row <- rv$unit.id[2]
    class <- .current.table[row, "class"]
    
    if (class == "numeric" &&
        modal.on == FALSE) {
      rv$cu.values <- rv$cu.table %>%
        filter(grepl(class, id))
      if (any(dim(rv$cu.values) == 0)) {
        rv$cu.values <- rep(NA, 5)
      }
      
      showModal(CU_Modal(rv$cu.values, cu.table = rv$cu.table))
      
      rv$modal.on <- TRUE
      
      isolate({
        rv$current.table[row, "unit"] <- ""
      })
    }
  })
  
  CU_Modal <- function(values = rep(NA, 5), cu.table = NULL) {
    modalDialog(
      title = "Custom Unit",
      tagList(
        # id
        fluidRow(
          column(6,
            offset = 3,
            textInput(
              ns("modal_id"),
              label = with_red_star("Unit identifier"),
              placeholder = "e.g. milligramsPerGram",
              value = if (!is.na(values[1])) values[1] else NULL
            ),
            # unitType
            textInput(
              ns("modal_unitType"),
              label = with_red_star("Physical property types the unit belongs to"),
              placeholder = "e.g. mass",
              value = if (!is.na(values[2])) values[2] else NULL
            ),
            # ParentSI
            selectInput(
              ns("modal_parentSI"),
              label = with_red_star("Parent unit in SI"),
              choices = main.env$FORMATS$units[-1],
              selected = if (!is.na(values[3])) values[3] else NULL
            ),
            # MultiplierToSI
            numericInput(
              ns("modal_multiplier"),
              label = with_red_star("Numeric multiplier computed from Parent unit in SI"),
              value = 1,
            ),
            # Description
            textAreaInput(
              ns("modal_description"),
              label = with_red_star("Unit description"),
              placeholder = "e.g. milligrams per gram",
              value = if (!is.na(values[5])) values[5] else NULL
            )
          )
        ) # end of fluidRow
      ),
      easyClose = FALSE,
      footer = tagList(
        actionButton(ns("modal_cancel"), "Cancel"),
        actionButton(ns("modal_submit"), "Submit")
      )
    )
  }
  
  # * CU server ----
  # Cancel
  onclick("modal_cancel", {
    req(isTRUE(rv$modal.on))
    
    # Close modal
    rv$modal.on <- FALSE
    removeModal()
    
    isolate({
      updateSelectInput(
        session,
        paste(rv$unit.id, collapse = "-"),
        selected = main.env$FORMATS$units[2]
      )
    })
    rv$unit.id <- character() # reset to default
  })
  
  # Submit button en/disable
  observe({
    req(isTRUE(rv$modal.on))
    
    # type a new one
    if (!input$modal_id %in% rv$cu.table$id &&
        input$modal_id != "custom" &&
        isTruthy(input$modal_id) &&
        isTruthy(input$modal_unitType) &&
        isTruthy(input$modal_parentSI) &&
        isTruthy(input$modal_multiplier) &&
        isTruthy(input$modal_description)) {
      shinyjs::enable("modal_submit")
    } else {
      shinyjs::disable("modal_submit")
    }
  })
  
  # Submit
  observeEvent(input$modal_submit,
    {
      req(isTRUE(rv$modal.on))
      
      # Close modal
      removeModal()
      rv$modal.on <- FALSE
      
      isolate({
        rv$cu.values <- c(
          input$modal_id,
          input$modal_unitType,
          input$modal_parentSI,
          input$modal_multiplier,
          input$modal_description
        )
      })
      
      # Update CU values
      if (rv$cu.values[1] %in% rv$cu.table$id) {
        rv$cu.table <- rv$cu.table %>%
          filter(id = rv$cu.values[1]) %>%
          replace(values = rv$cu.values)
      } # Add CU values
      else {
        names(rv$cu.values) <- colnames(rv$cu.table)
        rv$cu.table[dim(rv$cu.table)[1] + 1,] <- rv$cu.values
      }
      # update input UI
      rv$units.list <- unique(c(
        rv$cu.values["id"],
        rv$units.list
      ))
      isolate({
        updateSelectInput(
          session,
          paste(rv$unit.id, collapse = "-"),
          choices = rv$units.list,
          selected = rv$cu.values["id"]
        )
      })
      
      row <- rv$unit.id[2]
      rv$current.table[row, "unit"] <- rv$cu.values["id"]
    },
    priority = 1
  )
  
  output$CUUI <- renderTable({
    validate(
      need(isTruthy(unlist(rv$cu.table)), "No custom units registered")
    )
    return(rv$cu.table)
  })
  
  # Saves -----------------------------------------------------
  # observeEvent(rv$tables, {
  observe(
    {
      main.env$EAL$current[2] <- FALSE
      req(
        length(rv$tables) != 0 &&
          !any(sapply(rv$tables, identical, y = data.frame()))
      )
      
      main.env$EAL$current[2] <- all(
        unlist(
          lapply(
            rv$tables,
            function(table) {
              isTruthy(table) &&
                all(sapply(table$attributeName, isTruthy)) &&
                all(sapply(table$attributeDefinition, isTruthy)) &&
                all(sapply(table$class, isTruthy)) &&
                !any(grepl("!Add.*here!", table$unit)) &&
                !any(grepl("!Add.*here!", table$dateTimeFormatString))
            }
          ) # lapply
        ) # unlist
      ) # all
    },
    priority = -1
  )
  
  observeEvent(NSB$SAVE,
    {
      req(tail(main.env$EAL$history, 1) == "Attributes")
      
      savevar <- saveReactive(
        savevar = savevar,
        rv = list(Attributes = rv)
      )
    },
    label = "Save_Attributes",
    ignoreInit = TRUE
  )
  
  # en/disable buttons
  observeEvent(rv$current.file, {
    req(
      isTruthy(names(input)) &&
        isTruthy(names(rv$current.file))
    )
    
    if (rv$current.file == 1) {
      shinyjs::disable("file_prev")
    } else {
      shinyjs::enable("file_prev")
    }
    if (rv$current.file == length(rv$filenames)) {
      shinyjs::disable("file_next")
    } else {
      shinyjs::enable("file_next")
    }
  })
  
  # Process data -----------------------------------------------------
  observeEvent(NSB$NEXT,
    {
      req(main.env$EAL$current[1] == "Attributes")
      
      withProgress({
        setProgress(0.5, "Saving metadata")
        
        savevar <- saveReactive(
          savevar = savevar,
          rv = list(Attributes = rv)
        )
        
        # for each attribute data frame
        setProgress(0.8, "Resolving catvar templates")
        .do.template.catvars <- sapply(
          seq_along(rv$filenames),
          function(cur_ind) {
            
            # check for direction: CustomUnits or CatVars
            return(isTRUE("categorical" %in% rv$tables[[cur_ind]][, "class"]))
          }
        ) %>%
          unlist() %>%
          any()
        
        # EMLAL: template new fields if needed
        if (isTRUE(.do.template.catvars)) {
          try(
            EMLassemblyline::template_categorical_variables(
              path = savevar$emlal$SelectDP$dp.metadata.path,
              data.path = savevar$emlal$SelectDP$dp.data.path
            )
          )
        }
        
        setProgress(0.9, "Templating geographic coverage")
        try(
          EMLassemblyline::template_geographic_coverage(
            path = savevar$emlal$SelectDP$dp.metadata.path,
            data.path = savevar$emlal$SelectDP$dp.data.path,
            empty = TRUE,
            write.file = TRUE
          )
        )
        
        if (isFALSE(.do.template.catvars)) {
          isolate(main.env$EAL$navigate <- main.env$EAL$navigate + 1)
        }
        incProgress(0.1)
      })
    },
    priority = 1,
    ignoreInit = TRUE
  )
  
  # Output -----------------------------------------------------
  return(savevar)
}
