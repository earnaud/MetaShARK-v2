#' @title Data Package Template filling
#'
#' @description UI part of the Attributes module. Fill in the attributes of the data package
#'
#' @importFrom shiny NS fluidPage fluidRow column tags tagList HTML actionButton icon uiOutput
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
        column(12,
          uiOutput(ns("edit_attributes")) %>% withSpinner(color = "#599cd4")
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
#' @importFrom shiny observeEvent req reactiveValues observe renderUI tags callModule textAreaInput HTML selectInput tagList
#' observe eventReactive validate
#' @importFrom shinyjs hide show enable disable onclick
#' @importFrom EMLassemblyline template_categorical_variables template_geographic_coverage
#' @importFrom shinyBS bsCollapse bsCollapsePanel updateCollapse
Attributes <- function(input, output, session, savevar, globals, NSB) {
  ns <- session$ns
  
  if (globals$dev) {
    onclick("dev", {
      req(globals$EMLAL$NAVIGATE == 3)
      browser()
    }, asis=TRUE)
  }
  
  if (globals$dev || isTRUE(savevar$emlal$quick)) {
    .fill <- function(rv = rv) {
      lapply(seq(rv$tables), function(ind) {
        table <- rv$tables[[ind]]
        sapply(colnames(table), function(col) {
          # Set values
          if (col == "attributeDefinition") {
            rv$tables[[ind]][[col]] <- paste("Description for", rv$tables[[ind]][["attributeName"]])
          }
          if (col %in% c("missingValueCodeExplanation", "missingValueCode")) {
            rv$tables[[ind]][[col]] <- rep("LoremIpsum", dim(table)[1])
          }
          if (col == "dateTimeFormatString") {
            dat_row <- which(rv$tables[[ind]]$class == "Date")
            rv$tables[[ind]][dat_row, col] <- rep(globals$FORMAT$DATE[3], length(dat_row))
          }
          if (col == "unit") {
            uni_row <- which(rv$tables[[ind]]$class == "numeric")
            rv$tables[[ind]][uni_row, col] <- rep(globals$FORMAT$UNIT[2], length(uni_row))
          }
          
          # Update values
          if (ind == rv$current_file) {
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
        if (ind == rv$current_file) {
          rv$current_table <- rv$tables[[ind]]
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
    current_file = 1,
    tables = NULL,
    current_table = NULL,
    current_preview = NULL,
    CU_Table = data.frame(),
    cu_values = rep(NA,5),
    modalOn = FALSE,
    unitId = character(),
    unitList = character()
    , annotations = reactiveValues(
      values = data.frame(),
      count = 0
    )
  )
  rv$tables <- lapply(
    rv$filepath, readDataTable,
    data.table = FALSE, stringsAsFactors = FALSE
  )
  rv$current_table <- rv$tables[[rv$current_file]]
  rv$CU_Table <- readDataTable(
    dir(savevar$emlal$SelectDP$dp_metadata_path, pattern = "ustom", full.names = TRUE),
    stringsAsFactors = FALSE,
  )
  rv$unitList <- globals$FORMAT$UNIT
  if(checkTruth(savevar$emlal$Attributes)){
    rv$annotations$values <- savevar$emlal$Attributes$annotations
    rv$annotations$count <- nrow(rv$annotations$values)
  }
  else
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
  
  if (isTRUE(savevar$emlal$quick)) {
    rv <- .fill(rv)
  }
  
  observe({
    . <- rv$current_table # trigger
    
    .tmp <- unique(c(
      unlist(lapply(seq_along(rv$tables), function(t) {
        .tab <- if(t == rv$current_file)
          rv$current_table
        else
          rv$tables[[t]]
        .tab$unit
      })),
      # rv$CU_Table$id,
      globals$FORMAT$UNIT
    ))
    rv$unitList <- .tmp[.tmp != ""]
  }, label = "EAL3: unit list")
  
  # Reactive triggers
  curt <- makeReactiveTrigger()
  
  # List of observers
  obs <- reactiveValues()
  
  # Navigation buttons -----------------------------------------------------
  onclick("file_prev", {
    req(rv$current_file > 1)
    # Save
    if (!is.null(rv$current_table)) {
      rv$tables[[rv$current_file]] <- rv$current_table
    }
    # Change file
    rv$current_file <- rv$current_file - 1
  })
  
  onclick("file_next", {
    req(rv$current_file < length(rv$filenames))
    # Save
    if (!is.null(rv$current_table)) {
      rv$tables[[rv$current_file]] <- rv$current_table
    }
    # Change file
    rv$current_file <- rv$current_file + 1
  })
  
  # update table
  observeEvent(rv$current_file,{
    req(rv$current_file > 0)
    rv$current_table <- rv$tables[[rv$current_file]]
    rv$current_table[is.na(rv$current_table)] <- ""
    rv$current_preview <- readDataTable(
      rv$data.filepath[rv$current_file],
      stringsAsFactors = FALSE,
      nrows = 5
    )
  }, priority = 1)
  
  # display
  output$current_file <- renderUI(
    tags$div(
      h4(rv$filenames[rv$current_file]),
      style = paste0(
        "background: linear-gradient(90deg, #3c8dbc ",
        round(100 * rv$currentIndex / length(rv$filenames)),
        "%, white ",
        round(100 * rv$currentIndex / length(rv$filenames)),
        "%);"
      ),
      class = "ellipsis text-title"
    )
  )
  
  # * UI -----------------------------------------------------
  observeEvent(rv$current_file, {
    req(checkTruth(rv$current_table))
    current_table <- isolate(rv$current_table)
    
    output$edit_attributes <- renderUI({
      # validity check
      validate(
        need(
          !identical(current_table, data.frame()) &&
            isTruthy(current_table),
          "No valid attributes table."
        )
      )
      
      ui <- do.call(
        bsCollapse,
        args = c(
          lapply(
            seq(dim(current_table)[1]), # rows
            fields = colnames(current_table),
            function(row_index, fields) {
              # prepare variables
              attribute_row <- current_table[row_index, ]
              
              return(
                bsCollapsePanel(
                  title = attribute_row[fields[1]],
                  tagList(
                    column(9,
                      # Input ====
                      lapply(fields[-1], function(colname) {
                        # prepare var
                        saved_value <- isolate(rv$current_table[row_index, colname])
                        inputId <- paste(
                          isolate(rv$current_file),
                          row_index,
                          sep = "-"
                        )
                        
                        # GUI
                        attributeInputUI(
                          ns(inputId), 
                          colname, 
                          saved_value,
                          globals$FORMAT,
                          # rv$unitList,
                          # rv$CU_TABLE$id
                          rv
                        )
                        # {
                          # switch(colname,
                          #   attributeDefinition = textAreaInput(
                          #     ns(inputId),
                          #     value = saved_value,
                          #     with_red_star("Describe the attribute")
                          #   ),
                          #   class = selectInput(
                          #     ns(inputId),
                          #     "Dectected class (change if misdetected)",
                          #     choices = c("numeric", "character", "Date", "categorical"),
                          #     selected = saved_value
                          #   ),
                          #   unit = {
                          #     tmp <- selectInput(
                          #       ns(inputId),
                          #       with_red_star("Select an unit"),
                          #       unique(c(
                          #         saved_value,
                          #         as.character(rv$CU_Table$id),
                          #         globals$FORMAT$UNIT
                          #       )),
                          #       selected = if (isTruthy(saved_value)) saved_value
                          #     )
                          #     if (isTruthy(saved_value))
                          #       tmp
                          #     else
                          #       hidden(tmp)
                          #   },
                          #   dateTimeFormatString = {
                          #     tmp <- selectInput( # TODO add a module for hour format
                          #       ns(inputId),
                          #       with_red_star("Select a date format"),
                          #       unique(c(saved_value, globals$FORMAT$DATE)),
                          #       selected = saved_value
                          #     )
                          #     if (isTruthy(saved_value))
                          #       tmp
                          #     else
                          #       hidden(tmp)
                          #   },
                          #   missingValueCode = textInput(
                          #     ns(inputId),
                          #     "Code for missing value (max 1 word)",
                          #     value = saved_value
                          #   ),
                          #   missingValueCodeExplanation = textAreaInput(
                          #     ns(inputId),
                          #     "Explain Missing Values",
                          #     value = saved_value
                          #   ),
                          #   NULL
                          # ) # end of switch
                        # }
                        
                      }) # end of lapply colname
                    ),
                    column(3,
                      # Preview ====
                      h4("Preview:"),
                      tableOutput(ns(paste0("preview-", colnames(rv$current_preview)[row_index]))),
                      tags$hr(),
                      # Annotate ====
                      annotateUI(
                        ns(paste(
                          "annotate",
                          isolate(rv$current_file),
                          row_index,
                          sep = "-"
                        ))
                      )
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
  observeEvent(rv$current_file, {
    req(checkTruth(rv$current_table))
    
    sapply(
      seq(dim(rv$current_table)[1]),
      fields = colnames(rv$current_table)[-1], # not Attribute Name
      function(row_index, fields) {
        
        # TODO Update style: to correct
        #   updateCollapse(
        #     session = session,
        #     ns("collapse"),
        #     style = filled
        #   )
        
        # Preview ====
        preview_column <- colnames(rv$current_preview)[row_index]
        output[[paste0("preview-", preview_column)]] <- renderTable(rv$current_preview[[preview_column]])
        
        # Annotate ====
        annotateId <- paste(
          "annotate",
          isolate(rv$current_file),
          row_index,
          sep = "-"
        )

        .tmp <- callModule(annotate, annotateId,
          savevar, globals, rv, row_index)
        
        # Input ====
        lapply(fields, function(colname) {
          inputId <- paste(
            isolate(rv$current_file),
            row_index,
            sep = "-"
          )
          
          obs <- callModule(
            attributeInput,
            inputId,
            rv,
            row_index,
            colname,
            obs,
            curt
          )
          # # obs[[inputId]] <- observeEvent(input[[inputId]], {
          #   req(input[[inputId]])
          #   if(grepl("class", inputId)){ # input: class
          #     # Date
          #     date_id <- paste(
          #       isolate(rv$current_file),
          #       row_index, 
          #       "dateTimeFormatString",
          #       sep="-"
          #     )
          #     if(input[[inputId]] == "Date")
          #       show(date_id)
          #     else {
          #       isolate(rv$current_table[row_index, "dateTimeFormatString"] <- "")
          #       hide(date_id)
          #     }
          #     
          #     # Unit
          #     unit_id <- paste(
          #       isolate(rv$current_file),
          #       row_index, 
          #       "unit",
          #       sep="-"
          #     )
          #     if(input[[inputId]] == "numeric")
          #       show(unit_id)
          #     else{
          #       isolate(rv$current_table[row_index, "unit"] <- "")
          #       hide(unit_id)
          #     }
          #     
          #     rv$current_table[row_index, colname] <- input[[inputId]]
          #   } else if (grepl("missingValueCode$", inputId)) { # input: missing Value code
          #     if (grepl(".+ +.*", input[[inputId]])) {
          #       val <- gsub("^ +", "", input[[inputId]])
          #       updateTextInput(
          #         session,
          #         inputId,
          #         value = strsplit(val, " ")[[1]][1]
          #       )
          #       showNotification(
          #         id = session$ns("mvc_update"),
          #         ui = HTML("<code>missingValueCode</code> fields are limited to a <b>single word.</b>"),
          #         duration = 3,
          #         type = "warning"
          #       )
          #     }
          #     
          #     rv$current_table[row_index, colname] <- strsplit(input[[inputId]], " ")[[1]][1]
          #   } else { # input: others
          #     if(grepl("unit", inputId)){
          #       # Trigger CU
          #       if(input[[inputId]] == "custom" &&
          #         isFALSE(rv$modalOn)){
          #         isolate(rv$unitId <- c(
          #           isolate(rv$current_file),
          #           row_index,
          #           colname
          #         ))
          #         curt$trigger()
          #       } else
          #       # Remove CU
          #       if(!input[[inputId]] %in% rv$CU_Table$id &&
          #           checkTruth(rv$CU_Table$id)){
          #         rv$CU_Table <- rv$CU_Table %>% 
          #           slice(which(rv$CU_Table$id %in% rv$current_table$unit[-row_index]))
          #       }
          #     }
          #     # isolate(
          #       rv$current_table[row_index, colname] <- input[[inputId]]
          #     # )
          #   }
          #   rv$tables[[rv$current_file]] <- rv$current_table
          # }) # end of inner observeEvent
          
        }) # end of lapply colname
        
      } # end of *in situ* function
    ) # end of sapply : row_index
  }) # end of observeEvent
  
  # Custom units ----
  observe({
    curt$depend()
    df <- isolate(rv$current_table)
    modalOn <- isolate(rv$modalOn)
    req(any(df$unit == "custom"))
    
    rv$unitId <- c(
      rv$current_file,
      which(df$unit == "custom"),
      "unit"
    )
    
    row <- rv$unitId[2]
    class <- df[row,"class"]
    
    if(class == "numeric" && 
        modalOn == FALSE){
      rv$cu_values <- rv$CU_Table %>%
        filter(grepl(class, id))
      if(any(dim(rv$cu_values) == 0))
        rv$cu_values <- rep(NA, 5)
      
      showModal(CU_Modal(rv$cu_values, CU_Table = rv$CU_Table))
      
      rv$modalOn <- TRUE
      
      isolate({
        rv$current_table[row, "unit"] <- ""
      })
    }
  })
  
  CU_Modal <- function(values = rep(NA,5), CU_Table = NULL){
    modalDialog(
      title = "Custom Unit",
      tagList(
        # id
        fluidRow(
          column(6, offset = 3,
            textInput(
              ns("modal_id"),
              label = with_red_star("Unit identifier"),
              placeholder = "e.g. milligramsPerGram",
              value = if(!is.na(values[1])) values[1] else NULL
            ),
            # unitType
            textInput(
              ns("modal_unitType"),
              label = with_red_star("Physical property types the unit belongs to"),
              placeholder = "e.g. mass",
              value = if(!is.na(values[2])) values[2] else NULL
            ),
            # ParentSI
            selectInput(
              ns("modal_parentSI"),
              label = with_red_star("Parent unit in SI"),
              choices = globals$FORMAT$UNIT[-1],
              selected = if(!is.na(values[3])) values[3] else NULL
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
              value = if(!is.na(values[5])) values[5] else NULL
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
    req(isTRUE(rv$modalOn))
    
    # Close modal
    rv$modalOn <- FALSE
    removeModal()
    
    isolate({
      updateSelectInput(
        session,
        paste(rv$unitId, collapse = "-"),
        selected = globals$FORMAT$UNIT[2]
      )
    })
    rv$unitId <- character() # reset to default
  })
  
  # Submit button en/disable
  observe({
    req(isTRUE(rv$modalOn))
    
    # type a new one
    if(!input$modal_id %in% rv$CU_Table$id &&
        input$modal_id != "custom" &&
        isTruthy(input$modal_id)  &&
        isTruthy(input$modal_unitType) &&
        isTruthy(input$modal_parentSI) &&
        isTruthy(input$modal_multiplier) &&
        isTruthy(input$modal_description)) {
      enable("modal_submit")
    } else
      disable("modal_submit")
  })
  
  # Submit
  observeEvent(input$modal_submit, {
    req(isTRUE(rv$modalOn))
    
    # Close modal
    removeModal()
    rv$modalOn <- FALSE
    
    isolate({
      rv$cu_values <- c(
        input$modal_id,
        input$modal_unitType,
        input$modal_parentSI,
        input$modal_multiplier,
        input$modal_description
      )
    })
      
    # Update CU values
    if(rv$cu_values[1] %in% rv$CU_Table$id)
      rv$CU_Table <- rv$CU_Table %>% 
        filter(id = rv$cu_values[1]) %>% 
        replace(values = rv$cu_values)
    # Add CU values
    else{
      names(rv$cu_values) <- colnames(rv$CU_Table) 
      rv$CU_Table[dim(rv$CU_Table)[1]+1,] <- rv$cu_values
    }
    # update input UI
    rv$unitList <- unique(c(
      rv$cu_values["id"],
      rv$unitList
    ))
    isolate({
      updateSelectInput(
        session,
        paste(rv$unitId, collapse = "-"),
        choices = rv$unitList,
        selected = rv$cu_values["id"]
      )
    })
    
    row <- rv$unitId[2]
    rv$current_table[row, "unit"] <- rv$cu_values["id"]
  }, priority = 1)

  output$CUUI <- renderTable({
    validate(
      need(isTruthy(unlist(rv$CU_Table)), "No custom units registered")
    )
    return(rv$CU_Table)
  })
  
  # Saves -----------------------------------------------------
  # observeEvent(rv$tables, {
  observe({
    globals$EMLAL$COMPLETE_CURRENT <- FALSE
    req(
      length(rv$tables) != 0 &&
        !any(sapply(rv$tables, identical, y = data.frame()))
    )
    
    globals$EMLAL$COMPLETE_CURRENT <- all(
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
  
  observeEvent(NSB$SAVE, {
    req(tail(globals$EMLAL$HISTORY,1) == "Attributes")
    
    message("NSB$SAVE")
    
    savevar <- saveReactive(
      savevar = savevar,
      rv = list(Attributes = rv)
    )
  }, 
    label = "Save_Attributes",
    ignoreInit = TRUE
  )
  
  # en/disable buttons
  observeEvent(rv$current_file, {
    req(
      isTruthy(names(input)) &&
        isTruthy(names(rv$current_file))
    )
    
    if (rv$current_file == 1) {
      disable("file_prev")
    } else {
      enable("file_prev")
    }
    if (rv$current_file == length(rv$filenames)) {
      disable("file_next")
    } else {
      enable("file_next")
    }
  })
  
  # Process data -----------------------------------------------------
  observeEvent(NSB$NEXT, {
    req(globals$EMLAL$CURRENT == "Attributes")

    withProgress({
      setProgress(0.5, "Saving metadata")
      
      savevar <- saveReactive(
        savevar = savevar, 
        rv = list(Attributes = rv)
      )
      
      # for each attribute data frame
      setProgress(0.8, "Resolving catvar templates")
      templateCatvars <- sapply(
        seq_along(rv$filenames),
        function(cur_ind) {
          
          # check for direction: CustomUnits or CatVars
          return(isTRUE("categorical" %in% rv$tables[[cur_ind]][, "class"]))
        }
      ) %>% unlist %>% any
      
      # EMLAL: template new fields if needed
      if (isTRUE(templateCatvars)) {
        try(
          template_categorical_variables(
            path = savevar$emlal$SelectDP$dp_metadata_path,
            data.path = savevar$emlal$SelectDP$dp_data_path
          )
        )
      }
      
      setProgress(0.9, "Templating geographic coverage")
      try(
        template_geographic_coverage(
          path = savevar$emlal$SelectDP$dp_metadata_path,
          data.path = savevar$emlal$SelectDP$dp_data_path,
          empty = TRUE,
          write.file = TRUE
        )
      )
      
      if(isFALSE(templateCatvars))
        isolate(globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + 1)
      incProgress(0.1)
    })
  }, priority = 1, ignoreInit = TRUE)
  
  # Output -----------------------------------------------------
  return(savevar)
}

