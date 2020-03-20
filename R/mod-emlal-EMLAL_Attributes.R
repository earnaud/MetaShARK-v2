#' @title Data Package Template filling
#'
#' @description UI part of the Attributes module. Fill in the attributes of the data package
#'
#' @importFrom shiny NS fluidPage fluidRow column tags tagList HTML actionButton icon uiOutput
AttributesUI <- function(id, title, dev) {
  ns <- NS(id)
  
  return(
    fluidPage(
      # Inputs -----------------------------------------------------
      column(
        10,
        tags$h4("Data table attributes"),
        tagList("Even if EML Assembly Line automatically infers most
              of your data's metadata, some steps need you to check
              out. Please check the following attribute, and fill
              in at least the",
          with_red_star("mandatory elements.")
        ),
        fluidRow(
          column(1,
            actionButton(
              ns("file_prev"),
              "",
              icon("chevron-left")
            )
          ),
          column(10,
            uiOutput(ns("current_file"),
              inline = TRUE
            )
          ),
          column(1,
            actionButton(
              ns("file_next"),
              "",
              icon("chevron-right")
            )
          )
        ),
        fluidRow(
          uiOutput(ns("edit_attributes"))
        )
      ), # end of column
      
      # NSB -----------------------------------------------------
      column(
        2,
        navSidebar(
          ns("nav"),
          ... = tagList(
            if (dev) actionButton(ns("dev"), "Dev Check"),
            if (dev) actionButton(ns("fill"), "Fill")
          )
        )
      ) # end column 2
    ) # end fluidPage
  ) # end return
}

#' @describeIn AttributesUI
#'
#' @description server part of the Attributes module.
#'
#' @importFrom data.table fread fwrite
#' @importFrom shiny observeEvent req reactiveValues observe renderUI tags callModule textAreaInput HTML selectInput tagList
#' observe eventReactive validate
#' @importFrom shinyjs hide show enable disable
#' @importFrom EMLassemblyline template_categorical_variables template_geographic_coverage
#' @importFrom shinyBS bsCollapse bsCollapsePanel updateCollapse
Attributes <- function(input, output, session, savevar, globals) {
  ns <- session$ns
  
  if (globals$dev || isTRUE(savevar$emlal$quick)) {
    .fill <- function(rv = rv){
      lapply(seq(rv$tables), function(ind){
        table <- rv$tables[[ind]]
        sapply(colnames(table), function(col){
          # Set values
          if (col == "attributeDefinition"){
            rv$tables[[ind]][[col]] <- paste("Description for", rv$tables[[ind]][["attributeName"]])
          }
          if (col %in% c("missingValueCodeExplanation",  "missingValueCode")){
            rv$tables[[ind]][[col]] <- rep("Lorem Ipsum", dim(table)[1])
          }
          if (col == "dateTimeFormatString"){
            dat_row <- which(rv$tables[[ind]]$class == "Date")
            rv$tables[[ind]][dat_row, col] <- rep(globals$FORMAT$DATE[3], length(dat_row))
          }
          if (col == "unit"){
            uni_row <- which(rv$tables[[ind]]$class == "numeric")
            rv$tables[[ind]][uni_row, col] <- rep(globals$FORMAT$UNIT[1], length(uni_row))
          }
          
          # Update values
          if(ind == rv$current_file){
            sapply(1:dim(rv$tables[[ind]])[1], function(item){
              inputId <- paste(ind, item, col, sep = "-")
              if(inputId %in% names(input)){
                if(col %in% c("unit", "dateTimeFormatString"))
                  updateSelectInput(session, inputId, selected = rv$tables[[ind]][item,col])
                if(col %in% c("attributeDefinition", "missingValueCode", "missingValueCodeExplanation"))
                  updateTextAreaInput(session, inputId, value = rv$tables[[ind]][item,col])
              }
            })
          }
        }) # end of sapply
        
        # Update current table
        if(ind == rv$current_file)
          rv$current_table <- rv$tables[[ind]]
        
      }) # end of lapply
      return(rv)
    } # end of .fill
  }
  
  # DEV 
  if (globals$dev) {
    observeEvent(input$fill, {
      req(input$fill)
      rv <- .fill(rv)
    }) # end of observeEvent
    
    observeEvent(input$dev, {browser()})
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
    complete = FALSE
  )
  rv$tables <- lapply(
    rv$filepath,
    fread,
    data.table = FALSE,
    stringsAsFactors = FALSE
  )
  
  if(isTRUE(savevar$emlal$quick)){
    rv <- .fill(rv)
  }
  
  # Navigation buttons -----------------------------------------------------
  # Previous
  observeEvent(input$file_prev, {
    req(rv$current_file > 1)
    # Save
    if (!is.null(rv$current_table)) {
      rv$tables[[rv$current_file]] <- rv$current_table
    }
    # Change file
    rv$current_file <- rv$current_file - 1
  })
  # /Prev
  
  # Next
  observeEvent(input$file_next, {
    req(rv$current_file < length(rv$filenames))
    # Save
    if (!is.null(rv$current_table)) {
      rv$tables[[rv$current_file]] <- rv$current_table
    }
    # Change file
    rv$current_file <- rv$current_file + 1
  })
  # /Next
  
  # update table
  observeEvent(rv$current_file,
    {
      req(rv$current_file > 0)
      
      rv$current_table <- rv$tables[[rv$current_file]]
      rv$current_table[is.na(rv$current_table)] <- ""
      rv$current_preview <- fread(
        rv$data.filepath[rv$current_file],
        stringsAsFactors = FALSE,
        data.table = FALSE,
        nrows = 5
      )
    },
    priority = 1
  )
  
  # display
  output$current_file <- renderUI(
    tags$div(
      rv$filenames[rv$current_file],
      style = paste0(
        "display: inline-block;
        font-size:14pt;
        text-align:center;"
      )
    )
  )
  
  # generate UI -----------------------------------------------------
  observeEvent(rv$current_file, {
    req(rv$current_file)
    req(!identical(rv$current_table, data.frame()))
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
      
      # GUI
      ui <- do.call(
        bsCollapse,
        args = c(
          lapply(
            seq(dim(current_table)[1]),
            fields = isolate(colnames(current_table)),
            function(row_index, fields) {
              # prepare variables
              attribute_row <- isolate(current_table[row_index, ])
              
              return(
                bsCollapsePanel(
                  title = attribute_row[fields[1]],
                  tagList(
                    column(9,
                      lapply(fields[-1], function(colname) {
                        # prepare var
                        saved_value <- isolate(current_table[row_index, colname])
                        inputId <- paste(
                          isolate(rv$current_file),
                          row_index,
                          colname,
                          sep = "-"
                        )
                        
                        # GUI
                        switch(colname,
                          attributeDefinition = textAreaInput(
                            ns(inputId),
                            value = saved_value,
                            with_red_star("Describe the attribute")
                          ),
                          class = selectInput(
                            ns(inputId),
                            "Dectected class (change if misdetected)",
                            choices = c("numeric", "character", "Date", "categorical"),
                            selected = saved_value
                          ),
                          unit = if (isTruthy(saved_value)) {
                            selectInput(
                              ns(inputId),
                              with_red_star("Select an unit"),
                              unique(c(saved_value, globals$FORMAT$UNIT)),
                              selected = saved_value
                            )
                          },
                          dateTimeFormatString = if (isTruthy(saved_value)) {
                            selectInput( # TODO add a module for hour format
                              ns(inputId),
                              with_red_star("Select a date format"),
                              unique(c(saved_value, globals$FORMAT$DATE)),
                              selected = saved_value
                            )
                          },
                          missingValueCode = textInput(
                            ns(inputId),
                            "Code for missing value (max 1 word)",
                            value = saved_value
                          ),
                          missingValueCodeExplanation = textAreaInput(
                            ns(inputId),
                            "Explain Missing Values",
                            value = saved_value
                          ),
                          NULL
                        ) # end of switch
                      }) # end of lapply colname
                    ),
                    column(
                      3,
                      h4("Preview:"),
                      tableOutput(ns(paste0("preview-", colnames(rv$current_preview)[row_index])))
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
  },
    priority = 0
  ) # end of observeEvent
  
  # generate server -----------------------------------------------------
  observeEvent(names(input), {
    req(
      !identical(rv$t_table, data.frame()),
      any(unlist(sapply(colnames(rv$current_table), grepl, names(input))))
    )
    
    sapply(
      seq(dim(rv$current_table)[1]),
      fields = colnames(rv$current_table),
      function(row_index, fields) {
        
        {
          # TODO Update style: to correct
          # observe({
          #   # browser()
          #   currentIDs <- names(input)[
          #     grepl(
          #       paste(
          #         isolate(rv$current_file),
          #         row_index,
          #         sep = "-"
          #       ),
          #       names(input)
          #     )]
          #
          #   filled <- all(sapply(currentIDs, function(id){
          #     if(grepl("date|unit", id))
          #       isTruthy(input[[id]]) &&
          #       !grepl("!Add.*here!", input[[id]])
          #     else
          #       TRUE
          #   }))
          #   print(filled)
          #   filled <- if(filled) list("success") else list("danger")
          #   names(filled) <- rv$current_table$attributeName[row_index]
          #
          #   updateCollapse(
          #     session = session,
          #     ns("collapse"),
          #     style = filled
          #   )
          #
          #   print(filled)
          # }) # end of styles
        }
        
        preview_column <- colnames(rv$current_preview)[row_index]
        output[[paste0("preview-", preview_column)]] <- renderTable(rv$current_preview[preview_column])
        
        lapply(fields[-1], function(colname) {
          inputId <- paste(
            isolate(rv$current_file),
            row_index,
            colname,
            sep = "-"
          )
          
          if (inputId %in% names(input)) {
            observeEvent(input[[inputId]], {
              req(input[[inputId]])
              if(grepl("missingValueCode", inputId)){
                if(grepl(" ", input[[inputId]])){
                  updateTextInput(
                    session, 
                    inputId, 
                    value = strsplit(input[[inputId]], " ")[[1]][1]
                  )
                  showNotification(
                    id = session$ns("mvc_update"),
                    ui = HTML("<code>missingValueCode</code> fields are limited to a <b>single word.</b>"),
                    duration = 3,
                    type = "warning"
                  )
                }
                isolate(rv$current_table[row_index, colname] <- strsplit(input[[inputId]], " ")[[1]][1])
              }
              else
                isolate(rv$current_table[row_index, colname] <- input[[inputId]])
              
              rv$tables[[rv$current_file]] <- rv$current_table
            })
          }
          
        }) # end of lapply colname
      }
    ) # end of lapply : row_index
  }) # end of observeEvent
  
  # NSB -----------------------------------------------------
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar
  )
  observeEvent(input[["nav-save"]], {
    # write filled tables
    withProgress(
      sapply(
        seq_along(rv$filenames),
        function(cur_ind) {
          incProgress(1/length(rv$filenames))
          # write filled tables
          fn <- rv$filenames[cur_ind]
          path <- savevar$emlal$DataFiles$metadatapath[cur_ind]
          table <- rv$tables[[cur_ind]]
          fwrite(table, path, sep = "\t")
        }
      ), # end of sapply
      message = "Writing filled tables"
    )
  })
  callModule(
    nextTab, "nav",
    globals, "attributes"
  )
  callModule(
    prevTab, "nav",
    globals
  )
  
  # Saves -----------------------------------------------------
  # check for completeness
  observeEvent(rv$tables, {
    rv$complete <- FALSE
    req(
      length(rv$tables) != 0 &&
        !any(sapply(rv$tables, identical, y = data.frame()))
    )
    
    rv$complete <- all(
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
  
  # en/disable buttons
  observeEvent(rv, {
    req(
      isTruthy(names(input)) && 
        isTruthy(names(rv))
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
    if (isTRUE(rv$complete)) {
      enable("nav-nextTab")
    } else {
      disable("nav-nextTab")
    }
  })
  
  # Process data -----------------------------------------------------
  observeEvent(input[["nav-nextTab"]],
    {
      req(isTRUE(rv$complete))
      disable("nav-nextTab")
      
      # TODO add `withProgress`
      # for each attribute data frame
      templateCatvars <- FALSE
      nextStep <- sapply(
        seq_along(rv$filenames),
        function(cur_ind) {
          # write filled tables
          fn <- rv$filenames[cur_ind]
          path <- savevar$emlal$DataFiles$metadatapath[cur_ind]
          table <- rv$tables[[cur_ind]]
          fwrite(table, path, sep = "\t")
          
          # check for direction: CustomUnits or CatVars
          . <- 2
          if ("custom" %in% rv$tables[[cur_ind]][, "unit"]) {
            . <- 0
          } # custom units
          if ("categorical" %in% rv$tables[[cur_ind]][, "class"]) {
            . <- c(., 1) # categorical variables
          }
          return(.)
        }
      )
      
      if (any(nextStep == 1)) {
        templateCatvars <- TRUE
      }
      nextStep <- min(nextStep)
      
      # EMLAL: template new fields if needed
      if (isTRUE(templateCatvars)) { # might not be defined
        template_categorical_variables(
          path = savevar$emlal$SelectDP$dp_metadata_path,
          data.path = savevar$emlal$SelectDP$dp_data_path
        )
      }
      
      template_geographic_coverage(
        path = savevar$emlal$SelectDP$dp_metadata_path,
        data.path = savevar$emlal$SelectDP$dp_data_path,
        empty = TRUE,
        write.file = TRUE
      )
      
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + nextStep
      
      enable("nav-nextTab")
    },
    priority = 1
  )
  
  # Output -----------------------------------------------------
  return(savevar)
}
