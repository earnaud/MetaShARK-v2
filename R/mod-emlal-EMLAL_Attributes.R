#' @title Data Package Template filling
#'
#' @description UI part of the Attributes module. Fill in the attributes of the data package
#'
#' @importFrom shiny NS fluidPage fluidRow column tags tagList HTML actionButton icon uiOutput
AttributesUI <- function(id, title, dev) {
  ns <- NS(id)
  
  return(
    fluidPage(
      # Inputs ----
      column(
        10,
        tags$h4("Data table attributes"),
        HTML("Even if EML Assembly Line automatically infers most
              of your data's metadata, some steps need you to check
              out. Please check the following attribute, and fill
              in at least the <span style='color:red;'>mandatory
              </span> elements."),
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
      
      # NSB ----
      column(2,
        navSidebar(
          ns("nav"),
          ... = tagList(
            if (dev) actionButton(ns("check"), "Dev Check"),
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
  
  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
    
    observeEvent(input$fill, {
      toFill <- names(input)
      toFill <- toFill[grepl(pattern = "^[0-9]+-[0-9]+-", toFill)]
      sapply(toFill, function(id){
        id_type = gsub("^[0-9]+-[0-9]+-","", id)
        if(id_type %in% c("missingValueCodeExplanation", "attributeDefinition", "missingValueCode"))
          updateTextAreaInput(session, id, value = "Automatically filled field.")
        if(id_type %in% c("dateTimeFormatString")){
          updateSelectInput(session, id, selected = globals$FORMAT$DATE[3])
        }
        if(id_type %in% c("unit" ))
          updateSelectInput(session, id, selected = globals$FORMAT$UNIT[1])
      })
    })
  }
  
  # variable initialization ----
  rv <- reactiveValues(
    files = savevar$emlal$DataFiles$dp_data_files$metadatapath,
    filenames = basename(savevar$emlal$DataFiles$dp_data_files$metadatapath),
    current_file = 1,
    current_table = NULL,
    complete = FALSE
  )
  pathDataObject <- paste0(savevar$emlal$SelectDP$dp_path, "/", savevar$emlal$SelectDP$dp_name, "/data_objects/")
  previews <- sapply(
    paste0(pathDataObject, list.files(pathDataObject, pattern = "preview_")),
    fread,
    data.table = FALSE
  )
  names(previews) <- basename(names(previews))
  sapply(names(previews), function(file){
    sapply(seq(previews[[file]]), function(att_ind){
      # print(ns(paste0(file, "_", att_ind)))
      # browser()
      output[[paste0(file, "_", att_ind)]] <- renderTable(previews[[file]][att_ind])
    }) # end of sapply: attribute
  }) # end of sapply: file
  
  
  # Navigation buttons ----
  # Previous
  observeEvent(input$file_prev, {
    req(rv$current_file > 1)
    
    if(isTruthy(rv$current_table)){
      try(isolate(savevar$emlal$Attributes[[ rv$filenames[rv$current_file] ]] <- rv$current_table))
    }
    
    rv$current_file <- rv$current_file - 1
  })
  
  # Next
  observeEvent(input$file_next,{
    req(rv$current_file < length(rv$files))
    
    if(isTruthy(rv$current_table)){
      try(isolate(savevar$emlal$Attributes[[ rv$filenames[rv$current_file] ]] <- rv$current_table))
    }
    
    rv$current_file <- rv$current_file + 1
  })
  
  # update table
  observeEvent(rv$current_file, {
    req(rv$current_file > 0)
    
    if(
      isTruthy(savevar$emlal$Attributes[[ rv$filenames[rv$current_file] ]]) &&
        !identical(savevar$emlal$Attributes[[ rv$filenames[rv$current_file] ]], data.frame())
    ) {
      rv$current_table <- savevar$emlal$Attributes[[ rv$filenames[rv$current_file] ]]
    }
    else {
      rv$current_table <- fread(
        rv$files[rv$current_file],
        data.table = FALSE,
        stringsAsFactors = FALSE
      )
      rv$current_table[is.na(rv$current_table)] <- ""
    }
  }, priority = 1)
  
  # display
  output$current_file <- renderUI(
    tags$div(
      basename(rv$files[rv$current_file]),
      style = paste0(
        "display: inline-block;
        font-size:14pt;
        text-align:center;"
      )
    )
  )
  
  # generate UI ----
  observeEvent(rv$current_file, {
    req(!identical(rv$current_table, data.frame()))
    
    output$edit_attributes <- renderUI({
      # validity check
      validate(
        need(
          !identical(rv$current_table, data.frame()) && isTruthy(rv$current_table),
          "No valid attributes table."
        )
      )
      # GUI
      current_table <- isolate(rv$current_table)
      
      ui <- do.call(
        bsCollapse,
        args = c(
          lapply(
            seq(dim(rv$current_table)[1]),
            fields = isolate(colnames(rv$current_table)),
            function(row_index, fields){
              # prepare variables
              attribute_row <- isolate(rv$current_table[row_index,])
              
              return(
                bsCollapsePanel(
                  title = attribute_row[ fields[1] ],
                  tagList(
                    column(9,
                      lapply(fields[-1], function(colname) {
                        # prepare var
                        saved_value <- isolate(rv$current_table[row_index, colname])
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
                            "Describe the attribute concisely"
                          ),
                          class = HTML(paste("<b>Detected class:</b>", as.vector(saved_value))),
                          unit = if(isTruthy(saved_value))
                            selectInput(
                              ns(inputId),
                              with_red_star("Select an unit"),
                              unique(c(saved_value, globals$FORMAT$UNIT)),
                              selected = saved_value
                            ),
                          dateTimeFormatString = if(isTruthy(saved_value))
                            selectInput( # TODO add a module for hour format
                              ns(inputId),
                              with_red_star("Select a date format"),
                              unique(c(saved_value, globals$FORMAT$DATE)),
                              selected = saved_value
                            ),
                          missingValueCode = textInput(
                            ns(inputId),
                            "Code for missing value",
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
                    column(3,
                      h4("Preview:"),
                      tableOutput({ns(
                        paste0(
                          names(previews)[rv$current_file],
                          "_",
                          row_index
                        )
                      )})
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
    
  }, priority = 0) # end of observeEvent
  
  # generate server ----
  observeEvent(names(input), {
    req(
      !identical(rv$current_table, data.frame()), 
      any(unlist(sapply(colnames(rv$current_table), grepl, names(input))))
    )
    
    sapply(
      seq(dim(rv$current_table)[1]),
      fields = colnames(rv$current_table),
      function(row_index, fields){
        
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
        
        lapply(fields[-1], function(colname) {
          inputId <- paste(
            isolate(rv$current_file),
            row_index,
            colname,
            sep = "-"
          )
          
          if(inputId %in% names(input)){
            observeEvent(input[[inputId]], {
              req(input[[inputId]])
              isolate(rv$current_table[row_index, colname] <- input[[inputId]])
            })
          }
          
        }) # end of lapply colname
        
      }) # end of lapply : row_index
    
  }) # end of observeEvent
  
  # NSB ----
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar,
    savevar$emlal$SelectDP$dp_path,
    savevar$emlal$SelectDP$dp_name
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar,
    savevar$emlal$SelectDP$dp_path,
    savevar$emlal$SelectDP$dp_name
  )
  callModule(
    nextTab, "nav",
    globals, "attributes"
  )
  callModule(
    prevTab, "nav",
    globals
  )
  
  # Saves ----
  # check for completeness
  observeEvent(rv$current_table, {
    rv$complete <- FALSE
    .attributes <- reactiveValuesToList(savevar$emlal$Attributes)
    req(length(.attributes) != 0 &&
        !any(sapply(.attributes, identical, y = data.frame)) &&
        !identical(rv$current_table, data.frame()))
    
    rv$complete <- all(
      sapply(
        .attributes,
        function(table){
          isTruthy(table) &&
            all(sapply(table$attributeName, isTruthy)) && 
            all(sapply(table$class, isTruthy)) && 
            !any(grepl("!Add.*here!", table$unit)) && 
            !any(grepl("!Add.*here!", table$dateTimeFormatString))
        }
      )
    )
    
  }, priority = -1)
  
  # en/disable buttons
  observe({
    req(names(input))
    if(rv$current_file == 1)
      disable("file_prev")
    else 
      enable("file_prev")
    if(rv$current_file == length(rv$files))
      disable("file_next")
    else 
      enable("file_next")
    if (isTRUE(rv$complete))
      enable("nav-nextTab")
    else 
      disable("nav-nextTab")
  })
  
  # Process data ----
  observeEvent(input[["nav-nextTab"]], {
    req(isTRUE(rv$complete))
    disable("nav-nextTab")
    
    if(isTruthy(rv$current_table)){
      try(isolate(savevar$emlal$Attributes[[ rv$filenames[rv$current_file] ]] <- rv$current_table))
    }
    
    withProgress(
      message = "Processing entered metadata ...",
      {
        incProgress(1/7)
        # for each attribute data frame
        templateCatvars <- FALSE
        nextStep <- 2
        nextStep <- min(
          sapply(seq_along(rv$filenames), function(cur_ind) {
            incProgress(1/7) # x3
            # write filled tables
            fn <- rv$filenames[cur_ind]
            path <- savevar$emlal$DataFiles$dp_data_files$metadatapath[cur_ind]
            table <- savevar$emlal$Attributes[[fn]]
            fwrite(table, path, sep = "\t")
            . <- nextStep
            # check for direction: CustomUnits or CatVars
            if ("custom" %in% savevar$emlal$Attributes[[fn]][, "unit"]) {
              . <- 0
            } # custom units
            if ("categorical" %in% savevar$emlal$Attributes[[fn]][, "class"]) {
              templateCatvars <<- TRUE
              . <- min(., 1) # categorical variables
            }
            return(.)
          })
        )
        
        # EMLAL: template new fields if needed
        if(isTRUE(templateCatvars)) # might not be defined
          template_categorical_variables(
            path = paste(savevar$emlal$SelectDP$dp_path,
              savevar$emlal$SelectDP$dp_name,
              "metadata_templates",
              sep = "/"
            ),
            data.path = paste(savevar$emlal$SelectDP$dp_path,
              savevar$emlal$SelectDP$dp_name,
              "data_objects",
              sep = "/"
            )
          )
        incProgress(1/7)
        
        template_geographic_coverage(
          path = paste(savevar$emlal$SelectDP$dp_path,
            savevar$emlal$SelectDP$dp_name,
            "metadata_templates",
            sep = "/"
          ),
          data.path = paste(savevar$emlal$SelectDP$dp_path,
            savevar$emlal$SelectDP$dp_name,
            "data_objects",
            sep = "/"
          ),
          empty = TRUE,
          write.file = TRUE
        )
        incProgress(1/7)
        
        globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + nextStep
        incProgress(1/7)
      })
    enable("nav-nextTab")
  }, priority = 1)
  
  # Output ----
  return(savevar)
}
