# EMLAL_templateDP.R

## 3. Create DP template
templateDPUI <- function(id, title, IM){
  ns <- NS(id)
  
  return(
    fluidPage(
      # Inputs ----
      column(10,
             h4("Data table attributes"),
             HTML("Even if EML Assembly Line automatically infers most
                  of your data's metadata, some steps need you to check
                  out. Please check the following attribute, and fill 
                  in at least the <span style='color:red;'>mandatory
                  </span> elements."),
             fluidRow(
               tagList(
                 actionButton(ns("file_prev"),
                              "",
                              icon("chevron-left"),
                              width = "12%"),
                 uiOutput(ns("current_file"),
                          inline = TRUE),
                 actionButton(ns("file_next"),
                              "",
                              icon("chevron-right"),
                              width = "12%")
               ),
               style = "padding: 5px;"
             ),
             fluidRow(
               tagList(
                 actionButton(ns("attribute_prev"),
                              "",
                              icon("chevron-left"),
                              width = "12%"),
                 uiOutput(ns("current_attribute"),
                          inline = TRUE),
                 actionButton(ns("attribute_next"),
                              "",
                              icon("chevron-right"),
                              width = "12%")
               ),
               style = "padding: 5px;"
             ),
             uiOutput(ns("edit_template"))
      ),
      
      # Navigation buttons ----
      column(2,
             h4("Navigation"),
             quitButton(id, style = rightButtonStyle),
             saveButton(id, style = rightButtonStyle),
             shinyjs::disabled(nextTabButton(id, style = rightButtonStyle)),
             prevTabButton(id, style = rightButtonStyle),
             style = "text-align: center; padding: 0;"
             ,actionButton(ns("check2"),"Dev Check")
             ,actionButton(ns("check3"),"Fill")
      ) # end column 2
    ) # end fluidPage
  ) # end return
  
}

templateDP <- function(input, output, session, IM, savevar, globalRV){
  ns <- session$ns
  
  observeEvent(input$check2,{
    browser()
  })
  observeEvent(input$check3,{
    req(rv$current_file)
    sapply(names(rv$attributesTable), function(nn){
      rv$attributesTable[,nn] <- rep("a", 
                                     dim(rv$attributesTable)[1])
    })
    savevar$emlal$templateDP[[rv$current_file]] <- rv$attributesTable
  })
  # variable initialization ----
  
  # main local reactiveValues
  rv <- reactiveValues(
    # local save
    attributes = reactiveValues(),
    # utility
    ui = character(),
    completed = FALSE
  )
  
  observe({
    req(savevar$emlal$createDP$dp_data_files)
    rv$files_names <- savevar$emlal$createDP$dp_data_files$name
  })
  observeEvent(rv$files_names, {
    req(rv$files_names)
    rv$current_file <- rv$files_names[1]
  })
  
  # on file change
  observeEvent(rv$current_file, {
    req(rv$current_file) # already req savevar$..$dp_data_files
    req(savevar$emlal$createDP$dp_data_files$metadatapath)
    toRead <- savevar$emlal$createDP$dp_data_files
    toRead <- toRead$metadatapath[
      match(rv$current_file, toRead$name)
      ]
    # load attributes_table.txt
    rv$attributesTable <- fread(toRead, data.table = FALSE, 
                                stringsAsFactors = FALSE,
                                na.strings = NULL)
    rv$attributesTable[is.na(rv$attributesTable)] <- ""
    # get current attribute
    rv$current_attribute <- 0 # trick: trigger rv$current_attribute -dependant observers
    rv$current_attribute <- 1
  })
  observeEvent(rv$current_attribute, {
    req(rv$attributesTable)
    tmpAttributes <- rv$attributesTable[rv$current_attribute,]
    rv$ui <- names(tmpAttributes)
    rv$ui <- rv$ui[rv$ui != "attributeName"]
    # units case
    if(tmpAttributes["unit"] == "")
      rv$ui <- rv$ui[!grepl("unit", rv$ui)]
    # date case
    if(tmpAttributes[rv$ui[grepl("date", rv$ui)] ] == "")
      rv$ui <- rv$ui[!grepl("date", rv$ui)]
  })
  
  # Navigation buttons ----
  # ** files
  observeEvent(input$file_prev,{
    req(rv$attributes
        # , rv$customUnits
    )
    cur_ind <- match(rv$current_file, rv$files_names)
    if(cur_ind > 1){
      # save metadata
      rv <- saveInput(rv)
      # change file
      rv$current_file <- rv$files_names[cur_ind - 1]
    }
  })
  observeEvent(input$file_next,{
    req(rv$attributes
        # , rv$customUnits
    )
    cur_ind <- match(rv$current_file, rv$files_names)
    if(cur_ind < length(rv$files_names) ){
      # save metadata
      rv <- saveInput(rv)
      # change file
      rv$current_file <- rv$files_names[cur_ind + 1]
    }
  })
  # ** attribute
  observeEvent(input$attribute_prev,{
    req(rv$attributes
        # , rv$customUnits
    )
    if(rv$current_attribute > 1){
      # save metadata
      rv <- saveInput(rv)
      # change attribute
      rv$current_attribute <- rv$current_attribute - 1
    }
  })
  observeEvent(input$attribute_next,{
    req(rv$attributes
        # , rv$customUnits
    )
    if(rv$current_attribute < length(rv$attributesTable$attributeName)){
      # save metadata
      rv <- saveInput(rv)
      # change attribute
      rv$current_attribute <- rv$current_attribute + 1
    }
  })
  
  # outputs
  {
    output$current_file <- renderUI(div(rv$current_file,
                                        style = paste0("display: inline-block;
                                                       font-size:20pt;
                                                       text-align:center;
                                                       width:70%;
                                                       background: linear-gradient(90deg, #3c8dbc ",
                                                       round(100*match(rv$current_file, rv$files_names)/length(rv$files_names)),
                                                       "%, white ",
                                                       round(100*match(rv$current_file, rv$files_names)/length(rv$files_names)),
                                                       "%);")
    )
    )
    output$current_attribute <- renderUI(div(rv$attributesTable[rv$current_attribute,"attributeName"],
                                             style = paste0("display: inline-block;
                                                            font-size:15pt;
                                                            text-align:center;
                                                            width:70%;
                                                            background: linear-gradient(90deg, #3c8dbc ",
                                                            round(100*rv$current_attribute/dim(rv$attributesTable)[1]),
                                                            "%, white ",
                                                            round(100*rv$current_attribute/dim(rv$attributesTable)[1]),
                                                            "%);"
                                             )
                                             
    )
    )
  }
  
  callModule(onQuit, IM.EMLAL[5],
             # additional arguments
             globalRV, savevar,
             savevar$emlal$selectDP$dp_path, 
             savevar$emlal$selectDP$dp_name)
  callModule(onSave, IM.EMLAL[5],
             # additional arguments
             savevar,
             savevar$emlal$selectDP$dp_path, 
             savevar$emlal$selectDP$dp_name)
  callModule(nextTab, IM.EMLAL[5],
             globalRV, "template")
  callModule(prevTab, IM.EMLAL[5],
             globalRV, "template")
  
  # Procedurals / ----
  # / UI ----
  output$edit_template <- renderUI({
    req(rv$ui)
    
    # actions
    tagList(
      # write each attribute's characteristic
      lapply(rv$ui, function(colname) {
        # prepare var
        saved_value <- rv$attributesTable[rv$current_attribute, colname]
        if(grepl("date", colname)) saved_value <- gsub("^(.*) .*$","\\1", saved_value)
        
        # UI
        switch(colname,
               attributeDefinition = textAreaInput(ns(colname), value = saved_value,
                                                   "Describe the attribute concisely"),
               class = HTML(paste("<b>Detected class:</b>", as.vector(saved_value) ) ),
               unit = selectInput(ns(colname), 
                                  span("Existing unit", style=redButtonStyle),
                                  unique(c(saved_value, UNIT.LIST)),
                                  selected = saved_value
               ),
               dateTimeFormatString = tagList(selectInput(ns( paste0(colname,"_date") ),
                                                          span("Existing date format",
                                                               style=redButtonStyle),
                                                          unique(c(saved_value, DATE.FORMAT)),
                                                          selected = saved_value),
                                              selectInput(ns( paste0(colname,"_hour") ),
                                                          "Existing hour format",
                                                          HOUR.FORMAT )
               ),
               missingValueCode = textInput(ns(colname), 
                                            "Code for missing value",
                                            value = saved_value),
               missingValueCodeExplanation = textAreaInput(ns(colname),
                                                           "Explain Missing Values",
                                                           value = saved_value)
        ) # end of switch
      }) # end of lapply colname
    ) # end of tagList
  }) # end of UI
  
  
  # / Servers / ----
  observe({
    req( any(rv$ui %in% names(input)) )
    
    # / attributes ----
    sapply(names(rv$attributesTable), function(rvName) {
      # prepare variable
      # two different names: ids from input can be suffixed
      inputNames <- names(input)[
        which(grepl(rvName, names(input)))
        ]
      # 'unit' exception:
      if(rvName == "unit")
        inputNames <- inputNames[inputNames != "unitType"]
      
      # check corresponding input
      if(length(inputNames) != 0){
        if(
          rvName %in% rv$ui
        ){
          # show UI
          sapply(inputNames, shinyjs::show)
          
          # Input UI yet exists: create eventReactive
          rv$attributes[[rvName]] <- eventReactive({
            if(rvName == "dateTimeFormatString"){
              if(is.na(input[[paste0(rvName,"_hour")]])
                 || input[[paste0(rvName,"_hour")]] == "NA")
              {
                input[[paste0(rvName,"_date")]]
              }
              else
              {
                input[[paste0(rvName,"_date")]]
                input[[paste0(rvName,"_hour")]]
              }
            }
            else{
              input[[rvName]]
            }
          },{
            # get input value
            enter <- if(rvName == "dateTimeFormatString"){
              if(is.na(input[[paste0(rvName,"_hour")]])
                 || input[[paste0(rvName,"_hour")]] == "NA")
                input[[paste0(rvName,"_date")]]
              else
                paste(input[[paste0(rvName,"_date")]],
                      input[[paste0(rvName,"_hour")]],
                      sep = " " # make sure
                )
            }
            else if(rvName == "unit"
                    && input[[rvName]] == "custom")
              input[["id"]] # link to custom inputs
            else
              input[[rvName]]
            
            # check obtained value
            if(is.list(enter))
              enter <- unlist(enter)
            if(!isTruthy(enter)){
              message("Input [",rvName,"] is invalid: unchanged")
              enter <- ifelse(isTruthy(unlist(rv$attributesTable[rv$current_attribute,rvName])),
                              unlist(rv$attributesTable[rv$current_attribute,rvName]),
                              ""
              )
            } # end if
            enter
          }) # end of eventReactive
        }
        else{
          # hide UI
          sapply(inputNames, shinyjs::hide)
          
          # set reactiveValue to NULL
          rv$attributes[[rvName]] <- rv$attributesTable[rv$current_attribute, rvName]
        }
      } # end if inputNames == character(0)
      else{
        rv$attributes[[rvName]] <- rv$attributesTable[rv$current_attribute, rvName]
      }
    }) # end sapply
    
  })
  
  # Saves ----
  cpltTrigger <- makeReactiveTrigger()
  
  # regular saves in savevar - triggered in saveInput()
  observeEvent({
    input
  },{
    req(rv$current_file)
    savevar$emlal$templateDP[[rv$current_file]] <- rv$attributesTable
    cpltTrigger$trigger()
  })
  
  # check for completeness
  observe({
    cpltTrigger$depend()
    req(!is.null(rv$completed))
    rv$completed <- (
      isTruthy(rv$attributesTable$attributeName)
      && all(sapply(rv$attributesTable$attributeName, isTruthy))
      && all(sapply(rv$attributesTable$class, isTruthy))
      && !any(grepl("!Add.*here!",rv$attributesTable$unit))
      && !any(grepl("!Add.*here!",rv$attributesTable$dateTimeFormatString))
    )
    if(rv$completed){
      shinyjs::enable(ns("nextTab"))
    }
    else{
      shinyjs::disable(ns("nextTab"))
    }
  })
  
  # Process data ----
  observeEvent(input[[ns("nextTab")]], {
    req(rv$completed)
    # for each attribute data frame
    nextStep = 1 # default = catvars
    sapply(rv$files_names, function(fn){
      # write filled tables
      cur_ind <- match(fn, rv$files_names)
      path <- savevar$emlal$createDP$dp_data_files$metadatapath[cur_ind]
      table <- savevar$emlal$templateDP[[fn]]
      fwrite(table, path)
     
      # check for direction: customUnits or catvars
      if(nextStep > 0 &&
         "custom" %in% savevar$emlal$templateDP[[fn]][,"unit"])
          nextStep = 0
    })
    globalRV$navigate <- globalRV$navigate+nextStep
  },
  priority = 1)
  
  # Output ----
  return(savevar)
}