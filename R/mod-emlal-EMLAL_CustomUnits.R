#' @title CustomUnitsUI
#'
#' @description UI part of the CustomUnits module.
#'
#' @importFrom shiny NS fluidPage column fluidRow tagList tags icon actionButton uiOutput textOutput
CustomUnitsUI <- function(id, title, dev) {
  ns <- NS(id)
  
  return(
    fluidPage(
      # Features UI ----
      column(
        10,
        tags$h4("Custom units description"),
        fluidRow(
          fluidRow(
            column(1,
              actionButton(
                ns("attribute_prev"),
                "",
                icon("chevron-left")
              )
            ),
            column(10,
              uiOutput(
                ns("current_attribute"),
                inline = TRUE
              )
            ),
            column(1,
              actionButton(
                ns("attribute_next"),
                "",
                icon("chevron-right")
              )
            )
          ),
          textOutput(ns("attributeFile")),
          uiOutput(ns("edit_CU"))
        )
      ), # end of column1
      # NSB ----
      column(
        2,
        navSidebar(ns("nav"),
          ... = tagList(
            textOutput(ns("warning_completeness")),
            if (dev) actionButton(ns("check"), "Dev Check"),
            if (dev) actionButton(ns("fill"), "Fill")
          )
        )
      ) # end of column2
    ) # end of fluidPage
  ) # end of return
}

#' @title CustomUnitsUI
#'
#' @description server part of the CustomUnits module.
#'
#' @importFrom shiny observeEvent reactiveValues observe req isolate callModule renderUI tagList textInput selectInput
#' numericInput textAreaInput eventReactive reactiveValuesToList
#' @importFrom data.table fread
#' @importFrom shinyjs enable disable toggleState
#' @importFrom dplyr select mutate filter %>%
CustomUnits <- function(input, output, session,
  savevar, globals) {
  ns <- session$ns
  
  # DEV ----
  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }
  
  # fill the description fields with automatically filled field
  observeEvent(input$fill, {
    req(exists("rv"))
    sapply(names(rv$CU_Table), function(field){
      if(! field %in% c("parentSI", "multiplierToSI"))
        rv$CU_Table[, field] <- "Automatically filled field."
      else
        rv$CU_Table[, field] <- rv$attributes[[field]]()
    })
    savevar$emlal$Attributes$custom_units <- rv$CU_Table
  })
  
  
  # variable initialization ----
  rv <- reactiveValues(
    CU_Table = data.frame(),
    attributesNames = c(),
    attributesFiles = c(),
    current_index = integer(),
    current_attribute = c(),
    attributes = reactiveValues(),
    complete = FALSE
  )
  
  # Once-only triggered
  observeEvent(TRUE,
    {
      # need to have filled Attributes
      req(
        isolate(
          unlist(
            reactiveValuesToList(
              savevar$emlal$Attributes
            )
          )
        )
      )
      disable("nav-nextTab")
      rv$CU_Table <- fread(
        paste(savevar$emlal$SelectDP$dp_path,
          savevar$emlal$SelectDP$dp_name,
          "metadata_templates",
          "custom_units.txt",
          sep = "/"
        ),
        data.table = FALSE,
        stringsAsFactors = FALSE,
        na.strings = NULL
      )
      
      # get attributesNames and attributesFiles
      sapply(names(savevar$emlal$Attributes), function(file_name) {
        if (file_name != "custom_units") {
          # shorten attributes' data frame name
          tmp <- savevar$emlal$Attributes[[file_name]] 
          if (any(tmp$unit == "custom")) {
            # expand attributes' names list
            rv$attributesNames <<- c(
              rv$attributesNames,
              tmp$attributeName[tmp$unit == "custom"]
            )
            # get the attribute's corresponding data file's name
            rv$attributesFiles <<- c(
              rv$attributesFiles,
              rep(file_name, length(tmp$attributeName[tmp$unit == "custom"]))
            )
          }
        }
      })
      
      # correct CU_Table if needed
      if (any(dim(rv$CU_Table) == 0)) {
        isolate({
          sapply(
            1:length(rv$attributesNames),
            function(i) {
              rv$CU_Table[i, ] <- rep("", ncol(rv$CU_Table))
            }
          )
        })
      }
      rv$CU_Table <- rv$CU_Table %>% 
        mutate(parentSI = replace(.$parentSI, TRUE, "dimensionless")) %>% 
        mutate(multiplierToSI = replace(.$multiplierToSI, TRUE, 1))
      
      # set current index (for attribute)
      rv$current_index <- 1
    },
    once = TRUE
  )
  
  # Multiply triggered
  observeEvent(rv$current_index, {
    req(rv$current_index)
    rv$current_attribute <- rv$attributesNames[rv$current_index]
    output$attributeFile <- renderText({
      paste("In", rv$attributesFiles[rv$current_index])
    })
    
    # enable/disable units navigation button
    if (rv$current_index <= 1) {
      disable("attribute_prev")
    }
    else {
      enable("attribute_prev")
    }
    
    if (rv$current_index >= length(rv$attributesNames)) {
      disable("attribute_next")
    }
    else {
      enable("attribute_next")
    }
  })
  
  observeEvent(rv$CU_Table, {
    req(rv$CU_Table)
    rv$ui <- colnames(rv$CU_Table)
    savevar$emlal$Attributes$custom_units <- rv$CU_Table
  })
  
  # Navigation buttons ----
  
  # Attribute selection
  observeEvent(input$attribute_prev, {
    req(rv$attributesNames, rv$current_index)
    if (rv$current_index > 1) {
      rv$current_index <- rv$current_index - 1
    }
  })
  
  observeEvent(input$attribute_next, {
    req(rv$attributesNames, rv$current_index)
    if (rv$current_index < length(rv$attributesNames)) {
      rv$current_index <- rv$current_index + 1
    }
  })
  
  output$current_attribute <- renderUI({
    div(rv$current_attribute,
      style = paste0(
        "display: inline-block;
                        font-size:15pt;
                        text-align:center;
                        width:70%;
                        background: linear-gradient(90deg, #3c8dbc ",
        round(100 * rv$current_index / length(rv$attributesNames)),
        "%, white ",
        round(100 * rv$current_index / length(rv$attributesNames)),
        "%);"
      )
    )
  })
  
  # NSB
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
    globals, "CustomUnits"
  )
  callModule(
    prevTab, "nav",
    globals
  )
  
  # Procedurals ----
  # / UI ----
  # Warning: Error in choicesWithNames: argument "choices" is missing, with no default
  output$edit_CU <- renderUI({
    req(rv$ui)
    
    # actions
    tagList(
      # write each attribute's characteristic
      lapply(rv$ui, function(colname) {
        # prepare var
        saved_value <- rv$CU_Table[rv$current_index, colname]
        
        # UI
        switch(colname,
          id = textInput(ns(colname),
            label = with_red_star(colname),
            placeholder = "e.g. milligramsPerGram",
            value = saved_value
          ),
          unitType = textInput(ns(colname),
            label = span(colname, class = "redButton"),
            placeholder = "e.g. mass",
            value = saved_value
          ),
          parentSI = selectInput(ns(colname),
            label = span(colname, class = "redButton"),
            choices = globals$FORMAT$UNIT[-1],
            selected = saved_value,
          ),
          multiplierToSI = numericInput(ns(colname),
            label = span(colname, class = "redButton"),
            value = 1,
          ),
          description = textAreaInput(ns(colname),
            label = span(colname, class = "redButton"),
            placeholder = "e.g. milligrams per gram",
            value = saved_value,
          )
        ) # end of switch
      }) # end of lapply colname
    ) # end of tagList
  }) # end of UI
  
  # / Servers ----
  observe({
    req(any(rv$ui %in% names(input)))
    
    sapply(names(rv$CU_Table), function(rvName) {
      rv$attributes[[rvName]] <- eventReactive(input[[rvName]],
        {
          # get input value
          enter <- input[[rvName]]
          
          # check obtained value
          if (is.list(enter)) {
            enter <- unlist(enter)
          }
          return(enter)
        },
        ignoreNULL = FALSE
      ) # end eventReactive
    }) # end sapply
  }) # end observe
  
  # Saves ----
  observeEvent(
    {
      input$id
      input$unitType
      input$parentSI
      input$multiplierToSI
      input$description
    },
    {
      req(
        !is.null(unlist(reactiveValuesToList(rv$attributes))),
        rv$CU_Table
      )
      # save metadata
      rv$CU_Table[rv$current_index, ] <- printReactiveValues(
        rv$attributes
      )[names(rv$CU_Table)]
    }
  )
  
  rv$complete <- reactive({
    req(rv$CU_Table)
    all(sapply(unlist(rv$CU_Table), isTruthy))
  })
  
  observe({
    req(rv$complete)
    
    if (rv$complete()) {
      enable("nav-nextTab")
      output$warning_completeness <- renderText(NULL)
    } else {
      disable("nav-nextTab")
      output$warning_completeness <- renderText("All fields must be filled.")
    }
  })
  
  # Process data ----
  observeEvent(input[["nav-nextTab"]],
    {
      fwrite(
        rv$CU_Table,
        paste(savevar$emlal$SelectDP$dp_path,
          savevar$emlal$SelectDP$dp_name,
          "metadata_templates",
          "custom_units.txt",
          sep = "/"
        ),
        sep = "\t"
      )
      # avoid catvar filling if not templated
      if (
        !any(
          grepl("catvar", 
            dir(
              paste(savevar$emlal$SelectDP$dp_path,
                savevar$emlal$SelectDP$dp_name,
                "metadata_templates",
                sep = "/")
            )
          )
        )
      )
        globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE+1
    },
    priority = 1
  )
  
  observeEvent(input[["nav-prevTab"]],
    {
      
    },
    priority = 1
  )
  
  # Output ----
  return(savevar)
}
