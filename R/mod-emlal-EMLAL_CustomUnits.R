#' @title CustomUnitsUI
#'
#' @description UI part of the CustomUnits module.
#'
#' @importFrom shiny NS fluidPage column fluidRow tagList tags icon actionButton uiOutput textOutput
CustomUnitsUI <- function(id, title, dev) {
  ns <- NS(id)
  
  return(
    fluidPage(
      # Features UI -----------------------------------------------------
      column(
        10,
        tags$h4("Custom units description"),
        fluidRow(
          fluidRow(
            column(
              1,
              actionButton(
                ns("attribute_prev"),
                "",
                icon("chevron-left")
              )
            ),
            column(
              10,
              uiOutput(
                ns("current_attribute"),
                inline = TRUE
              )
            ),
            column(
              1,
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
      # NSB -----------------------------------------------------
      column(
        2,
        navSidebar(ns("nav"),
          ... = tagList(
            textOutput(ns("warning_completeness")),
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
  
  # DEV -----------------------------------------------------
  # fill the description fields with automatically filled field
  if(globals$dev || isTRUE(savevar$quick)){
    observeEvent(input$fill, {
      req(exists("rv"))
      sapply(names(rv$CU_Table), function(field) {
        if (!field %in% c("parentSI", "multiplierToSI")) {
          rv$CU_Table[, field] <- "Automatically filled field."
        } else if (field == "id") {
          rv$CU_Table[, field] <- paste0("unit-", 1:dim(rv$CU_Table)[1])
        } else {
          rv$CU_Table[, field] <- rv$attributes[[field]]()
        }
      })
    })
  }
  
  # variable initialization -----------------------------------------------------
  rv <- reactiveValues(
    CU_Table = data.frame(),
    attributesNames = c(),
    attributesFiles = c(),
    current_index = integer(),
    current_attribute = c(),
    attributes = reactiveValues(),
    complete = FALSE
  )
  
  rv$CU_Table <- fread(
    paste(
      savevar$emlal$SelectDP$dp_metadata_path,
      "custom_units.txt",
      sep = "/"
    ),
    data.table = FALSE,
    stringsAsFactors = FALSE,
    na.strings = NULL
  )
  
  attributeFiles <- list.files(
    savevar$emlal$SelectDP$dp_metadata_path,
    pattern = "attributes", 
    full.names = TRUE
  )
  # browser()
  sapply(attributeFiles, function(file_name) {
    # read attribute file
    tmp <- fread(
      file_name, 
      stringsAsFactors = FALSE, 
      data.table = FALSE
    )
    # detect interesting rows -- "custom" or else
    tmp_ind <- !(tmp$unit %in% c("", globals$FORMAT$UNIT) &
        tmp$unit != "custom")
    if (any(tmp_ind)) {
      # expand attributes' names list
      rv$attributesNames <<- c(
        rv$attributesNames,
        tmp$attributeName[tmp_ind]
      )
      # get the attribute's corresponding data file's basename
      rv$attributesFiles <<- c(
        rv$attributesFiles,
        rep(basename(file_name), length(tmp$attributeName[tmp_ind]))
      )
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
  
  # Navigation buttons -----------------------------------------------------
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
  })
  
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
      fwrite(
        rv$CU_Table,
        paste(
          savevar$emlal$SelectDP$dp_metadata_path,
          "custom_units.txt",
          sep = "/"
        ),
        sep = "\t"
      ),
      message = "Writing filled tables"
    )
  })
  callModule(
    nextTab, "nav",
    globals, "CustomUnits"
  )
  callModule(
    prevTab, "nav",
    globals
  )
  
  # Procedurals -----------------------------------------------------
  # / UI -----------------------------------------------------
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
            value = if(isTruthy(saved_value)) saved_value else "custom"
          ),
          unitType = textInput(ns(colname),
            label = with_red_star(colname),
            placeholder = "e.g. mass",
            value = saved_value
          ),
          parentSI = selectInput(ns(colname),
            label = with_red_star(colname),
            choices = globals$FORMAT$UNIT[-1],
            selected = saved_value,
          ),
          multiplierToSI = numericInput(ns(colname),
            label = with_red_star(colname),
            value = 1,
          ),
          description = textAreaInput(ns(colname),
            label = with_red_star(colname),
            placeholder = "e.g. milligrams per gram",
            value = saved_value,
          )
        ) # end of switch
      }) # end of lapply colname
    ) # end of tagList
  }) # end of UI
  
  # / Servers -----------------------------------------------------
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
  
  # Saves -----------------------------------------------------
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
    req(names(input))
    
    if (rv$complete()) {
      enable("nav-nextTab")
      output$warning_completeness <- renderText(NULL)
    } else {
      disable("nav-nextTab")
      output$warning_completeness <- renderText("All fields must be filled.")
    }
  })
  
  # Process data -----------------------------------------------------
  observeEvent(input[["nav-nextTab"]], {
    # Write modified Attributes units
    tmp <- data.frame(
      file = rv$attributesFiles,
      attributeName = rv$attributesNames,
      value = rv$CU_Table$id,
      stringsAsFactors = FALSE
    )
    
    sapply(unique(tmp$file), function(filename){
      file_ind <- tmp$file == filename
      tmp_attr <- tmp$attributeName[file_ind]
      filename <- list.files(savevar$emlal$SelectDP$dp_metadata_path, pattern = filename, full.names = TRUE)
      
      df <- fread(filename, stringsAsFactors = FALSE, data.table = FALSE)
      df[df$attributeName %in% tmp_attr, "unit"] <- tmp$value[file_ind]
      fwrite(df, filename, sep = "\t")
    })
    
    # Write Custom Units
    fwrite(
      rv$CU_Table,
      paste(
        savevar$emlal$SelectDP$dp_metadata_path,
        "custom_units.txt",
        sep = "/"
      ),
      sep = "\t"
    )
    # avoid catvar filling if not templated
    if (
      !any(
        grepl(
          "catvar",
          dir(
            savevar$emlal$SelectDP$dp_metadata_path
          )
        )
      )
    ) {
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + 1
    }
  },
  priority = 1
  )
  
  # Output -----------------------------------------------------
  return(savevar)
}
