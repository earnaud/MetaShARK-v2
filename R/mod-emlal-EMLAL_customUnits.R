#' @title customUnitsUI
#'
#' @description UI part for the custom units part of EMLAL
#'
#' @importFrom shiny fluidPage column fluidRow NS tagList actionButton icon
#' uiOutput textOutput
customUnitsUI <- function(id, title, dev) {
  ns <- NS(id)

  return(
    fluidPage(
      # Features UI ----
      column(
        10,
        fluidRow(
          title,
          fluidRow(
            tagList(
              actionButton(ns("attribute_prev"),
                "",
                icon("chevron-left"),
                width = "12%"
              ),
              uiOutput(ns("current_attribute"),
                inline = TRUE
              ),
              actionButton(ns("attribute_next"),
                "",
                icon("chevron-right"),
                width = "12%"
              )
            ),
            style = "padding: 5px;"
          ),
          uiOutput(ns("edit_CU"))
        )
      ), # end of column1
      # Navigation UI ----
      column(
        2,
        navSidebar(ns("nav"),
          ... = tagList(
            textOutput(ns("warning_completeness")),
            if (dev) actionButton(ns("check"), "Dev Check")
          )
        )
      ) # end of column2
    ) # end of fluidPage
  ) # end of return
}

#' @title customUnits
#'
#' @description server part of the custom units module.
#'
#' @param savevar global reactiveValue containing the saved information
#' entered by the user.
#' @param globals global list containing fixed setting values for the
#' app.
#'
#' @importFrom shiny observeEvent reactiveValues observe isolate renderUI
#' callModule req textInput selectInput textAreaInput numericInput eventReactive
#' isTruthy renderText
#' @importFrom data.table fread fwrite
#' @importFrom shinyjs enable disable
customUnits <- function(input, output, session,
                        savevar, globals) {
  ns <- session$ns

  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }
  i <- 0

  # variable initialization ----

  rv <- reactiveValues(
    attributes = reactiveValues(),
    complete = FALSE
  )


  observe({
    req(savevar$emlal$templateDP) # app has been to previous step
    rv$CU_Table <- fread(paste(savevar$emlal$selectDP$dp_path,
      savevar$emlal$selectDP$dp_name,
      "metadata_templates",
      "custom_units.txt",
      sep = "/"
    ),
    data.table = FALSE,
    stringsAsFactors = FALSE,
    na.strings = NULL
    )
  })

  observeEvent(rv$CU_Table, {
    req(rv$CU_Table)
    if (any(dim(rv$CU_Table) == 0)) {
      sapply(names(savevar$emlal$templateDP), function(file_name) {
        tmp <- savevar$emlal$templateDP[[file_name]]
        # browser()
        if (any(tmp$unit == "custom")) {
          rv$attributesNames <- c(
            rv$attributesNames,
            tmp$attributeName[tmp$unit == "custom"]
          )
        }
      })
    }
  })

  observeEvent(rv$attributesNames, {
    isolate({
      sapply(
        1:length(rv$attributesNames),
        function(i) {
          rv$CU_Table[i, ] <- rep("", ncol(rv$CU_Table))
        }
      )
    })
    rv$current_attribute <- rv$attributesNames[1]
  })

  observeEvent(rv$current_attribute, {
    rv$current_index <- match(
      rv$current_attribute,
      rv$attributesNames
    )
  })

  observeEvent(
    {
      rv$CU_Table
    },
    {
      req(rv$CU_Table)
      rv$ui <- colnames(rv$CU_Table)
      savevar$emlal$templateDP$custom_units <- rv$CU_Table
    }
  )

  # Navigation buttons ----

  # Attribute selection
  observeEvent(input$attribute_prev, {
    req(rv$attributesNames)

    if (rv$current_index > 1) {
      # change attribute
      rv$current_attribute <- rv$attribute[rv$current_index - 1]
    }
  })

  observeEvent(input$attribute_next, {
    req(rv$attributesNames)

    if (rv$current_index < length(rv$CU_Table$attributeName)) {
      # change attribute
      rv$current_attribute <- rv$attribute[rv$current_index + 1]
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
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    nextTab, "nav",
    globals, globals$EMLAL$PREVIOUS[1]
  )
  callModule(
    prevTab, "nav",
    globals, globals$EMLAL$PREVIOUS[1]
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
            label = colname,
            placeholder = "e.g. milligramsPerGram",
            value = saved_value
          ),
          unitType = textInput(ns(colname),
            label = colname,
            placeholder = "e.g. mass",
            value = saved_value
          ),
          parentSI = selectInput(ns(colname),
            label = colname,
            choices = globals$FORMAT$UNIT[-1],
            selected = saved_value
          ),
          multiplierToSI = numericInput(ns(colname),
            label = colname,
            value = 1
          ),
          description = textAreaInput(ns(colname),
            label = colname,
            placeholder = "e.g. milligrams per gram",
            value = saved_value
          )
        ) # end of switch
      }) # end of lapply colname
    ) # end of tagList
  }) # end of UI

  # / Servers ----

  observe({
    req(any(rv$ui %in% names(input)))

    sapply(names(rv$CU_Table), function(rvName) {
      rv$attributes[[rvName]] <- eventReactive(input[[rvName]], {
        # get input value
        enter <- input[[rvName]]

        # check obtained value
        if (is.list(enter)) {
          enter <- unlist(enter)
        }
        if (!isTruthy(enter)) {
          # message("Input [",rvName,"] is invalid: unchanged")
          enter <- ifelse(isTruthy(unlist(rv$CU_Table[rv$current_index, rvName])),
            unlist(rv$CU_Table[rv$current_index, rvName]),
            ""
          ) # end ifelse
        } # end if
        return(enter)
      }) # end eventReactive
    }) # end sapply
  }) # end observe
  # end Servers

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
        unlist(reactiveValuesToList(rv$attributes)),
        rv$CU_Table
      )
      # save metadata
      rv$CU_Table[rv$current_index, ] <- printReactiveValues(
        rv$attributes
      )[names(rv$CU_Table)]
    }
  )

  observeEvent(rv$CU_Table, {
    req(rv$CU_Table)
    cat("eval\n")
    rv$complete <- isTruthy(unlist(rv$CU_Table))
  })

  observeEvent(rv$complete, {
    if (rv$complete) {
      enable("nav-nextTab")
      output$warning_completeness <- renderText(NULL)
    }
    else {
      disable("nav-nextTab")
      output$warning_completeness <- renderText("All fields must be filled.")
    }
  })

  # Process data ----

  observeEvent(input[["nav-nextTab"]],{
    req(unlist(rv$CU_Table))
    
    # EMLAL: write CU modifications
    fwrite(rv$CU_Table, paste(savevar$emlal$selectDP$dp_path,
                              savevar$emlal$selectDP$dp_name,
                              "metadata_templates",
                              "custom_units.txt",
                              sep = "/"
    ))
    # MUST DO: replace attribute's 'custom units' in units fields by
    # the name of this custom unit (saved in savevar)
    
    files_names <- savevar$emlal$DPfiles$dp_data_files$name
    sapply(files_names, function(fn) {
      # write filled tables
      cur_ind <- match(fn, files_names)
      path <- savevar$emlal$DPfiles$dp_data_files$metadatapath[cur_ind]
      table <- savevar$emlal$templateDP[[fn]]
      browser()
      fwrite(table, path)
    })
    
    # Next: read road programmed in templateDP
    globals$EMLAL$PREVIOUS[1] <- "customUnits"
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE+globals$EMLAL$PREVIOUS[3]
  }, priority = 1)

  observeEvent(input[["nav-prevTab"]], {
    globals$EMLAL$PREVIOUS[1] <- "customUnits"
  })
  
  # Output ----
  return(savevar)
}
