#' @title Data Package Template filling
#'
#' @description UI part of the templateDP module. Fill in the attributes of the data package
#'
#' @importFrom shiny NS fluidPage fluidRow column tags tagList HTML actionButton icon uiOutput
templateDPUI <- function(id, title, dev) {
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

        tags$div(
          fluidRow(
            tagList(
              actionButton(ns("file_prev"),
                "",
                icon("chevron-left"),
                width = "12%"
              ),
              uiOutput(ns("current_file"),
                inline = TRUE
              ),
              actionButton(ns("file_next"),
                "",
                icon("chevron-right"),
                width = "12%"
              )
            ),
            style = "padding: 5px;"
          ),
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
          uiOutput(ns("edit_template")),
          class = "inputBox"
        )
      ),

      # NSB ----
      column(
        2,
        navSidebar(ns("nav"),
          ... = tagList(
            if (dev) actionButton(ns("check"), "Dev Check"),
            if (dev) actionButton(ns("fill"), "Fill")
          )
        )
      ) # end column 2
    ) # end fluidPage
  ) # end return
}

#' @describeIn templateDPUI
#'
#' @description server part of the templateDP module.
#'
#' @importFrom data.table fread fwrite
#' @importFrom shiny observeEvent req reactiveValues observe renderUI tags callModule textAreaInput HTML selectInput tagList
#' observe eventReactive validate
#' @importFrom shinyjs hide show enable disable
#' @importFrom EMLassemblyline template_categorical_variables template_geographic_coverage
templateDP <- function(input, output, session, savevar, globals) {
  ns <- session$ns

  # DEV ----

  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })

    # fill the attribute fields with "a" or random units/dateFormat to enable 'nextTab' button
    observeEvent(input$fill, {
      req(exists("rv"))
      sapply(rv$files_names, function(fn) {
        nns <- names(savevar$emlal$templateDP[[fn]])
        sapply(nns, function(nn) {
          if (grepl("date", nn)) {
            savevar$emlal$templateDP[[fn]][, nn] <- gsub(
              "^.+$", globals$FORMAT$DATE[1],
              savevar$emlal$templateDP[[fn]][, nn]
            )
          }
          else if (grepl("unit", nn)) {
            savevar$emlal$templateDP[[fn]][, nn] <- gsub(
              "^.+$", globals$FORMAT$UNIT[10], # not custom
              savevar$emlal$templateDP[[fn]][, nn]
            )
          }
          else if (!(nn %in% c("attributeName", "class"))) {
            savevar$emlal$templateDP[[fn]][, nn] <- "Automatically filled field."
          }
        })
      })
      rv$attributesTable <- savevar$emlal$templateDP[[rv$current_file]]
    })
  }

  # variable initialization ----
  # main local reactiveValues
  rv <- reactiveValues(
    # local save
    attributes = reactiveValues(),
    # utility
    ui = character(),
    completed = FALSE,
    templateCatvars = FALSE
  )

  observe({
    req(savevar$emlal$DPfiles$dp_data_files)
    rv$files_names <- savevar$emlal$DPfiles$dp_data_files$name
  })
  observeEvent(rv$files_names, {
    req(rv$files_names)
    sapply(rv$files_names, function(fn) {
      # not saved yet in savevar
      if (is.null(savevar$emlal$templateDP[[fn]])) {
        toRead <- savevar$emlal$DPfiles$dp_data_files
        toRead <- toRead$metadatapath[
          match(fn, toRead$name)
        ]
        savevar$emlal$templateDP[[fn]] <- fread(toRead,
          data.table = FALSE,
          stringsAsFactors = FALSE,
          na.strings = NULL
        )
        savevar$emlal$templateDP[[fn]][is.na(savevar$emlal$templateDP[[fn]])] <- ""
        rv$current_attribute <- 0 # trick: trigger rv$current_attribute-dependant observers
        rv$current_attribute <- 1
      }
      # yet saved in savevar
      else {

      }
    })
    rv$current_file <- rv$files_names[1]
  })

  # on file change
  observeEvent(rv$current_file, {
    req(rv$current_file)
    rv$attributesTable <- savevar$emlal$templateDP[[rv$current_file]]
    # set current attribute
    # rv$current_attribute <- 0 # trick: trigger rv$current_attribute-dependant observers
    rv$current_attribute <- 1
  })
  observeEvent(
    {
      rv$current_file
      rv$current_attribute
    },
    {
      req(rv$attributesTable)
      tmpAttributes <- rv$attributesTable[rv$current_attribute, ]
      rv$ui <- names(tmpAttributes)
      rv$ui <- rv$ui[rv$ui != "attributeName"]
      # units case
      if (tmpAttributes["unit"] == "") {
        rv$ui <- rv$ui[!grepl("unit", rv$ui)]
      }
      # date case
      if (tmpAttributes[rv$ui[grepl("date", rv$ui)] ] == "") {
        rv$ui <- rv$ui[!grepl("date", rv$ui)]
      }
    }
  )

  # Navigation buttons ----
  # ** files
  observeEvent(input$file_prev,
    {
      req(
        rv$attributes
      )
      cur_ind <- match(rv$current_file, rv$files_names)
      if (cur_ind > 1) {
        # save metadata
        rv <- saveInput(rv)
        savevar$emlal$templateDP[[rv$current_file]] <- rv$attributesTable
        # change file
        rv$current_file <- rv$files_names[cur_ind - 1]
      }
    },
    priority = -1
  )
  observeEvent(input$file_next,
    {
      req(
        rv$attributes
      )
      cur_ind <- match(rv$current_file, rv$files_names)
      if (cur_ind < length(rv$files_names)) {
        # save metadata
        rv <- saveInput(rv)
        savevar$emlal$templateDP[[rv$current_file]] <- rv$attributesTable
        # change file
        rv$current_file <- rv$files_names[cur_ind + 1]
      }
    },
    priority = -1
  )
  # ** attribute
  observeEvent(input$attribute_prev, {
    req(
      rv$attributes
    )
    if (rv$current_attribute > 1) {
      # save metadata
      rv <- saveInput(rv)
      # change attribute
      rv$current_attribute <- rv$current_attribute - 1
    }
  })
  observeEvent(input$attribute_next, {
    req(
      rv$attributes
    )
    if (rv$current_attribute < length(rv$attributesTable$attributeName)) {
      # save metadata
      rv <- saveInput(rv)
      # change attribute
      rv$current_attribute <- rv$current_attribute + 1
    }
  })

  # outputs
  {
    output$current_file <- renderUI(tags$div(rv$current_file,
      style = paste0(
        "display: inline-block;
        font-size:20pt;
        text-align:center;
        width:70%;
        background: linear-gradient(90deg, #3c8dbc ",
        round(100 * match(rv$current_file, rv$files_names) / length(rv$files_names)),
        "%, white ",
        round(100 * match(rv$current_file, rv$files_names) / length(rv$files_names)),
        "%);"
      )
    ))
    output$current_attribute <- renderUI(tags$div(rv$attributesTable[rv$current_attribute, "attributeName"],
      style = paste0(
        "display: inline-block;
        font-size:15pt;
        text-align:center;
        width:70%;
        background: linear-gradient(90deg, #3c8dbc ",
        round(100 * rv$current_attribute / dim(rv$attributesTable)[1]),
        "%, white ",
        round(100 * rv$current_attribute / dim(rv$attributesTable)[1]),
        "%);"
      )
    ))
  }

  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  observeEvent(input$`nav-save`,
    {
      rv <- saveInput(rv)
    },
    priority = 2
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
    globals, "template"
  )
  callModule(
    prevTab, "nav",
    globals
  )

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
        if (grepl("date", colname)) saved_value <- gsub("^(.*) .*$", "\\1", saved_value)

        # UI
        switch(colname,
          attributeDefinition = textAreaInput(ns(colname),
            value = saved_value,
            "Describe the attribute concisely"
          ),
          class = HTML(paste("<b>Detected class:</b>", as.vector(saved_value))),
          unit = selectInput(ns(colname),
            span("Existing unit", class = "redButton"),
            unique(c(saved_value, globals$FORMAT$UNIT)),
            selected = saved_value
          ),
          dateTimeFormatString = tagList(
            selectInput(ns(paste0(colname, "_date")),
              tags$span("Existing date format",
                class = "redButton"
              ),
              unique(c(saved_value, globals$FORMAT$DATE)),
              selected = saved_value
            ),
            selectInput(
              ns(paste0(colname, "_hour")),
              "Existing hour format",
              globals$FORMAT$HOUR
            )
          ),
          missingValueCode = textInput(ns(colname),
            "Code for missing value",
            value = saved_value
          ),
          missingValueCodeExplanation = textAreaInput(ns(colname),
            "Explain Missing Values",
            value = saved_value
          )
        ) # end of switch
      }) # end of lapply colname
    ) # end of tagList
  }) # end of UI


  # / Servers ----
  observe({
    req(any(rv$ui %in% names(input)))

    sapply(names(rv$attributesTable), function(rvName) {
      # prepare variable
      # two different names: ids from input can be suffixed
      inputNames <- names(input)[
        which(grepl(rvName, names(input)))
      ]
      # 'unit' exception:
      if (rvName == "unit") {
        inputNames <- inputNames[inputNames != "unitType"]
      }

      # check corresponding input
      if (length(inputNames) != 0) {
        if (rvName %in% rv$ui) {
          # show UI
          sapply(inputNames, show)

          # Input UI yet exists: create eventReactive
          rv$attributes[[rvName]] <- eventReactive(
            {
              if (rvName == "dateTimeFormatString") {
                if (is.na(input[[paste0(rvName, "_hour")]])
                || input[[paste0(rvName, "_hour")]] == "NA") {
                  input[[paste0(rvName, "_date")]]
                }
                else {
                  input[[paste0(rvName, "_date")]]
                  input[[paste0(rvName, "_hour")]]
                }
              }
              else {
                input[[rvName]]
              }
            },
            {
              # get input value
              enter <- if (rvName == "dateTimeFormatString") {
                if (is.na(input[[paste0(rvName, "_hour")]])
                || input[[paste0(rvName, "_hour")]] == "NA") {
                  input[[paste0(rvName, "_date")]]
                } else {
                  paste(input[[paste0(rvName, "_date")]],
                    input[[paste0(rvName, "_hour")]],
                    sep = " " # make sure
                  )
                }
              }
              else {
                input[[rvName]]
              }

              # check obtained value
              if (is.list(enter)) {
                enter <- unlist(enter)
              }
              if (!isTruthy(enter)) {
                # message("Input [",rvName,"] is invalid: unchanged")
                enter <- ifelse(isTruthy(unlist(rv$attributesTable[rv$current_attribute, rvName])),
                  unlist(rv$attributesTable[rv$current_attribute, rvName]),
                  ""
                )
              } # end if
              return(enter)
            }
          ) # end of eventReactive
        }
        else {
          # hide UI
          sapply(inputNames, shinyjs::hide)

          # set reactiveValue to the known value
          rv$attributes[[rvName]] <- rv$attributesTable[rv$current_attribute, rvName]
        }
      } # end if inputNames == character(0)
      else {
        rv$attributes[[rvName]] <- rv$attributesTable[rv$current_attribute, rvName]
      }
    }) # end sapply
  })

  # Saves ----
  cpltTrigger <- makeReactiveTrigger()

  # regular saves in savevar - triggered in saveInput()
  observeEvent(
    {
      input$file_prev
      input$file_next
      input$attribute_prev
      input$attribute_next
      input[["nav-nextTab"]]
      input[["nav-prevTab"]]
    },
    {
      # invisible(names(input))
      req(rv$current_file, rv$attributesTable)
      savevar$emlal$templateDP[[rv$current_file]] <- rv$attributesTable
      if (globals$dev) {
        cat("saved", rv$current_file, "\n")
      }
    },
    priority = 2
  )

  # check for completeness
  observe({
    rv$completed <- NULL
    rv$completed <- (
      isTruthy(rv$attributesTable$attributeName)
      && all(sapply(rv$attributesTable$attributeName, isTruthy))
      && all(sapply(rv$attributesTable$class, isTruthy))
      && !any(grepl("!Add.*here!", rv$attributesTable$unit))
      && !any(grepl("!Add.*here!", rv$attributesTable$dateTimeFormatString))
    )
  })
  observe({
    invisible(names(input))
    validate(
      need(!is.null(rv$completed), "rv$completed not defined")
    )
    if (rv$completed) {
      enable("nav-nextTab")
    }
    else {
      disable("nav-nextTab")
    }
  })

  # Process data ----
  observeEvent(input[["nav-nextTab"]],
    {
      req(rv$completed)
      disable("nav-nextTab")

      # for each attribute data frame
      nextStep <- 2
      nextStep <- min(
        sapply(rv$files_names, function(fn) {
          # write filled tables
          cur_ind <- match(fn, rv$files_names)
          path <- savevar$emlal$DPfiles$dp_data_files$metadatapath[cur_ind]
          table <- savevar$emlal$templateDP[[fn]]
          fwrite(table, path)

          # check for direction: customUnits or catvars
          if (nextStep > 0 &&
            "custom" %in% savevar$emlal$templateDP[[fn]][, "unit"]) {
            return(0)
          } # custom units
          else if (nextStep > 1 &&
            "categorical" %in% savevar$emlal$templateDP[[fn]][, "class"]) {
            rv$templateCatvars <- TRUE
            return(1) # categorical variables
          }
          else {
            return(2)
          } # default = geographic Coverage
        })
      )

      # EMLAL: template new fields if needed
      if(rv$templateCatvars)
        template_categorical_variables(
          path = paste(savevar$emlal$selectDP$dp_path,
            savevar$emlal$selectDP$dp_name,
            "metadata_templates",
            sep = "/"
          ),
          data.path = paste(savevar$emlal$selectDP$dp_path,
            savevar$emlal$selectDP$dp_name,
            "data_objects",
            sep = "/"
          )
        )
      
      template_geographic_coverage(
        path = paste(savevar$emlal$selectDP$dp_path,
          savevar$emlal$selectDP$dp_name,
          "metadata_templates",
          sep = "/"
        ),
        data.path = paste(savevar$emlal$selectDP$dp_path,
          savevar$emlal$selectDP$dp_name,
          "data_objects",
          sep = "/"
        ),
        empty = TRUE,
        write.file = TRUE
      )

      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + nextStep

      # browser()
      enable("nav-nextTab")
    },
    priority = 1
  )

  # Output ----
  return(savevar)
}
