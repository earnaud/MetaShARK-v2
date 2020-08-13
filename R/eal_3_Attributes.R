#' @title Data Package Template filling
#'
#' @description UI part of the Attributes module. Fill in the attributes of the data package
#'
#' @import shiny
#'
#' @noRd
AttributesUI <- function(id, main.env) {
  ns <- NS(id)

  return(
    fluidPage(
      tagList(
        "Even if EML Assembly Line automatically infers most
        of your data's metadata, some steps need you to check
        out. Please check the following attribute, and fill
        in at least the", withRedStar("mandatory elements.")
      ),
      # Attributes
      fluidRow(
        column(
          1,
          actionButton(
            NS(id, "file_prev"),
            "",
            icon("chevron-left")
          )
        ),
        column(
          10,
          uiOutput(NS(id, "current_file"),
            inline = TRUE
          )
        ),
        column(
          1,
          actionButton(
            NS(id, "file_next"),
            "",
            icon("chevron-right")
          )
        )
      ),
      fluidRow(
        column(
          12,
          uiOutput(NS(id, "edit_attributes"))
        )
      ),
      # Custom Units
      tags$h4("Custom Units"),
      fluidRow(
        tableOutput(NS(id, "CUUI"))
      )
    ) # end fluidPage
  ) # end return
}

#' @importFrom data.table fwrite
#' @import shiny
#' @importFrom shinyjs hide show enable disable onclick
#' @importFrom EMLassemblyline template_categorical_variables template_geographic_coverage
#' @importFrom shinyBS bsCollapse bsCollapsePanel updateCollapse
#'
#' @noRd
Attributes <- function(id, full.id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Quick ----
    if (isTRUE(main.env$dev) || isTRUE(main.env$save.variable$quick)) {
      .fill <- function(rv = rv) {
        lapply(seq(main.env$local.rv$tables), function(ind) {
          .table <- main.env$local.rv$tables[[ind]]
          sapply(colnames(.table), function(col) {
            # Set values
            if (col == "attributeDefinition") {
              main.env$local.rv$tables[[ind]][[col]] <- paste("Description for", main.env$local.rv$tables[[ind]][["attributeName"]])
            }
            if (col %in% c("missingValueCodeExplanation", "missingValueCode")) {
              main.env$local.rv$tables[[ind]][[col]] <- rep("LoremIpsum", dim(.table)[1])
            }
            if (col == "dateTimeFormatString") {
              .dat.row <- which(main.env$local.rv$tables[[ind]]$class == "Date")
              main.env$local.rv$tables[[ind]][[col]] <- rep("", dim(.table)[1])
              if (isTruthy(.dat.row)) {
                main.env$local.rv$tables[[ind]][.dat.row, col] <- rep(main.env$FORMATS$dates[3], length(.dat.row))
              }
            }
            if (col == "unit") {
              .uni.row <- which(main.env$local.rv$tables[[ind]]$class == "numeric")
              main.env$local.rv$tables[[ind]][[col]] <- rep("", dim(.table)[1])
              if (isTruthy(.uni.row)) {
                main.env$local.rv$tables[[ind]][.uni.row, col] <- rep(main.env$FORMATS$dates[2], length(.uni.row))
              }
            }

            # Update values
            if (ind == main.env$local.rv$current.file) {
              sapply(1:dim(main.env$local.rv$tables[[ind]])[1], function(item) {
                inputId <- paste(ind, item, col, sep = "-")
                if (inputId %in% names(input)) {
                  if (col %in% c("unit", "dateTimeFormatString")) {
                    updateSelectInput(session, inputId, selected = main.env$local.rv$tables[[ind]][item, col])
                  }
                  if (col %in% c("attributeDefinition", "missingValueCode", "missingValueCodeExplanation")) {
                    updateTextAreaInput(session, inputId, value = main.env$local.rv$tables[[ind]][item, col])
                  }
                }
              })
            }
          }) # end of sapply

          # Update current table
          if (ind == main.env$local.rv$current.file) {
            main.env$local.rv$current.table <- main.env$local.rv$tables[[ind]]
          }
        }) # end of lapply
        return(rv)
      } # end of .fill
    }

    # variable initialization ----

    # Set a bunch of local variables when loading this page
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page == 3)
      
      req(main.env$save.variable$DataFiles)
      # Path to data files
      main.env$local.rv$data.filepath <- main.env$save.variable$DataFiles$datapath
      main.env$local.rv$current.file <- as.numeric(checkTruth(main.env$local.rv$data.filepath))

      # Path to metadata templates
      main.env$local.rv$filepath <- main.env$save.variable$DataFiles$metadatapath
      if (checkTruth(main.env$local.rv$filepath)) {
        main.env$local.rv$filenames <- basename(main.env$local.rv$filepath)
        main.env$local.rv$tables <- lapply(
          main.env$local.rv$filepath,
          readDataTable,
          data.table = FALSE, stringsAsFactors = FALSE
        )
        main.env$local.rv$current.table <- main.env$local.rv$tables[[main.env$local.rv$current.file]]
      }

      # Path to metadata directory
      if (checkTruth(main.env$save.variable$SelectDP$dp.metadata.path)) {
        main.env$local.rv$cu.table <- readDataTable(
          dir(
            isolate(main.env$save.variable$SelectDP$dp.metadata.path),
            pattern = "ustom",
            full.names = TRUE
          ),
          stringsAsFactors = FALSE
        )
      }
    },
    label = "EAL3: set values"
    )

    # if (checkTruth(main.env$save.variable$Attributes$annotations)) {
    #   rv$annotations$values <- isolate(main.env$save.variable$Attributes$annotations)
    #   rv$annotations$count <- nrow(rv$annotations$values)
    # }
    # else {
    #   rv$annotations$values <- data.frame(
    #     id = character(),
    #     element = character(),
    #     context = character(),
    #     subject = character(),
    #     predicate_label = character(),
    #     predicate_uri = character(),
    #     object_label = character(),
    #     object_uri = character(),
    #     stringsAsFactors = FALSE
    #   )
    # }

    observeEvent(main.env$save.variable$quick,
      {
        # Selected quick selection and not gone further
        if (isTRUE(main.env$save.variable$quick) &&
          length(main.env$save.variable$EAL$history) < 4) {
          main.env$local.rv <- .fill(main.env$local.rv)
        }
      },
      once = TRUE
    )

    # Update unit list with customs
    observe(
      {
        .tab <- isolate(main.env$local.rv$current.table) # trigger
        req(.tab)
        .tmp <- unique(c(
          unlist(lapply(seq_along(main.env$local.rv$tables), function(t) {
            if (t == main.env$local.rv$current.file) {
              return(.tab$unit)
            } else {
              return(main.env$local.rv$tables[[t]]$unit)
            }
          })),
          main.env$FORMATS$units
        ))
        main.env$local.rv$units.list <- .tmp[.tmp != ""]
      },
      label = "EAL3: unit list"
    )

    # Reactive triggers
    curt <- makeReactiveTrigger()

    # List of observers
    obs <- reactiveValues()

    # Navigation buttons ----
    shinyjs::onclick("file_prev", {
      req(main.env$local.rv$current.file > 1)
      # Save
      if (!is.null(main.env$local.rv$current.table)) {
        main.env$local.rv$tables[[main.env$local.rv$current.file]] <- main.env$local.rv$current.table
      }
      # Change file
      main.env$local.rv$current.file <- main.env$local.rv$current.file - 1
    })

    shinyjs::onclick("file_next", {
      req(main.env$local.rv$current.file < length(main.env$local.rv$filenames))
      # Save
      if (!is.null(main.env$local.rv$current.table)) {
        main.env$local.rv$tables[[main.env$local.rv$current.file]] <- main.env$local.rv$current.table
      }
      # Change file
      main.env$local.rv$current.file <- main.env$local.rv$current.file + 1
    })

    # update table
    observeEvent(main.env$local.rv$current.file,
      {
        req(main.env$local.rv$current.file > 0)
        main.env$local.rv$current.table <- main.env$local.rv$tables[[main.env$local.rv$current.file]]
        main.env$local.rv$current.table[is.na(main.env$local.rv$current.table)] <- ""
        main.env$local.rv$current.preview <- readDataTable(
          main.env$local.rv$data.filepath[main.env$local.rv$current.file],
          stringsAsFactors = FALSE,
          nrows = 5
        )
      },
      label = "EAL3: update table",
      priority = 1
    )

    # display
    output$current_file <- renderUI(
      tags$div(
        main.env$local.rv$filenames[main.env$local.rv$current.file],
        class = "ellipsis",
        style = paste0(
          "display: inline-block;
          font-size:14pt;
          text-align:center;
          width:100%;
          background: linear-gradient(90deg, #3c8dbc ",
          round(100 * main.env$local.rv$current.file / length(main.env$local.rv$filenames)),
          "%, white ",
          round(100 * main.env$local.rv$current.file / length(main.env$local.rv$filenames)),
          "%);"
        )
      )
    )

    # * UI ----
    observeEvent(main.env$local.rv$current.file, {
      req(checkTruth(main.env$local.rv$current.table))
      .current.table <- main.env$local.rv$current.table

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
              function(row.index, fields) {
                # prepare variables
                attribute.row <- .current.table[row.index, ]

                return(
                  bsCollapsePanel(
                    title = attribute.row[fields[1]],
                    tagList(
                      column(
                        9,
                        # Input ====
                        lapply(fields[-1], function(colname) {
                          # prepare var
                          saved.value <- .current.table[row.index, colname]
                          inputId <- paste(
                            isolate(main.env$local.rv$current.file),
                            row.index,
                            sep = "-"
                          )

                          # GUI
                          attributeInputUI(
                            NS(id, inputId),
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
                          NS(id, paste0(
                            "preview-",
                            colnames(main.env$local.rv$current.preview)[row.index]
                          ))
                        ),
                        tags$hr(),
                        # Annotate ====
                        # tags$div(
                        #   annotateUI(
                        #     NS(id, paste(
                        #       "annotate",
                        #       isolate(rv$current.file),
                        #       row.index,
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
            ), # end of lapply : row.index
            id = NS(id, "collapse")
          )
        )
        return(ui)
      })
    },
    label = "EAL3: set UI"
    ) # end of observeEvent

    # * Server ----
    observeEvent(main.env$local.rv$current.file, {
      req(checkTruth(main.env$local.rv$current.table))

      sapply(
        seq(dim(main.env$local.rv$current.table)[1]),
        fields = colnames(main.env$local.rv$current.table)[-1], # not Attribute Name
        function(row.index, fields) {

          # TODO Update style: to correct
          #   updateCollapse(
          #     session = session,
          #     NS(id, "collapse"),
          #     style = filled
          #   )

          # Preview ====
          preview.column <- colnames(main.env$local.rv$current.preview)[row.index]
          output[[paste0("preview-", preview.column)]] <- renderTable(main.env$local.rv$current.preview[[preview.column]])

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
          #   save.variable, main.env, rv, row.index
          # )

          # Input ====
          lapply(fields, function(colname) {
            inputId <- paste(
              isolate(main.env$local.rv$current.file),
              row.index,
              sep = "-"
            )

            obs <- attributeInput(
              inputId,
              rv,
              row.index,
              colname,
              obs,
              curt
            )
          }) # end of lapply colname
        } # end of *in situ* function
      ) # end of sapply : row.index
    },
    label = "EAL3: set server"
    ) # end of observeEvent

    # Custom units ----
    observe({
      curt$depend()
      .current.table <- main.env$local.rv$current.table
      modal.on <- isolate(main.env$local.rv$modal.on)

      req(isTruthy(.current.table))
      req(any(.current.table$unit == "custom"))

      main.env$local.rv$unit.id <- c(
        main.env$local.rv$current.file,
        which(.current.table$unit == "custom"),
        "unit"
      )

      row <- main.env$local.rv$unit.id[2]
      class <- .current.table[row, "class"]

      if (class == "numeric" &&
        modal.on == FALSE) {
        main.env$local.rv$cu.values <- main.env$local.rv$cu.table %>%
          dplyr::filter(grepl(class, id))
        if (any(dim(main.env$local.rv$cu.values) == 0)) {
          main.env$local.rv$cu.values <- rep(NA, 5)
        }

        showModal(CU_Modal(main.env$local.rv$cu.values, cu.table = main.env$local.rv$cu.table))

        main.env$local.rv$modal.on <- TRUE

        isolate({
          main.env$local.rv$current.table[row, "unit"] <- ""
        })
      }
    },
    label = "EAL3: observe CU"
    )

    CU_Modal <- function(values = rep(NA, 5), cu.table = NULL) {
      modalDialog(
        title = "Custom Unit",
        tagList(
          # id
          fluidRow(
            column(6,
              offset = 3,
              textInput(
                NS(id, "modal_id"),
                label = withRedStar("Unit identifier"),
                placeholder = "e.g. milligramsPerGram",
                value = if (!is.na(values[1])) values[1] else NULL
              ),
              # unitType
              textInput(
                NS(id, "modal_unitType"),
                label = withRedStar("Physical property types the unit belongs to"),
                placeholder = "e.g. mass",
                value = if (!is.na(values[2])) values[2] else NULL
              ),
              # ParentSI
              selectInput(
                NS(id, "modal_parentSI"),
                label = withRedStar("Parent unit in SI"),
                choices = main.env$FORMATS$units[-1],
                selected = if (!is.na(values[3])) values[3] else NULL
              ),
              # MultiplierToSI
              numericInput(
                NS(id, "modal_multiplier"),
                label = withRedStar("Numeric multiplier computed from Parent unit in SI"),
                value = 1,
              ),
              # Description
              textAreaInput(
                NS(id, "modal_description"),
                label = withRedStar("Unit description"),
                placeholder = "e.g. milligrams per gram",
                value = if (!is.na(values[5])) values[5] else NULL
              )
            )
          ) # end of fluidRow
        ),
        easyClose = FALSE,
        footer = tagList(
          actionButton(NS(id, "modal_cancel"), "Cancel"),
          actionButton(NS(id, "modal_submit"), "Submit")
        )
      )
    }

    # * CU server ----
    # Cancel
    onclick("modal_cancel", {
      req(isTRUE(main.env$local.rv$modal.on))

      # Close modal
      main.env$local.rv$modal.on <- FALSE
      removeModal()

      isolate({
        updateSelectInput(
          session,
          paste(main.env$local.rv$unit.id, collapse = "-"),
          selected = main.env$FORMATS$units[2]
        )
      })
      main.env$local.rv$unit.id <- character() # reset to default
    })

    # Submit button en/disable
    observe({
      req(isTRUE(main.env$local.rv$modal.on))

      # type a new one
      if (!input$modal_id %in% main.env$local.rv$cu.table$id &&
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
    },
    label = "EAL3: set CU server")

    # Submit
    observeEvent(input$modal_submit, {
      req(isTRUE(main.env$local.rv$modal.on))
      
      # Close modal
      removeModal()
      main.env$local.rv$modal.on <- FALSE
      
      isolate({
        main.env$local.rv$cu.values <- c(
          input$modal_id,
          input$modal_unitType,
          input$modal_parentSI,
          input$modal_multiplier,
          input$modal_description
        )
      })
      
      # Update CU values
      if (main.env$local.rv$cu.values[1] %in% main.env$local.rv$cu.table$id) {
        main.env$local.rv$cu.table <- main.env$local.rv$cu.table %>%
          dplyr::filter(id = main.env$local.rv$cu.values[1]) %>%
          base::replace(values = main.env$local.rv$cu.values)
      } # Add CU values
      else {
        names(main.env$local.rv$cu.values) <- colnames(main.env$local.rv$cu.table)
        main.env$local.rv$cu.table[dim(main.env$local.rv$cu.table)[1] + 1, ] <- main.env$local.rv$cu.values
      }
      # update input UI
      main.env$local.rv$units.list <- unique(c(
        main.env$local.rv$cu.values["id"],
        main.env$local.rv$units.list
      ))
      isolate({
        updateSelectInput(
          session,
          paste(main.env$local.rv$unit.id, collapse = "-"),
          choices = main.env$local.rv$units.list,
          selected = main.env$local.rv$cu.values["id"]
        )
      })
      
      row <- main.env$local.rv$unit.id[2]
      main.env$local.rv$current.table[row, "unit"] <- main.env$local.rv$cu.values["id"]
    },
      label = "EAL3: submit CU",
      priority = 1
    )

    output$CUUI <- renderTable({
      validate(
        need(isTruthy(unlist(main.env$local.rv$cu.table)), "No custom units registered")
      )
      main.env$local.rv$cu.table
    })

    # Saves ----
    # observeEvent(rv$tables, {
    observe({
      req(main.env$EAL$page == 3)
      
      main.env$EAL$completed <- FALSE
      req(
        length(main.env$local.rv$tables) != 0 &&
          !any(sapply(main.env$local.rv$tables, identical, y = data.frame()))
      )
      
      main.env$EAL$completed <- all(
        unlist(
          lapply(
            main.env$local.rv$tables,
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
    label = "EAL3: continuous save"
    )

    # en/disable buttons
    observeEvent(main.env$local.rv$current.file, {
      req(
        isTruthy(names(input)) &&
          isTruthy(names(main.env$local.rv$current.file))
      )

      if (main.env$local.rv$current.file == 1) {
        shinyjs::disable("file_prev")
      } else {
        shinyjs::enable("file_prev")
      }
      if (main.env$local.rv$current.file == length(main.env$local.rv$filenames)) {
        shinyjs::disable("file_next")
      } else {
        shinyjs::enable("file_next")
      }
    },
    label = "EAL3: en/disable buttons"
    )

    # Process data ----
    observeEvent(main.env$EAL$.next,
      {
        req(main.env$EAL$current == "Attributes")

        withProgress({
          setProgress(0.5, "Saving metadata")

          saveReactive(main.env)

          # for each attribute data frame
          setProgress(0.8, "Importing catvar templates")
          .do.template.catvars <- sapply(
            seq_along(main.env$local.rv$filenames),
            function(cur_ind) {

              # check for direction: CustomUnits or CatVars
              return(isTRUE("categorical" %in% main.env$local.rv$tables[[cur_ind]][, "class"]))
            }
          ) %>%
            unlist() %>%
            any()

          # EMLAL: template new fields if needed
          if (isTRUE(.do.template.catvars)) {
            try(
              EMLassemblyline::template_categorical_variables(
                path = main.env$save.variable$SelectDP$dp.metadata.path,
                data.path = main.env$save.variable$SelectDP$dp.data.path
              )
            )
          }

          setProgress(0.9, "Templating geographic coverage")
          try(
            EMLassemblyline::template_geographic_coverage(
              path = main.env$save.variable$SelectDP$dp.metadata.path,
              data.path = main.env$save.variable$SelectDP$dp.data.path,
              empty = TRUE,
              write.file = TRUE
            )
          )

          if (isFALSE(.do.template.catvars)) {
            isolate(main.env$EAL$page <- main.env$EAL$page + 1)
          }
          incProgress(0.1)
        })
      },
      label = "EAL3: process data",
      priority = 1,
      ignoreInit = TRUE
    )
  })
}
