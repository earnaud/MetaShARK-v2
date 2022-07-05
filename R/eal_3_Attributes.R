#' @import shiny
#' @importFrom shinyTree shinyTree
#'
#' @noRd
AttributesUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      id = ns("disclaimer"),
      class = "disclaimer",
      tags$h4("Disclaimer"),
      tags$p("(click to dismiss)"),
      "Metadata is partially inferred from your data. Please check the
      guessed attribute values to avoid any mistake."
    ),
    fluidRow(
      ## shinyTree ====
      column(
        4,
        tags$fieldset(
          tags$legend(
            helpLabel(
              "Attribute selection",
              "Here are the attributes of your dataset, matching the
              columns of the data files. Click an attribute to edit it."
            )
          ),
          tags$div(
            shinyTree::shinyTree(
              # TODO add colors
              ns("tree") # ,
              # types = "{ 'red-node': {'a_attr' : { 'style' : 'color:red' }},
              #   'green-node': {'a_attr' : { 'style' : 'color:green' }} }"
            ),
            style = "overflow-y: scroll;"
          )
        )
      ),
      ## Full Form ====
      column(
        7,
        tags$fieldset(
          tags$legend("Attribute description"),
          shinyjs::hidden(
            tags$span(
              id = "file_info",
              class = "inlined",
              ### File name ----
              tags$b("Current file: "), textOutput(ns("filename")),
              ### Manual edit ====
              shinyWidgets::actionBttn(
                inputId = ns("manual_edit"),
                label = "Manual edit",
                style = "fill",
                color = "primary"
              ) |>
                tagAppendAttributes(style = "margin-left: 2em")
            )
          ),

          # Div if no attribute found
          tags$div(
            id = "no_attribute",
            helpLabel(
              helpText("No attribute selected"),
              "Click an attribute in the opposite tree to edit it."
            )
          ),

          # Form to edit a selected attribute
          shinyjs::hidden(
            tags$div(
              id = "form",
              fluidRow(
                column(
                  8,
                  tags$div(
                    style = "width: 100%;",
                    ### attribute name (output) ----
                    tags$h4(textOutput(ns("attribute_name"))),
                    ### attributeDefinition ----
                    textAreaInput(
                      ns("attributeDefinition"),
                      helpLabel(
                        "Description of the attribute",
                        "Precise definition of the attribute which
                                explains the contents of the attribute fully
                                so that a data user could interpret the
                                attribute accurately."
                      ),
                      value = "!Add description here!",
                      resize = "both"
                    ) |>
                      shiny::tagAppendAttributes(style = "width: initial;"),
                    ### class ----
                    selectInput(
                      ns("class"),
                      helpLabel(
                        "Dectected class (change if misdetected)",
                        "Type of content in the attribute: set
                                'categorical' for categorized characters
                                variables, 'character' for free characters
                                variables, 'numeric' for numeric variables and
                                'Date' for time variables."
                      ),
                      choices = c("numeric", "character", "Date", "categorical")
                    ),
                    ### dateTimeFormatString ----
                    selectInput(
                      ns("dateTimeFormatString"),
                      helpLabel("Select a date format", "Common date and time
                                formats in which is expressed the attribute."),
                      choices = c(NA_character_)
                    ),
                    ### unit ----
                    unitsUI(ns("units")),

                    ### missingValueCode ----
                    textInput(
                      ns("missingValueCode"),
                      helpLabel("Code for missing value (1 word)", "This value
                      entered is what is placed into a data table if the value
                      is missing for some reason.")
                    ),
                    ### missingValueCodeExplanation ----
                    textAreaInput(
                      ns("missingValueCodeExplanation"),
                      helpLabel("Explain Missing Values", "The reason causing
                                that there is a missing value."),
                      width = "100%"
                    )
                  ) # end div
                ), # end column
                ## Preview ----
                column(
                  4,
                  tags$div(
                    id = "preview",
                    style = "
                      border-left: 2px solid lightgray;
                      padding: 1em;
                    ",
                    helpLabel(
                      tags$h4("Data Preview"),
                      "Here is displayed the content of the selected attribute."
                    ),
                    tableOutput(ns("preview"))
                  ) # end div
                ) # end column -- preview
              ) # end fluidRow
            ) #  end div
          ) #  end hidden -- attribute metadata
        ) # end fieldset
      ) # end column -- description
    ), # end fluidRow -- page

    ## Custom Units ----
    customUnitsUI(ns("custom_units"))
  )
}

#' @importFrom shinyTree renderTree
Attributes <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    # Dev job ====
    if (main_env$dev) .browse_dev(main_env, 3)

    # Disclaimer ====
    shinyjs::onclick(session$ns("disclaimer"), {
        # animate cause it looks nice
        shinyjs::hide("disclaimer", anim = TRUE, time = 0.25)
      },
      asis = TRUE
    )

    ## Tree ====

    ## Make tree ----
    # initialize empty tree
    output$tree <- shinyTree::renderEmptyTree()
    outputOptions(output, "tree", suspendWhenHidden = FALSE)

    # Update tree with content from setup step
    observeEvent(main_env$EAL$page, {
        req(main_env$EAL$page == 3)
        req(isContentTruthy(main_env$local_rv$tree.content))

        if (main_env$dev) {
          devmsg("update tree", tag = "attributes")
        }

        shinyTree::updateTree(
          session = session,
          treeId = "tree",
          data = main_env$local_rv$tree.content
        )
      },
      priority = -1,
      label = "EAL3: update tree"
    )

    ## Get input from tree ----
    # Setup reactives
    selected_file <- reactiveVal()
    selected_row <- reactiveVal()

    # Observe input
    observeEvent(input$tree, {
        req(main_env$EAL$page == 3)
        req(isContentTruthy(input$tree))

        if (main_env$dev) {
          devmsg("clicked tree", tag = "attributes")
        }

        ### path exploration ----
        # Selected node
        .selected <- if (isContentTruthy(input$tree)) {
          shinyTree::get_selected(input$tree)
        } else {
          NULL
        }
        # Path to selected node
        .ancestor <- if (isContentTruthy(.selected)) {
          attr(.selected[[1]], "ancestry")
        } else {
          NULL
        }

        # get selected file ----
        if (isContentTruthy(.selected)) {
          selected_file(
            if (length(.ancestor) == 0) {
              .selected[[1]][1]
            } else {
              .ancestor
            }
          )
        }

        # change row
        if (isContentTruthy(selected_file())) { # shall be
          selected_row(
            ifelse(
              identical(selected_file(), .selected[[1]][1]),
              # clicked file
              1, # select first attribute
              # clicked attribute
              which(
                main_env$local_rv$md_tables[[selected_file()]]$
                  attributeName == .selected[[1]][1]
              )
            )
          )
        }
      },
      label = "EAL3: click tree"
    )

    ## Display selection ----
    # Show selected filename
    output$filename <- renderText(selected_file())

    # Toggle file info div
    observeEvent(selected_file(), {
      shinyjs::toggle(
        selector = "#file_info",
        condition = isContentTruthy(selected_file())
      )
    })

    # Show selected attributeName
    output$attribute_name <- renderText({
      req(selected_row())

      main_env$local_rv$md_tables[[selected_file()]]$
        attributeName[selected_row()]
    })

    ## On attribute change ----
    observeEvent(input$tree, {
        validate(
          need(isContentTruthy(selected_file()), "Not a valid file"),
          need(isContentTruthy(selected_row()), "Not a valid attribute")
        )

        # Disable temporarily next button
        main_env$EAL$completed <- FALSE

        ## Update form values ----

        .row <- main_env$local_rv$md_tables[[selected_file()]][
          selected_row(),
        ]

        # Set attributeDefinition
        updateTextAreaInput(session, "attributeDefinition",
          value = .row$attributeDefinition
        )
        # Set class
        updateSelectInput(session, "class", selected = .row$class)
        # Set dateTimeFormatString
        updateSelectInput(
          session, "dateTimeFormatString",
          choices = main_env$FORMATS$dates,
          selected = .row$dateTimeFormatString
        )
        # Set unit
        .tmp <- setUnitList(main_env,
          set = if (.row$class == "numeric") .row$unit
        )
        updateSelectInput(
          session, "unit",
          choices = .tmp$unit_list,
          selected = .tmp$set_unit
        )
        # Set missingValueCode
        updateTextInput(session, "missingValueCode",
          value = .row$missingValueCode
        )
        # Set missingValueCodeExplanation
        updateTextAreaInput(session, "missingValueCodeExplanation",
          value = .row$missingValueCodeExplanation
        )

        ## Check validity ----

        # attributeDefinition
        checkFeedback(input, "attributeDefinition", type = "danger")
        # class
        checkFeedback(input, "class", type = "danger")
        # dateTimeFormatString
        checkFeedback(
          input, "dateTimeFormatString",
          condition = if (input$class == "Date") {
            isTruthy(input$dateTimeFormatString) &&
              input$dateTimeFormatString %in% main_env$FORMATS$dates
          } else { # hidden
            TRUE
          },
          type = "danger"
        )
        # unit
        checkFeedback(
          input, "unit",
          condition = if (input$class == "numeric") {
            isTruthy(input$unit) &&
              input$unit %grep% main_env$FORMATS$units
          } else { # hidden
            TRUE
          },
          type = "danger"
        )
        # missingValueCode
        checkFeedback(input, "missingValueCode", type = "warning")
        # missingValueCodeExplanation
        checkFeedback(input, "missingValueCodeExplanation", type = "warning")
      },
      ignoreNULL = FALSE,
      ignoreInit = FALSE,
      priority = -2, # need the reactiveVals to have changed values
      label = "EAL3: update upon attribute change"
    )

    ## Manual edit ====

    ### setup ----

    metadata_editor_ui <- metadataEditorUI(session$ns("manual_edit"))
    metadataEditor("manual_edit", main_env, selected_file)

    ### instantiate observer ----
    observeEvent(input$manual_edit, {
      req(main_env$EAL$page == 3)

      main_env$local_rv$metadata_editor <- DataEditR::dataEditServer(
        "manual_edit-metadata_edit",
        data = reactive(main_env$local_rv$md_tables[[selected_file()]]),
        col_edit = FALSE,
        col_names = FALSE,
        col_factor = FALSE,
        col_options = list(
          class = c("character", "Date", "categorical", "numeric"),
          unit = unlist(main_env$FORMATS$units),
          dateTimeFormatString = main_env$FORMATS$dates
        ),
        row_edit = FALSE,
        quiet = TRUE
      )

      showModal(metadata_editor_ui)
    })

    ### Update UI ----
    # For a clean display
    observeEvent(main_env$EAL$ping, { # ping is sent from metadataEditor
      req(main_env$EAL$page == 3)
      req(main_env$EAL$ping == "update current row")

      # reset ping
      isolate(main_env$EAL$ping <- "")

      .row <- main_env$local_rv$md_tables[[selected_file()]][selected_row(), ]

      # update description
      updateTextAreaInput(session, "attributeDefinition",
        value = .row$attributeDefinition
      )
      # update class
      updateSelectInput(session, "class", selected = .row$class)
      # update dateTimeFormatString
      if (.row$class == "Date") {
        updateSelectInput(session, "dateTimeFormatString",
          selected = .row$dateTimeFormatString
        )
      }
      # update unit
      if (.row$class == "numeric") {
        updateSelectInput(session, "unit", selected = .row$unit)
      }
      # update missingValueCode
      updateTextInput(session, "missingValueCode",
        value = .row$missingValueCode
      )
      # update missingValueCodeExplanation
      updateTextAreaInput(session, "missingValueCodeExplanation",
        value = .row$missingValueCodeExplanation
      )
    })

    ## Form ====

    ### toggle forms -----
    observe({
      shinyjs::toggle(
        selector = "#no_attribute",
        condition = is.null(selected_row())
      )
      shinyjs::toggle(
        selector = "#form",
        condition = !is.null(selected_row())
      )
    })

    ### attributeDefinition ----
    observeEvent(input$attributeDefinition, {
        validate(
          need(isTruthy(selected_file()), "No file selected"),
          need(isTruthy(selected_row()), "No attribute selected")
        )

        # Get value
        main_env$local_rv$md_tables[[selected_file()]]$
          attributeDefinition[selected_row()] <<- input$attributeDefinition

        # Check validity
        checkFeedback(input, "attributeDefinition", type = "danger")
      },
      label = "EAL3: definition input"
    )

    ### class ----
    observeEvent(input$class, {
        validate(
          need(isTruthy(selected_file()), "No file selected"),
          need(isTruthy(selected_row()), "No attribute selected"),
          need(isTruthy(input$class), "Invalid value for class"),
          need(
            input$class %in% c("character", "categorical", "numeric", "Date"),
            "Value not found for class"
          )
        )

        # Get value
        .row <- selected_row()

        main_env$local_rv$md_tables[[selected_file()]]$class[.row] <<- input$class

        #### Check DateTimeFormatString ----
        shinyjs::toggle(
          "dateTimeFormatString",
          condition = input$class == "Date"
        )
        if (input$class != "Date") { # erase dtfs if changed from Date to other
          updateSelectInput(
            session,
            "dateTimeFormatString",
            selected = ""
          )
          main_env$local_rv$md_tables[[selected_file()]]$
            dateTimeFormatString[selected_row()] <- ""
        } else { # check dtfs if set to Date
          .dtfs <- main_env$local_rv$md_tables[[selected_file()]]$
            dateTimeFormatString[selected_row()]
          # set default if invalid
          if (isFALSE(.dtfs %in% main_env$FORMATS$dates)) {
            .dtfs <- main_env$FORMATS$dates[1]
          } # YYYY-MM-DD
          # update dtfs input
          updateSelectInput(
            session,
            "dateTimeFormatString",
            selected = .dtfs
          )
          main_env$local_rv$md_tables[[selected_file()]]$
            dateTimeFormatString[selected_row()] <- .dtfs
        }

        ### Check units ----
        shinyjs::toggle("unit_div", condition = input$class == "numeric")
        if (input$class != "numeric") {
          updateSelectInput(session, "unit", selected = "")
          main_env$local_rv$md_tables[[selected_file()]]$
            unit[selected_row()] <- ""
        } else {
          .unit <- main_env$local_rv$md_tables[[selected_file()]]$
            unit[selected_row()]
          .tmp <- setUnitList(
            main_env,
            set = optional(.unit, default = "dimensionless")
          )
          updateSelectInput(
            session,
            "unit",
            choices = .tmp$unit.list,
            selected = .tmp$set.unit
          )
          main_env$local_rv$md_tables[[selected_file()]]$
            unit[selected_row()] <- .tmp$set.unit
        }

        # Check validity
        checkFeedback(input, "class", type = "danger")
      },
      label = "EAL3: class input"
    )

    ### dateTimeFormatString ----
    observeEvent(input$dateTimeFormatString, {
        req(main_env$EAL$page == 3)

        validate(
          need(isTruthy(selected_file()), "No file selected"),
          need(isTruthy(selected_row()), "No attribute selected"),
          need(input$class == "Date", "Not a Date"),
          need(
            !is.na(input$dateTimeFormatString),
            "Unset dateTimeFormatString input."
          )
        )

        # Correct input value
        .value <- input$dateTimeFormatString
        # TODO add date choices input
        if (isFALSE(.value %in% main_env$FORMATS$dates)) {
          .value <- main_env$FORMATS$dates[1] # YYYY-MM-DD
          updateSelectInput(
            session,
            "dateTimeFormatString",
            selected = .value
          )
        }

        main_env$local_rv$md_tables[[selected_file()]]$
          dateTimeFormatString[selected_row()] <- .value
        # main_env$local_rv$md_tables[[selected_file()]] <<- replaceValue(
        #   main_env$local_rv$md_tables[[selected_file()]],
        #   selected_row(),
        #   "dateTimeFormatString",
        #   .value
        # )

        # Check validity
        checkFeedback(
          input,
          "dateTimeFormatString",
          condition = if (input$class == "Date") {
            isTruthy(input$dateTimeFormatString) &&
              .value %in% main_env$FORMATS$dates
          } else {
            TRUE
          },
          type = "danger"
        )
      },
      label = "EAL3: dateTimeFormatString input"
    )

    ### unit ----
    units("units", main.env, selected_file, selected_attribute, selected_class)
    
    ### missingValueCode ----
    observeEvent(input$missingValueCode, {
        validate(
          need(isTruthy(selected_file()), "No file selected"),
          need(isTruthy(selected_row()), "No attribute selected")
        )

        .value <- input$missingValueCode

        # Limit to 1 word
        if (grepl(".+ +.*", .value)) {
          # Shorten
          .value <- strsplit(
            gsub("^ +", "", .value),
            split = " "
          )[[1]][1]
          # Update input
          updateTextInput(
            session,
            "missingValueCode",
            value = .value
          )
          # Send notification to user
          showNotification(
            id = "mvc_update",
            ui = tagList(
              tags$code("missingValueCode"),
              "fields are limited to a",
              tags$b("single word.")
            ),
            duration = 3,
            type = "warning"
          )
        }

        main_env$local_rv$md_tables[[selected_file()]] <<- replaceValue(
          main_env$local_rv$md_tables[[selected_file()]],
          selected_row(),
          "missingValueCode",
          .value
        )

        # Check validity
        checkFeedback(
          input,
          "missingValueCode",
          type = if (identical(
            isContentTruthy(.value),
            isContentTruthy(input$missingValueCodeExplanation)
          )) {
            "warning"
          } else {
            "danger"
          }
        )

        if (isContentTruthy(input$missingValueCode)) {
          checkFeedback(input, "missingValueCodeExplanation", type = "danger")
        } else {
          checkFeedback(input, "missingValueCodeExplanation", type = "warning")
        }
      },
      label = "EAL3: missing value code"
    )

    ### missingValueCodeExplanation ----
    observeEvent(input$missingValueCodeExplanation, {
        validate(
          need(isTruthy(selected_file()), "No file selected"),
          need(isTruthy(selected_row()), "No attribute selected")
        )

        .value <- input$missingValueCodeExplanation
        main_env$local_rv$md_tables[[selected_file()]] <<- replaceValue(
          main_env$local_rv$md_tables[[selected_file()]],
          selected_row(),
          "missingValueCodeExplanation",
          .value
        )

        # Check validity
        checkFeedback(
          input,
          "missingValueCodeExplanation",
          type = if (identical(
            isContentTruthy(.value),
            isContentTruthy(input$missingValueCode) ||
              input$missingValueCode == ""
          )) {
            "warning"
          } else {
            "danger"
          }
        )

        checkFeedback(
          input,
          "missingValueCode",
          type = if (identical(
            isContentTruthy(.value),
            isContentTruthy(input$missingValueCode) ||
              input$missingValueCode == ""
          )) {
            "warning"
          } else {
            "danger"
          }
        )
      },
      label = "EAL3: missing value code explanation"
    )

    ## Preview ====
    output$preview <- renderTable({
      req(isContentTruthy(selected_file()))
      req(isContentTruthy(selected_row()))

      validate(
        need(
          isContentTruthy(main_env$local_rv$
            preview[[selected_file()]][[selected_row()]]),
          "Empty preview colum."
        )
      )

      if (main_env$dev) {
        devmsg("render preview", tag = "attributes")
      }

      main_env$local_rv$preview[[selected_file()]][[selected_row()]] |>
        as.character() |>
        enc2utf8() |>
        as.data.frame() |>
        setNames(
          nm = main_env$local_rv$md_tables[[selected_file()]]$
            attributeName[selected_row()]
        )
    })

    # Completeness ====
    observeEvent({
        input$attributeDefinition
        input$class
        input$dateTimeFormatString
        input$unit
        input$missingValueCode
        input$missingValueCodeExplanation
      }, {
        req(main_env$EAL$page == 3)

        # Upon any input change, check attribute's completeness
        main_env$local_rv$completed[[selected_file()]][[
        # use the attributeName of selected row
        main_env$local_rv$md_tables[[selected_file()]]$
          attributeName[selected_row()]
        ]] <- (
          isContentTruthy(input$attributeDefinition) &&
            isContentTruthy(input$class) &&
            input$class %in% c("character", "numeric", "Date", "categorical") && (
            if (input$class == "Date") {
              isContentTruthy(input$dateTimeFormatString) &&
                input$dateTimeFormatString %in% main_env$FORMATS$dates
            } else {
              TRUE
            }
          ) && (
            if (input$class == "numeric") {
              isContentTruthy(input$unit) &&
                input$unit %in% c(
                  main_env$local_rv$custom.units$table$id,
                  unlist(main_env$FORMATS$units)
                )
            } else {
              TRUE
            }
          ) && (
            if (input$missingValueCodeExplanation != "") {
              isContentTruthy(input$missingValueCode) &&
                isContentTruthy(input$missingValueCodeExplanation)
            } else {
              (input$missingValueCode == "")
            }
          )
        )

        # Update whole completeness
        main_env$EAL$completed <- main_env$local_rv$completed |>
          listReactiveValues() |>
          unlist() |>
          all()
      },
      label = "EAL3: completeness",
      priority = -2
    )

    # Update main_env tag_list ====
    observe({
        req(main_env$EAL$page == 3)
        # make a call
        main_env$EAL$completed

        # If any problem, tell the user
        .checked_attributes <- which(
          main_env$local_rv$completed |>
            isFALSE() |>
            listReactiveValues() |>
            unlist()
        ) |>
          names() |>
          gsub(pattern = "\\.txt\\.", replacement = ".txt: ") |>
          paste(collapse = "\n", sep = "\n")
        if (.checked_attributes != "") {
          .checked_attributes <- tagList(
            tags$b("Invalid attributes:"),
            tags$br(),
            .checked_attributes
          )
        }
        main_env$local_rv$tag_list$completed <- .checked_attributes

        # next step tag
        if (main_env$local_rv$use_catvars()) {
          main_env$local_rv$tag_list$next.step <- tagList()
        } else {
          main_env$local_rv$tag_list$next.step <- tagList(
            tags$br(),
            "No categorical variables found: will skip to geographic coverage."
          )
        }

        # assemble
        main_env$EAL$tag_list <- tagList(
          listReactiveValues(main_env$local_rv$tag_list)
        )
      },
      priority = -1,
      label = "tag_list update"
    )

    # (End of Attributes) ====
  })
}
