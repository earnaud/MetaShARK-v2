#' @noRd
build_attributes_tree <- function(main_env) {
  if (main_env$EAL$page == 3) {
    .tables <- isolate(main_env$local_rv$md.tables)
    req(isContentTruthy(.tables))

    devmsg("compute tree", tag = "attributes")

    lapply(
      names(.tables),
      # Files node
      function(file.name) {
        structure(lapply(
          .tables[[file.name]]$attributeName,
          # Attributes leaves
          file.name = file.name,
          function(attribute_name, file.name) {
            # Render leaf
            structure(
              attribute_name,
              # sttype="default",
              sticon = "fa fa-table"
            )
          }
        ) |>
          setNames(nm = .tables[[file.name]]$attributeName),
        stopened = TRUE,
        # sttype = "root",
        sticon = "fa fa-file"
        )
      }
    ) |>
      setNames(nm = names(.tables))
  } else {
    list()
  }
}

#' @noRd
replace_value <- function(table, attribute_name, field, new_value) {
  x <- try({
    row <- which(table$attributeName == attribute_name)
    col <- field
    table[row, col] <- new_value
  })

  return(table)
}

#' @noRd
setUnitList <- function(main_env, set = NULL) {

  # Flat list
  choices <- c({
      .tmp <- main_env$local_rv$custom_units$table$id
      if (isTruthy(.tmp)) {
        names(.tmp) <- "custom"
      } else {
        .tmp <- NULL
      }
      .tmp
    },
    main_env$FORMATS$units
  )

  # Input format
  types <- unique(names(choices[
    sapply(choices, function(c) length(c) != 1 && !is.null(c))
  ]))
  out <- choices[sapply(choices, length) == 1]
  if (!is.null(out$custom)) {
    out <- c(custom = out$custom, out[names(out) != "custom"])
  }
  # prepare actual list of choices - with custom units if any
  sapply(types, function(type) {
    out[[type]] <<- unname(choices[which(names(choices) == type)])[[1]]
  })

  # Correct value set for updates
  if (!is.null(set)) {
    set <- set |>
      setNames(nm = names(out)[
        which(sapply(
          out,
          function(.ul) {
            set %in% .ul
          }
        ))
      ]) |>
      as.list()
  }

  # Output
  return(list(
    unit.list = out,
    set.unit = set
  ))
}

# Custom Units ====
#' @import shiny
#'
#' @noRd
customUnitsUI <- function(id) {
  ns <- NS(id)

  shinyjs::hidden(
    tags$div(
      id = "custom_units_div",
      tags$hr(),
      tags$h4("Custom units"),
      fluidRow(
        # Input ----
        column(4, DataEditR::dataEditUI(ns("edit"))),
        # Feedback ----
        column(
          4,
          # # Unused custom unit -- does not appear in metadata tables
          uiOutput(ns("feedback")),
          # "Unused custom unit: %s",
          # # Empty cells -- except for full empty rows
          # "All information must be filled.",
          # # Duplicated ID
          # "Duplicated ID are not allowed",
          # # Invalid unitType
          # # select among SI (+ SI-derived?)
          # sprintf(
          #   "Invalid unitType:%s",
          #   paste(.invalid.types, collapse = ", ")
          # ),
          selectInput(
            ns("unitType"),
            helpLabel(
              "Select a SI unit",
              "This SI unit is used as a base for your custom unit
              (e.g. mol is the parent unit for mol/kg)"
            ),
            choices = NULL
          )
        ),
        # Help ----
        column(
          4,
          tagList(
            tags$h5("Filling Custom Units"),
            tags$ul(
              tags$li("id: which name shall be given to the custom unit
                      (make it unique)"),
              tags$li("unitType: which type of value is this unit giving measure
                      (e.g. mass)"),
              tags$li("parentSI: which SI is this unit derived from (if any)"),
              tags$li("multiplierToSI: by which amount is the original SI
                      multiplied to obtain this unit"),
              tags$li("description: any description of the unit, its motivation,
                      related methods ...")
            )
          )
        )
      )
    )
  ) # end of hidden
}

#' @import shiny
#' @importFrom shinyjs onclick toggleState
#' @importFrom dplyr filter
#'
#' @noRd
customUnits <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    # Setup ----
    # setup a new server each time page 3 is up
    observeEvent(main_env$EAL$page, {
      req(main_env$EAL$page == 3)

      DataEditR::dataEditServer(
        "custom_units",
        data = reactive(main_env$local_rv$custom_units$table),
        col_edit = FALSE,
        # col_options ?
        col_names = FALSE,
        quiet = TRUE
      )
    })

    # Display ----
    # display custom units div if any to be filled
    observeEvent({
        input$add_custom_units
        main_env$local_rv$custom_units$table
      }, {
        req(main_env$EAL$page == 3)

        shinyjs::toggle(
          selector = "#^custom_units_div",
          condition = nrow(main_env$local_rv$custom_units$table) > 0
        )
      },
      priority = -1,
      label = "EAL3: show custom units"
    )

    # Manage input ----
    observeEvent(input$`custom_units-x`, {
      req(main_env$EAL$page == 3)
      browser()
      ## Save ----
      # For now, save all table + shortcut
      main_env$local_rv$custom_units$table <- .table <- input$`custom_units-x`

      # Feedback ----

      # point empty cells -- except full empty rows which will be removed later
      .nemptycells <- sum(.table == "")
      if (.nemptycells > 0) {
        sprintf("%s cells are empty.", .nemptycells)
      }
      # point invalid content

      # offer the unit list once again to help
      # point duplicated ID
    })
  })
}

# Manual editing ====
#' @import shiny
#' @noRd
metadataEditorUI <- function(id) {
  ns <- NS(id)

  modalDialog(
    title = "Manual edit",
    tagList(
      tags$p("This panel supports features similar to a spreadsheet,
             like copy-pasting over multiple rows."),
      fluidRow(
        column(9, DataEditR::dataEditUI(ns("metadata_edit"))),
        column(3, uiOutput(ns("errors")))
      )
    ),
    size = "l",
    footer = tagList(
      actionButton(ns("validate"), "Save and close")
    )
  )
}

#' @import shiny
#' @noRd
metadataEditor <- function(id, main_env, selected_file) {
  moduleServer(id, function(input, output, session) {
    manual_edit_errors <- reactiveVal("")

    ## manage input ----
    observeEvent(input$`metadata_edit-x`, {
        req(main_env$EAL$page == 3)
        .content <- main_env$local_rv$metadata.editor()
        .errors <- list()

        ### check emptiness ----
        # attributeName
        .errors <- manualEditCheck(.content["attributeName"], .errors)
        # attributeDefinition
        .errors <- manualEditCheck(.content["attributeDefinition"], .errors)
        # class
        .errors <- manualEditCheck(.content["class"], .errors)

        ### check invalid values ----
        # class
        .test <- sapply(
          .content$class, `%in%`,
          c("character", "categorical", "numeric", "Date")
        )
        if (isFALSE(all(.test))) { # any is false
          .row.ind <- which(!.test)
          .errors[[length(.errors) + 1]] <- HTML(
            sprintf(
              "Invalid values for <code>class</code> at l. %s.",
              paste(.row.ind, collapse = ", ")
            )
          )
        }
        # dateTimeFormatString
        .test <- sapply(seq_along(.content), function(i) {
          if (.content$class[i] != "date") {
            TRUE
          } else { # only test dates
            .content$dateTimeFormatString[i] %in% main_env$FORMATS$dates
          }
        })
        if (isFALSE(all(.test))) { # add error if any is false
          .row.ind <- which(!.test)
          .errors[[length(.errors) + 1]] <- HTML(
            sprintf(
              "Invalid values for <code>dateTimeFormatString</code> at l. %s.",
              paste(.row.ind, collapse = ", ")
            )
          )
        }
        # unit
        .test <- sapply(seq_along(.content$unit), function(i) {
          if (.content$class[i] != "numeric") {
            TRUE
          } else { # only test dates
            .content$unit[i] %in% unlist(main_env$FORMATS$units)
          }
        })
        if (isFALSE(all(.test))) { # add error if any is false
          .row.ind <- which(!.test)
          .errors[[length(.errors) + 1]] <- HTML(
            sprintf(
              "Invalid values for <code>unit</code> at l. %s.",
              paste(.row.ind, collapse = ", ")
            )
          )
        }

        ### check not required values ----
        # dates
        .test <- .content$class != "date" &
          sapply(.content$dateTimeFormatString, isTruthy)
        if (any(.test)) {
          .errors[[length(.errors) + 1]] <- HTML(
            sprintf(
              "Unnecessary values for <code>dateTimeFormatString</code> at l.%s.",
              paste(which(.test), collapse = ", ")
            )
          )
        }
        # units
        .test <- .content$class != "numeric" &
          sapply(.content$unit, isTruthy)
        if (any(.test)) {
          .errors[[length(.errors) + 1]] <- HTML(
            sprintf(
              "Unnecessary values for <code>unit</code> at l. %s.",
              paste(which(.test), collapse = ", ")
            )
          )
        }

        ### check missing value pairs ----
        .test <- sapply(.content$missingValueCode, isContentTruthy) ==
          sapply(.content$missingValueCodeExplanation, isContentTruthy)
        if (isFALSE(all(.test))) {
          .errors[[length(.errors) + 1]] <- HTML(
            sprintf("Inconsistent missing value at l. %s", which(!.test))
          )
        }

        ### final check ----
        .valid <- length(.errors) == 0
        if (!.valid) {
          # send errors
          manual_edit_errors(tags$ul(lapply(.errors, tags$li)))
        } else {
          manual_edit_errors("")
        }
        # toggle close modal button
        shinyjs::toggleState("manual_edit-valid", condition = .valid)
      },
      ignoreInit = TRUE,
      priority = -1
    )

    output$errors <- renderUI({
      manual_edit_errors()
    })

    # Save ====
    observeEvent(input$validate, {
      req(isFALSE(isContentTruthy(manual_edit_errors())))

      # shortcut
      md <- main_env$local_rv$metadata.editor()
      # curate
      md[is.na(md)] <- ""
      # send ping = "update current row"
      if (!identical(main_env$local_rv$md.tables[[selected_file()]], md)) {
        main_env$EAL$ping <- "update current row"
      }
      # save
      main_env$local_rv$md.tables[[selected_file()]] <- md
      # leave
      removeModal()
    })
  })
}

manualEditCheck <- function(content, errors) {
  if (!all(sapply(content[[1]], isContentTruthy))) {
    .row.ind <- which(!sapply(content[[1]], isContentTruthy))
    errors[[length(errors) + 1]] <- HTML(
      sprintf(
        "Empty content for <code>%s</code> at l. %s",
        colnames(content), paste(.row.ind, collapse = ", ")
      )
    )
  }

  return(errors)
}
