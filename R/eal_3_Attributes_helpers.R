#' @noRd
buildAttributesTree <- function(main_env) {
  if (main_env$EAL$page == 3) {
    .tables <- isolate(main_env$local_rv$md_tables)
    req(isContentTruthy(.tables))

    devmsg("compute tree", tag = "attributes")

    lapply(
      names(.tables),
      # Files node
      function(file_name) {
        structure(lapply(
          .tables[[file_name]]$attributeName,
          # Attributes leaves
          file_name = file_name,
          function(attribute_name, file_name) {
            # Render leaf
            structure(
              attribute_name,
              # sttype="default",
              sticon = "fa fa-table"
            )
          }
        ) |>
          setNames(nm = .tables[[file_name]]$attributeName),
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
replaceValue <- function(table, attribute_name, field, new_value) {
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
    unit_list = out,
    set_unit = set
  ))
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
        .content <- main_env$local_rv$metadata_editor()
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
      md <- main_env$local_rv$metadata_editor()
      # curate
      md[is.na(md)] <- ""
      # send ping = "update current row"
      if (!identical(main_env$local_rv$md_tables[[selected_file()]], md)) {
        main_env$EAL$ping <- "update current row"
      }
      # save
      main_env$local_rv$md_tables[[selected_file()]] <- md
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
