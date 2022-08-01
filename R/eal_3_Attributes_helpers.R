#' @noRd
buildAttributesTree <- function(local_rv) {
  
  .tables <- listReactiveValues(local_rv$md_tables)
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
  
}



# Manual editing ====
#' @import shiny
#' @noRd
metadataEditorUI <- function(id) {
  ns <- NS(id)

  modalDialog(
    title = "Manual edit",
    easyClose = TRUE,
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
      modalButton("Cancel"),
      actionButton(ns("validate"), "Save and close")
    )
  )
}

#' @import shiny
#' @noRd
metadataEditor <- function(id, main_env, selected_table, selected_file) {
  moduleServer(id, function(input, output, session) {
    ## setup variable ----
    manual_edit_errors <- reactiveVal("")

    ## handle input ----
    metadata_editor <- DataEditR::dataEditServer(
      "metadata_edit",
      data = selected_table,
      col_edit = FALSE,
      col_names = FALSE,
      col_factor = FALSE,
      col_options = list(
        class = c("character", "Date", "categorical", "numeric"),
        unit = isolate(unlist(main_env$FORMATS$units)),
        dateTimeFormatString = isolate(main_env$FORMATS$dates)
      ),
      row_edit = FALSE,
      quiet = TRUE
    )
    
    # curate input ----
    observeEvent(input$`metadata_edit-x`, {
      req(main_env$EAL$page == 3)
      
      .content <- metadata_editor()
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
      .test <- sapply(seq_row(.content), \(i) {
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
      .test <- sapply(seq_row(.content), \(i) {
        if (.content$class[i] != "numeric") {
          TRUE
        } else { # only test dates
          .content$unit[i] %in% c(
            unlist(main_env$FORMATS$units),
            main_env$local_rv$custom_units$table$id
          )
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
      .test <- .content$class != "Date" &
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
      shinyjs::toggleState("manual_edit-validate", condition = .valid)
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
      md <- metadata_editor()
      # curate
      md[is.na(md)] <- ""
      # send ping = "update current row"
      if (!identical(selected_table(), md)) {
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
