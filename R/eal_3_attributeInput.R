#' @importFrom shiny NS tagList
attributeInputUI <- function(id, colname, value, formats, rv) {
  ns <- NS(id)

  ui <- switch(colname,
    attributeDefinition = textAreaInput(
      ns(colname),
      value = value,
      with_red_star("Describe the attribute")
    ),
    class = selectInput(
      ns(colname),
      "Dectected class (change if misdetected)",
      choices = c("numeric", "character", "Date", "categorical"),
      selected = value
    ),
    unit = {
      tmp <- selectInput(
        ns(colname),
        with_red_star("Select an unit"),
        rv$unitList,
        selected = if (isTruthy(value)) value
      )
      if (isTruthy(value)) {
        tmp
      } else {
        hidden(tmp)
      }
    },
    dateTimeFormatString = {
      tmp <- selectInput( # TODO better hour format
        ns(colname),
        with_red_star("Select a date format"),
        unique(c(value, formats$DATE)),
        selected = value
      )
      if (isTruthy(value)) {
        tmp
      } else {
        hidden(tmp)
      }
    },
    missingValueCode = textInput(
      ns(colname),
      "Code for missing value (max 1 word)",
      value = value
    ),
    missingValueCodeExplanation = textAreaInput(
      ns(colname),
      "Explain Missing Values",
      value = value
    ),
    NULL
  ) # end of switch

  return(ui)
}

#' @importFrom shiny observeEvent req isolate updateTextInput showNotification
#' HTML
#' @importFrom shinyjs show hide
attributeInput <- function(input, output, session,
                           rv, row_index, colname, obs, curt) {
  ns <- session$ns

  obs[[ns(colname)]] <- observeEvent(input[[colname]],
    {
      req(input[[colname]])

      .val <- input[[colname]]

      # Class ====
      if (colname == "class") {
        browser()
        # Date
        date_id <- "dateTimeFormatString"
        if (input[[colname]] == "Date") {
          shinyjs::show(date_id)
        } else {
          isolate(rv$current_table[row_index, "dateTimeFormatString"] <- "")
          shinyjs::hide(date_id)
        }

        # Unit
        unit_id <- "unit"
        if (input[[colname]] == "numeric") {
          shinyjs::show(unit_id)
        } else {
          isolate(rv$current_table[row_index, "unit"] <- "")
          shinyjs::hide(unit_id)
        }
      }
      # Missing Value Code ====
      if (colname == "missingValueCode") { # input: missing Value code
        if (grepl(".+ +.*", input[[colname]])) {
          .val <- strsplit(gsub("^ +", "", .val), split = " ")[[1]][1]

          updateTextInput(
            session,
            colname,
            value = .val
          )
          showNotification(
            id = session$ns("mvc_update"),
            ui = HTML("<code>missingValueCode</code> fields are limited to a
              <b>single word.</b>"),
            duration = 3,
            type = "warning"
          )
        }
      }
      # Units ====
      if (grepl("unit", ns(colname))) {
        # Trigger CU
        if (input[[colname]] == "custom" &&
          isFALSE(rv$modalOn)) {
          curt$trigger()
        }

        if (isFALSE(input[[colname]] %in% rv$unitList)) {
          .cu <- rv$current_table[row_index, colname]
          if (.cu %in% rv$CU_Table$id) {
            .ind <- which(rv$CU_Table$id == .cu)
            rv$CU_Table$id <- rv$CU_Table$id[-.ind]
          }
        }
        if(session$ns("") == "fill-EMLAL-Attributes-3-6-")
          browser()
      }

      rv$current_table[row_index, colname] <- .val
      rv$tables[[rv$current_file]] <- rv$current_table
    },
    label = ns(colname)
  )

  # Output ----
  return(obs)
}
