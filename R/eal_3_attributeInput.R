#' @import shiny
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
        rv$units.list,
        selected = if (isTruthy(value)) value
      )
      if (isTruthy(value)) {
        tmp
      } else {
        shinyjs::hidden(tmp)
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
        shinyjs::hidden(tmp)
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

#' @import shiny
#' @importFrom shinyjs show hide
attributeInput <- function(input, output, session,
  rv, row.index, colname, obs, curt) {
  ns <- session$ns
  
  obs[[ns(colname)]] <- observeEvent(input[[colname]],
    {
      req(input[[colname]])
      
      .val <- input[[colname]]
      
      # Class ====
      if (colname == "class") {
        # Date
        date.id <- "dateTimeFormatString"
        if (input[[colname]] == "Date") {
          isolate(rv$current.table[row.index, "unit"] <- input[[date.id]])
          shinyjs::show(date.id)
        } else {
          isolate(rv$current.table[row.index, "dateTimeFormatString"] <- "")
          shinyjs::hide(date.id)
        }
        
        # Unit
        unit.id <- "unit"
        if (input[[colname]] == "numeric") {
          isolate(rv$current.table[row.index, "unit"] <- input[[unit.id]])
          shinyjs::show(unit.id)
        } else {
          isolate(rv$current.table[row.index, "unit"] <- "")
          shinyjs::hide(unit.id)
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
            isFALSE(rv$modal.on)) {
          curt$trigger()
        }
        
        if (isFALSE(input[[colname]] %in% rv$units.list)) {
          .cu <- rv$current.table[row.index, colname]
          if (.cu %in% rv$cu.table$id) {
            .ind <- which(rv$cu.table$id == .cu)
            rv$cu.table$id <- rv$cu.table$id[-.ind]
          }
        }
      }
      
      # Set values ====
      if(
        (colname == "unit" && 
            rv$current.table[row.index, "class"] != "numeric") ||
          (colname == "dateTimeFormatString" && 
              rv$current.table[row.index, "class"] != "Date")
      )
        .val <- ""
      rv$current.table[row.index, colname] <- .val
      rv$tables[[rv$current.file]] <- rv$current.table
    },
    label = ns(colname)
  )
  
  # Output ----
  return(obs)
}