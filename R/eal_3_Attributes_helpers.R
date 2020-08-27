# AttributeInputList ====
# This is not a proper module, just a wrapper for attributeInputs

#' @importFrom shinyBS bsCollapsePanel
#' @import shiny
attributeInputListUI <- function(id, row.index, main.env) {
  # prepare variables
  .row <- main.env$local.rv$md.tables[[main.env$local.rv$current$file]][row.index, ]
  
  # value
  shinyBS::bsCollapsePanel(
    title = .row[1],
    tagList(
      column(9,
        # Input ====
        lapply(seq_along(.row)[-1], function(col.index) {
          attributeInputUI(
            id = NS(id, names(.row)[col.index]),
            colname = names(.row)[col.index],
            value = .row[col.index],
            formats = main.env$FORMATS,
            rv = main.env$local.rv
          )
        }) # end of lapply colname
      ),
      column(3,
        # Preview ====
        h4("Preview"),
        tableOutput(
          NS(id, paste0("preview-", .row[1]))
        ),
        if(isTRUE(main.env$dev))
          actionButton(NS(id, "dev"), "dev"),
        # Annotations ====
        if(isTRUE(main.env$wip)) {
          tags$div(
            tags$hr(),
            actionButton(
              NS(id, "annotations"),
              label = NULL,
              icon = icon("project-diagram"),
              width = "100%"
            ),
            class = "wip"
          )
        }
        # , tags$hr(),
        # TODO Insert annotations here
      ) # end of column
    )
  ) # end of bsCollapsePanel
}

attributeInputList <- function(id, row.index, main.env) {
  moduleServer(id, function(input, output, session) {
    if(isTRUE(main.env$dev))
      observeEvent(input$dev, { browser() })
    
    # prepare variables
    .row <- main.env$local.rv$md.tables[[main.env$local.rv$current$file]][row.index, ]
    
    # Server ====
    lapply(seq_along(.row)[-1], function(col.index) {
      attributeInput(
        input = input,
        id = names(.row)[col.index],
        main.env = main.env,
        row.index = row.index # not duplicated with id due to module behavior
      )
    }) # end of lapply colname
    
    # Toggle dates & units ====
    observe({
      main.env$local.rv$current$update.view$depend()
      
      # Date
      shinyjs::toggle(
        "dateTimeFormatString",
        condition = input$class == "Date"
      )
      # Unit
      shinyjs::toggle(
        "unit",
        condition = input$class == "numeric"
      )
    })
    
    # Preview ====
    output[[paste0("preview-", .row[1])]] <- renderTable(
      main.env$local.rv$current$preview[,row.index]
    )
  })
}

# AttributeInput ====

#' @import shiny
attributeInputUI <- function(id, colname, value, formats, rv) {
  ui <- switch(colname,
    attributeDefinition = textAreaInput(
      id,
      value = value,
      withRedStar("Describe the attribute")
    ),
    class = selectInput(
      id,
      "Dectected class (change if misdetected)",
      choices = c("numeric", "character", "Date", "categorical"),
      selected = value
    ),
    unit = {
      tmp <- selectInput(
        id,
        withRedStar("Select an unit"),
        choices = setUnitList(main.env),
        selected = if (isTruthy(value) &&
                       !grepl("!Ad.*ere!", value) &&
                       value != "custom")
          value
      )
      if (isTruthy(value)) {
        tmp
      } else {
        shinyjs::hidden(tmp)
      }
    },
    dateTimeFormatString = {
      tmp <- selectInput( # TODO better hour format
        id,
        withRedStar("Select a date format"),
        unique(c(value, formats$dates)),
        selected = if (isTruthy(value) && !grepl("!Ad.*ere!", value)) value
      )
      if (isTruthy(value)) {
        tmp
      } else {
        shinyjs::hidden(tmp)
      }
    },
    missingValueCode = textInput(
      id,
      "Code for missing value (max 1 word)",
      value = value
    ),
    missingValueCodeExplanation = textAreaInput(
      id,
      "Explain Missing Values",
      value = value
    ),
    NULL
  ) # end of switch
  
  return(ui)
}

#' @import shiny
#' @importFrom shinyjs show hide
attributeInput <- function(input, id, colname, main.env, row.index) {
  # moduleServer(id, function(input, output, session) {
  observeEvent(input[[id]], {
    req(input[[id]])
    
    .value <- input[[id]]
    isolate({
      .current.table <- main.env$local.rv$md.tables[[main.env$local.rv$current$file]]
    })
    
    # Missing Value Code ====
    if (id == "missingValueCode") { # input: missing Value code
      if (grepl(".+ +.*", .value)) {
        .value <- strsplit(gsub("^ +", "", .value), split = " ")[[1]][1]
        
        updateTextInput(
          session,
          id,
          value = .value
        )
        showNotification(
          id = "mvc_update", # avoid multiple messages
          ui = tagList(
            tags$code("missingValueCode"),
            "fields are limited to a",
            tags$b("single word.")
          ),
          duration = 3,
          type = "warning"
        )
      }
    }
    
    # Units ====
    if (id == "unit") {
      req(input$class == "numeric")
      # Trigger CU edition if "custom" is selected
      if (.value == "custom" && 
          main.env$local.rv$custom.units$modal.state == "closed") {
        main.env$local.rv$custom.units$trigger$trigger()
      }
      
      # Check back for custom units
      lapply(main.env$local.rv$md.tables, function(table){
        if(isFALSE(all(
          main.env$local.rv$custom.units$table$id %in% table$unit
        ))) {
          .ind <- which(!main.env$local.rv$custom.units$table$id %in% table$unit)
          main.env$local.rv$custom.units$table$id <- main.env$local.rv$custom.units$table$id[-ind,]
        }
      })
    }
    
    # Set values ====
    # Correct values
    if (
      (id == "unit" && .current.table[row.index, "class"] != "numeric") ||
      (id == "dateTimeFormatString" && .current.table[row.index, "class"] != "Date")
    ) {
      # If selected class does not match unit or date, set input to blank
      .value <- ""
    }
    
    isolate({
      .current.table[row.index, id] <- .value
      main.env$local.rv$md.tables[[main.env$local.rv$current$file]] <- .current.table
    })
  },
  label = id
  )
}

#' @noRd
setUnitList <- function(main.env) {
  # Flat list
  choices <- c(
    {
      .tmp <- main.env$local.rv$custom.units$table$id
      if(isTruthy(.tmp))
        names(.tmp) <- paste0("custom/", .tmp)
      .tmp
    },
    main.env$FORMATS$units
  )
  
  # Input format
  types <- gsub("/.+", "", names(choices)) %>% unique()
  out <- list()
  sapply(types, function(type){
    out[[type]] <<- as.list(unname(choices[grepl(paste0("^", type), names(choices))]))
  })
  
  # Output
  return(out)
}

# Custom Units ====

customUnitsUI <- function(id, values = rep(NA, 5), cu.table = NULL) {
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

customUnits <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    #  * Cancel ----
    shinyjs::onclick("modal_cancel", {
      req(main.env$EAL$page == 3)
      req(main.env$local.rv$custom.units$modal.state == "open")
      
      # Close modal
      main.env$local.rv$custom.units$modal.state <- "closing"
      main.env$local.rv$custom.units$trigger$trigger()
      removeModal()
      
      # Set CU values
      isolate({
        main.env$local.rv$custom.units$values <- rep(NA, 5)
      })
    })
    
    # * Validate submit ----
    observe({
      req(main.env$EAL$page == 3)
      req(main.env$local.rv$custom.units$modal.state == "open")
      
      # type a new one
      shinyjs::toggleState(
        "modal_submit",
        condition = (
          !input$modal_id %in% main.env$local.rv$custom.units$table$id &&
            input$modal_id != "custom" &&
            isTruthy(input$modal_id) &&
            isTruthy(input$modal_unitType) &&
            isTruthy(input$modal_parentSI) &&
            isTruthy(input$modal_multiplier) &&
            isTruthy(input$modal_description)
        )
      )
    },
    label = "EAL3: Validate submit"
    )
    
    # * Submit ----
    shinyjs::onclick("modal_submit", { 
      req(main.env$EAL$page == 3)
      req(main.env$local.rv$custom.units$modal.state == "open")
      
      # Close modal
      removeModal()
      main.env$local.rv$custom.units$modal.state <- "closing"
      main.env$local.rv$custom.units$trigger$trigger()
      
      isolate({
        main.env$local.rv$custom.units$values <- c(
          input$modal_id,
          input$modal_unitType,
          input$modal_parentSI,
          input$modal_multiplier,
          input$modal_description
        )
      })
      
      # Update CU values ... 
      if (main.env$local.rv$custom.units$values[1] %in% 
          main.env$local.rv$custom.units$table$id) {
        main.env$local.rv$custom.units$table <- main.env$local.rv$custom.units$table %>%
          dplyr::filter(id = main.env$local.rv$custom.units$values[1]) %>%
          base::replace(values = main.env$local.rv$custom.units$values)
      } else { # ... or add CU values
        names(main.env$local.rv$custom.units$values) <- colnames(main.env$local.rv$custom.units$table)
        main.env$local.rv$custom.units$table[dim(main.env$local.rv$custom.units$table)[1] + 1, ] <- main.env$local.rv$custom.units$values
      }
      
      # Save CU values
      row <- main.env$local.rv$custom.units$unit.id[2]
      main.env$local.rv$md.tables[[main.env$local.rv$current$file]][row, "unit"] <- main.env$local.rv$custom.units$values["id"]
    })
    
  })
}
