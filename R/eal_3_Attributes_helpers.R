# UI ====

#' @import shiny
#' @importFrom shinyBS bsCollapsePanel
#' @importFrom shinyFeedback useShinyFeedback

#' 
#' @noRd
.attributeInputUI <-  function(id, row.ind, table.name, main.env) {
  # Set variables
  row <- main.env$local.rv$md.tables[[table.name]][row.ind,]
  attribute <- row$attributeName
  
  # Render UI
  shinyBS::bsCollapsePanel(
    title = attribute,
    value = attribute,
    ... = fluidRow(
      # Create an input per metadata field
      column(9,
             lapply(
               names(row),
               .fieldInputUI,
               table.name = table.name, 
               row.ind = row.ind,
               main.env = main.env,
               id = NS(id, attribute)
             )
      ),
      # Preview the attribute
      column(3, tableOutput(NS(id, "preview"))
      )
    )
  )
}

#' @import shiny
#' @importFrom shinyjs hidden
#' @importFrom shinyFeedback useShinyFeedback
#'
#' @noRd
.fieldInputUI <- function(id, table.name, row.ind, md.name, main.env) {
  
  # Set variables
  value <- main.env$local.rv$md.tables[[table.name]][row.ind, md.name]
  formats <- main.env$FORMATS
  
  # Render UI
  tagList(
    shinyFeedback::useShinyFeedback(),
    switch(
      md.name,
      # attributeDefinition ----
      attributeDefinition = textAreaInput(
        NS(id, md.name),
        value = value,
        "Describe the attribute"
      ),
      # class ----
      class = selectInput(
        NS(id, md.name),
        "Dectected class (change if misdetected)",
        choices = c("numeric", "character", "Date", "categorical"),
        selected = value
      ),
      # unit ----
      unit = {
        tmp <- selectInput(
          NS(id, md.name),
          "Select an unit",
          choices = setUnitList(main.env),
          selected = if(isTruthy(value) && 
                        !grepl("!.*!", value) &&
                        value != "custom")
            value
        )
        if (isTruthy(value)) {
          tmp
        } else {
          shinyjs::hidden(tmp)
        }
      },
      # dateTimeFormatString ----
      dateTimeFormatString = {
        tmp <- selectInput( # TODO better hour format
          NS(id, md.name),
          "Select a date format",
          unique(c(value, formats$dates)),
          selected = if (isTruthy(value) && !grepl("!Ad.*ere!", value)) value
        )
        if (isTruthy(value)) {
          tmp
        } else {
          shinyjs::hidden(tmp)
        }
      },
      # missingValueCode ----
      missingValueCode = textInput(
        NS(id, md.name),
        "Code for missing value (max 1 word)",
        value = value
      ),
      # missingValueCodeExplanation ----
      missingValueCodeExplanation = textAreaInput(
        NS(id, md.name),
        "Explain Missing Values",
        value = value
      ),
      NULL
    )
  )
}

# Server =====

#' @import shiny
#'
#' @noRd
.attributeInput <- function(id, row.ind, main.env) {
  moduleServer(id, function(input, output, session) {
    
    # Set variables
    table.name <- id
    row <- main.env$local.rv$md.tables[[table.name]][row.ind,]
    attribute <- row$attributeName
    
    # Set server
    lapply(
      names(row),
      .fieldInput,
      table.name = table.name,
      row.ind = row.ind,
      main.env = main.env,
      id = attribute
    )
    
    # Render preview
    output$preview <- renderTable({
      .table <- main.env$local.rv$preview[[
        which(basename(names(main.env$local.rv$preview)) == table.name)
      ]][attribute]
    })
    
  })
}

#' @import shiny
#' @importFrom shinyjs toggle
#' @importFrom shinyFeedback hideFeedback showFeedbackSuccess showFeedbackWarning showFeedbackDanger
#'
#' @noRd
.fieldInput <- function(id, table.name, row.ind, md.name, main.env) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input[[md.name]], {
      req(input[[md.name]])
      .value <- input[[md.name]]
      table <- main.env$local.rv$md.tables[[table.name]]
      .row <- table[row.ind, "attributeName"]
      
      # Toggle inputs ====
      if(md.name == "class") {
        shinyjs::toggle("unit", condition = .value == "numeric")
        shinyjs::toggle("dateTimeFormatString", condition = .value == "Date")
      }
      
      # Missing Value Code ====
      if (md.name == "missingValueCode") {
        if (grepl(".+ +.*", .value)) {
          .value <- strsplit(gsub("^ +", "", .value), split = " ")[[1]][1]
          
          updateTextInput(
            session,
            md.name,
            value = .value
          )
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
      }
      
      # Units ====
      if (md.name == "unit") {
        req(input$class == "numeric" && .value != "NA")
        # Trigger CU edition if "custom" is selected
        if (.value == "custom" &&
            main.env$local.rv$custom.units$modal.state == "closed")
          main.env$local.rv$custom.units$modal.state <- "open"
        
        # Check back for custom units
        # - get all units columns from attributes table
        .attribute.units <- lapply(main.env$local.rv$md.tables, function(.table){
          .table$unit
        }) %>% unlist %>% unique
        # - check if custom units are not found among them
        .notfoundcu <- isFALSE(main.env$local.rv$custom.units$table$id %in% .attribute.units)
        if(any(.notfoundcu)) {
          # - remove not found custom units
          .tmp <- main.env$local.rv$custom.units$table$id[-which(.notfoundcu),]
          main.env$local.rv$custom.units$table$id <- .tmp
        }
      }
      
      # Completeness ====
      main.env$local.rv$completed[[table.name]][[.row]][[md.name]] <- switch(
        md.name,
        # attributeDefinition
        attributeDefinition = isTruthy(input[[md.name]]),
        # class
        class = isTruthy(input[[md.name]]),
        # dateTimeFormatString
        dateTimeFormatString = if(main.env$local.rv$md.tables[[table.name]][row.ind, "class"] == "Date") 
          isTruthy(input[[md.name]]) && isFALSE(grepl("^!.*!$", input[[md.name]]))
        else
          TRUE,
        # unit
        unit = {
          if(main.env$local.rv$md.tables[[table.name]][row.ind, "class"] == "numeric") 
            isTruthy(input[[md.name]]) &&
            input[[md.name]] != "custom" && input[[md.name]] != "NA" &&
            isFALSE(grepl("^!.*!$", input[[md.name]]))
          else
            TRUE
        },
        TRUE
      )
      
      # # Feedback ====
      # shinyFeedback::hideFeedback(md.name)
      # 
      # if(isTRUE(main.env$local.rv$completed[[table.name]][[.row]][[md.name]]))
      #   shinyFeedback::showFeedbackSuccess(md.name)
      # else {
      #   if(md.name == "unit" && .value == "custom")
      #     shinyFeedback::showFeedbackWarning(
      #       md.name,
      #       text = "describe the custom unit"
      #     )
      #   else if(md.name %in% c("attributeDefinition", "dateTimeFormatString", "unit"))
      #     shinyFeedback::showFeedbackDanger(
      #       md.name,
      #       text = "invalid value provided"
      #     )
      #   else if(md.name == "missingValueCode")
      #     shinyFeedback::showFeedbackWarning(
      #       md.name,
      #       text = "blank code means 'no missing value'"
      #     )
      # }
      
      # Set values ====
      # Correct values
      if (
        (md.name == "unit" && table[row.ind, "class"] != "numeric") ||
        (md.name == "dateTimeFormatString" && table[row.ind, "class"] != "Date") ||
        (is.na(.value) || .value == "NA")
      ) {
        # If selected class does not match unit or date, force set input to blank
        .value <- ""
      }
      
      isolate({
        table[row.ind, md.name] <- .value
        main.env$local.rv$md.tables[[table.name]] <- table
      })
      
    }, label = session$ns(md.name) ) # end of observeEvent
  })
}

#' @noRd
setUnitList <- function(main.env) {
  # Flat list
  choices <- c(
    "---/NA" = NA,
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
#' @import shiny
#'
#' @noRd
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

#' @import shiny
#' @importFrom shinyjs onclick toggleState 
#' @importFrom dplyr filter
#'
#' @noRd
customUnits <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    #  * Cancel ----
    shinyjs::onclick("modal_cancel", {
      req(main.env$EAL$page == 3)
      req(main.env$local.rv$custom.units$modal.state == "open")
      
      # Close modal
      main.env$local.rv$custom.units$modal.state <- "closing"
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
