unitsUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    id = ns("unit_div"),
    column(
      6,
      selectInput(
        ns("unit"),
        label = tagList(
          tags$b("Select an unit"),
          helpText("You can search a unit by typing its name.")
        ),
        choices = c(NA_character_)
      )
    ),
    column(
      2,
      actionButton(ns("add_custom_units"), "", icon("plus")),
      style = "bottom: -80px"
    )
  )
}

units <- function(id, main_env, selected_file, selected_attribute, selected_class) {
  moduleServer(id, function(input, output, session) {
    # Toggle and update ----
    # depends on class
    observeEvent(selected_class(), {
      req(main_env$EAL$page == 3)

      shinyjs::toggle(
        "unit_div",
        condition = selected_class() == "numeric"
      )
      if(selected_class() != "numeric") {
        updateSelectInput(
          session,
          "unit",
          selected = ""
        )
      } else {
        .unit <- main_env$local_rv$md_tables[[selected_file()]] |>
          filter(attributeName == selected_attribute()) |>
          select(unit)
        
        .tmp <- setUnitList(
          main_env, 
          set = optional(.unit,
                         default = main_env$FORMATS$units$dimensionless[1])
        )
        updateSelectInput(
          session,
          "unit",
          choices = .tmp$unit_list,
          selected = .tmp$set_unit
        )
      }
    })
    
    # Handle input ----
    observeEvent(input$unit, {
      # validity checks
      validate(
        need(isTruthy(selected_file()), "No file selected"),
        need(isTruthy(selected_attribute()), "No attribute selected"),
        need(selected_class() == "numeric", "Not a number"),
        need(!is.na(input$unit), "Unset_unit input.")
      )
      
      # Correct input value
      .value <- input$unit

      if(isFALSE(.value %in% c(
        unlist(main_env$FORMATS$units),
        main_env$local_rv$custom_units$table$id
      ))) {
        .value <- main_env$FORMATS$units$dimensionless[1]
        
        updateSelectInput(
          session,
          "unit",
          selected = .value
        )
        showNotification(
          id = "unit_not_found",
          "Queried unit not found"
        )
      }
      
      # Save value
      .row <- which(main_env$local_rv$md_tables[[selected_file()]]$attributeName == selected_attribute())
      main_env$local_rv$md_tables[[selected_file()]][.row, "unit"] <<- .value
      
      # Check validity
      .condition <- if(selected_class() == "numeric") {
        isTruthy(.value) && 
          .value %in% c(
            unlist(main_env$FORMATS$units),
            main_env$local_rv$custom_units$table$id
          )
      } else 
        TRUE # not a number: do not block progression to Attributes step
      checkFeedback(
        input, 
        "unit",
        condition = .condition,
        type = "danger"
      )
    })
    
    # Custom Units ----
    observeEvent(input$add_custom_units, {
      req(main_env$EAL$page == 3)
      
      # Properly show modal
      showModal(
        customUnitsUI(
          ns = session$ns,
          main_env = main_env
        )
      )
    })
    
    ## Cancel ----
    # Just remove popup
    observeEvent(input$modal_cancel, {
      req(main_env$EAL$page == 3)
      removeModal()
    },
    label = "EAL3: CU cancel")
    
    ## Validate submit ----
    observe({
      req(main_env$EAL$page == 3)
      
      # Validate each field
      checkFeedback(
        input,
        "modal_id",
        condition = !input$modal_id %in% main_env$local_rv$custom_units$table$id &&
          input$modal_id != "custom" &&
          isTruthy(input$modal_id),
        type = "danger"
      )
      checkFeedback(input, "modal_unitType", type = "danger")
      checkFeedback(input, "modal_parentSI", type = "danger")
      checkFeedback(input, "modal_multiplier", type = "danger")
      checkFeedback(input, "modal_description", type = "danger")
      
      # Toggle submit button
      shinyjs::toggleState(
        "modal_submit",
        condition = (
          !input$modal_id %in% main_env$local_rv$custom_units$table$id &&
            input$modal_id != "custom" &&
            isTruthy(input$modal_id) &&
            isTruthy(input$modal_unitType) &&
            isTruthy(input$modal_parentSI) &&
            isTruthy(input$modal_multiplier) &&
            isTruthy(input$modal_description)
        )
      )
    },
    label = "EAL3: CU check submit"
    )
    
    ## Submit ----
    observeEvent(input$modal_submit, {
      req(main_env$EAL$page == 3)
      
      # Close modal
      removeModal()
      
      .values <- c(
        input$modal_id,
        input$modal_unitType,
        input$modal_parentSI,
        input$modal_multiplier,
        input$modal_description
      ) |>
        setNames(nm = colnames(main_env$local_rv$custom_units$table))

      ### Save CU values ----
      # CU table is expected to have been made reactive at savevariable_functions.R#392
      # Update CU values ...
      if (.values[1] %in% main_env$local_rv$custom_units$table$id) {
        browser()
        main_env$local_rv$custom_units$table <- main_env$local_rv$custom_units$table |>
          dplyr::filter(id = .values[1]) |>
          base::replace(values = .values)
      } else { # ... or add CU values
        main_env$local_rv$custom_units$table[
          nrow(main_env$local_rv$custom_units$table) + 1, ] <- .values
      }

      ## Update unit input ----
      .tmp <- setUnitList(main_env, set = .values[1])
      updateSelectInput(
        session,
        "unit",
        choices = .tmp$unit_list,
        selected = .tmp$set_unit
      )
    },
    priority = 1,
    label = "EAL3 CU: do submit")
  })
}


#' @import shiny
#'
#' @noRd
customUnitsUI <- function(ns, main_env) {
  modalDialog(
    title = "Custom Unit",
    tagList(
      # disclaimer: incompatible with Manual edit
      tags$b("Warning: custom units might not be supported by the manual edit feature.
              Using it can lead to the unit overwritten by a default value (dimensionless)."),
      # id
      fluidRow(
        column(
          6,
          offset = 3,
          textInput(
            ns("modal_id"),
            label = "Unit identifier",
            placeholder = "e.g. milligramsPerGram"
          ),
          # unitType
          textInput(
            ns("modal_unitType"),
            label = "Physical property types the unit belongs to",
            placeholder = "e.g. mass"
          ),
          # ParentSI
          selectInput(
            ns("modal_parentSI"),
            label = "Parent unit in SI",
            choices = main_env$FORMATS$units
          ),
          # MultiplierToSI
          numericInput(
            ns("modal_multiplier"),
            label = "Numeric multiplier computed from Parent unit in SI",
            value = 1
          ),
          # Description
          textAreaInput(
            ns("modal_description"),
            label = "Unit description",
            placeholder = "e.g. milligrams per gram"
          )
        )
      ) # end of fluidRow
    ),
    easyClose = FALSE,
    footer = tagList(
      actionButton(ns("modal_cancel"), "Cancel"),
      shinyjs::disabled(actionButton(ns("modal_submit"), "Submit"))
    )
  )
}

#' @noRd
setUnitList <- function(main_env, set = NULL) {
  # Flat list
  choices <- sapply(main_env$FORMATS$units, as.list)
  .tmp <- main_env$local_rv$custom_units$table$id
  if(isContentTruthy(.tmp))
    choices[["custom"]] <- sapply(.tmp, as.list)
  
  # Correct value set for updates
  if(!is.null(set)) {
    set <- set |>
      setNames(nm = names(choices)[
        which(sapply(
          choices,
          function(.ul)
            set %in% .ul
        ))
      ]) |>
      unname() # |>
      # as.list()
  }
  
  # Output
  return(list(
    unit_list = choices,
    set_unit = set
  ))
}