#' @noRd
buildAttributesTree <- function(main.env) {
  if(main.env$EAL$page == 3){
    .tables <- isolate(main.env$local.rv$md.tables)
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
          function(attributeName, file.name){
            # Render leaf
            structure(
              attributeName,
              # sttype="default",
              sticon="fa fa-table"
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
replaceValue <- function(table, attribute.name, field, new.value) {
  
  x <- try({
    row <- which(table$attributeName == attribute.name)
    col <- field
    table[row, col] <- new.value
  })
  
  return(table)
}

#' @noRd
setUnitList <- function(main.env, set = NULL) {
  # Flat list
  choices <- c(
    {
      .tmp <- main.env$local.rv$custom.units$table$id
      if(isTruthy(.tmp))
        names(.tmp) <- "custom"
      .tmp
    },
    main.env$FORMATS$units
  )
  
  # Input format
  types <- unique(names(choices[sapply(choices, length) != 1]))
  out <- choices[sapply(choices, length) == 1]
  out <- c(custom = out$custom, out[names(out) != "custom"])
  # browser()
  
  sapply(types, function(type){
    out[[type]] <<- unname(choices[which(names(choices) == type)])[[1]]
  })
  # Correct value set for updates
  if(!is.null(set)) {
    set <- set |>
      setNames(nm = names(out)[
        which(sapply(
          out,
          function(.ul)
            set %in% .ul
        ))
      ]) |>
      as.list()
  }
  
  # TODO rework unit listing
  
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
customUnitsUI <- function(id, values = rep(NA, 5), main.env) {
  modalDialog(
    title = "Custom Unit",
    tagList(
      # id
      fluidRow(
        column(
          6,
          offset = 3,
          textInput(
            NS(id, "modal_id"),
            label = "Unit identifier",
            placeholder = "e.g. milligramsPerGram",
            value = optional(values[1])
          ),
          # unitType
          textInput(
            NS(id, "modal_unitType"),
            label = "Physical property types the unit belongs to",
            placeholder = "e.g. mass",
            value = optional(values[2])
          ),
          # ParentSI
          selectInput(
            NS(id, "modal_parentSI"),
            label = "Parent unit in SI",
            choices = {
              main.env$FORMATS$units
              # browser()
              # split(
              #   unname(main.env$FORMATS$units[-1]),
              #   as.factor(names(main.env$FORMATS$units[-1]))
              # )
            },
            selected = optional(values[3])
          ),
          # MultiplierToSI
          numericInput(
            NS(id, "modal_multiplier"),
            label = "Numeric multiplier computed from Parent unit in SI",
            value = optional(values[4], default = 1),
          ),
          # Description
          textAreaInput(
            NS(id, "modal_description"),
            label = "Unit description",
            placeholder = "e.g. milligrams per gram",
            value = optional(values[5])
          )
        )
      ) # end of fluidRow
    ),
    easyClose = FALSE,
    footer = tagList(
      actionButton(NS(id, "modal_cancel"), "Cancel"),
      shinyjs::disabled(actionButton(NS(id, "modal_submit"), "Submit"))
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
    observeEvent(input$modal_cancel, {
      req(main.env$EAL$page == 3)
      .newCancel <- main.env$local.rv$custom.units$cancel() + 1
      main.env$local.rv$custom.units$cancel(.newCancel)
      removeModal()
    },
    label = "EAL3: CU cancel")
    
    # * Validate submit ----
    observe({
      req(main.env$EAL$page == 3)
      
      # Validate each field
      checkFeedback(
        input,
        "modal_id",
        condition = !input$modal_id %in% main.env$local.rv$custom.units$table$id &&
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
    label = "EAL3: CU check submit"
    )
    
    # * Submit ----
    observeEvent(input$modal_submit, {
      req(main.env$EAL$page == 3)
      
      # Close modal
      removeModal()
      
      .values <- c(
        input$modal_id,
        input$modal_unitType,
        input$modal_parentSI,
        input$modal_multiplier,
        input$modal_description
      ) |>
        setNames(nm = colnames(main.env$local.rv$custom.units$table))
      
      # CU table is exected to have been made reactive at savevariable_functions.R#392
      # Update CU values ...
      if (.values[1] %in% main.env$local.rv$custom.units$table$id) {
        main.env$local.rv$custom.units$table <- main.env$local.rv$custom.units$table |>
          dplyr::filter(id = .values[1]) |>
          base::replace(values = .values)
      } else { # ... or add CU values
        main.env$local.rv$custom.units$table[
          nrow(main.env$local.rv$custom.units$table) + 1, ] <- .values
      }
    },
    label = "EAL3 CU do submit")
    
  })
}