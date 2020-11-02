#' @import shiny
#'
#' @noRd
roleInputUI <- function(id, role.choices, val, width) {
  selectInput(
    inputId = NS(id, "role"),
    label = "Role",
    choices = role.choices, 
    selected = unlist(strsplit(val, ",")),
    multiple = TRUE,
    selectize = TRUE,
    width = width
  )
}

#' @import shiny
#' @importFrom shinyjs toggleState
#'
#' @noRd
roleInput <- function(id, main.env, row) {
  moduleServer(id, function(input, output, session) {
    
    # Get main role input ====
    observeEvent(input$role, {
      req(main.env$EAL$page == 7)
      .last <- tail(input$role, 1)
      
      # Get value
      if(length(.last) == 0) {
        main.env$local.rv$Personnel$role[row()] <- ""
      } else if(.last != "Other") {
        if(isTruthy(input$role)) {
          main.env$local.rv$Personnel$role[row()] <- paste(
            input$role,
            collapse = ","
          )
        } else
          main.env$local.rv$Personnel$role[row()] <- ""
      } else { # Ask for custom role
        showModal(
          modalDialog(
            title = "Define your own role",
            textInput(
              session$ns("role-other"),
              "Other role"
            ),
            footer = tags$span(
              actionButton(session$ns("modal-close"), "Confirm", icon = icon("check")),
              actionButton(session$ns("modal-cancel"), "Cancel", icon = icon("ban"))
            )
          )
        )
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE,
    label = session$ns("role") 
    )
    
    # Validate modal button ====
    observe({
      req(main.env$EAL$page == 7)
      
      shinyjs::toggleState(
        "modal-close",
        condition = isTruthy(input$`role-other`) && 
          isFALSE(input$`role-other` %in% main.env$local.rv$role.choices$Other)
      )
    })
    
    # Get modal input ====
    observeEvent(input$`modal-cancel`, {
      req(main.env$EAL$page == 7)
      removeModal()
      
      updateSelectizeInput(
        session, 
        "role",
        selected = input$role[which(input$role != "Other")]
      )
    })
    
    observeEvent(input$`modal-close`, {
      req(main.env$EAL$page == 7)
      removeModal()
      main.env$local.rv$last.modified <- row()
      
      main.env$local.rv$role.choices$Other <- unique(c(
        main.env$local.rv$role.choices$Other,
        input$`role-other`
      ))
      
      updateSelectizeInput(
        session, 
        "role",
        choices = main.env$local.rv$role.choices,
        selected = replace(
          input$role,
          which(input$role == "Other"), 
          input$`role-other`
        )
      )
    },
    label = session$ns("modal-close")
    )
    
    # Update role choices ====
    observeEvent({
      ifelse(main.env$EAL$page == 7,
        main.env$local.rv$trigger(),
        FALSE
      )
    }, {
      req(main.env$EAL$page == 7)
      req(main.env$local.rv$last.modified != row())
      
      updateSelectizeInput(
        session,
        "role",
        choices = main.env$local.rv$role.choices,
        selected = input$role
      )
    },
    ignoreInit = TRUE,
    label = session$ns("update-role")
    )
  })
}