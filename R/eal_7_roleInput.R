#' @import shiny
#'
#' @noRd
roleInputUI <- function(id, role_choices, val, width) {
  selectInput(
    inputId = NS(id, "role"),
    label = "Role",
    choices = role_choices,
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
roleInput <- function(id, main_env, row) {
  moduleServer(id, function(input, output, session) {

    # Get main role input ====
    observeEvent(input$role, {
      req(main_env$EAL$page == 7)
      .last <- tail(input$role, 1)

      # Get value
      if (length(.last) == 0) {
        main_env$local_rv$Personnel$role[row()] <- ""
      } else if (.last != "Other") {
        if (isTruthy(input$role)) {
          main_env$local_rv$Personnel$role[row()] <- paste(
            input$role,
            collapse = ","
          )
        } else {
          main_env$local_rv$Personnel$role[row()] <- ""
        }
      } else { # Ask for custom role
        showModal(
          modalDialog(
            title = "Define your own role",
            textInput(
              session$ns("role-other"),
              "Other role"
            ),
            footer = tags$span(
              actionButton(session$ns("modal-close"), "Confirm",
                           icon = icon("check")
              ),
              actionButton(session$ns("modal-cancel"), "Cancel",
                           icon = icon("ban")
              )
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
      req(main_env$EAL$page == 7)

      shinyjs::toggleState(
        "modal-close",
        condition = isTruthy(input$`role-other`) &&
          isFALSE(input$`role-other` %in% main_env$local_rv$role_choices$Other)
      )
    })

    # Get modal input ====
    observeEvent(input$`modal-cancel`, {
      req(main_env$EAL$page == 7)
      removeModal()

      updateSelectizeInput(
        session,
        "role",
        selected = input$role[which(input$role != "Other")]
      )
    })

    observeEvent(input$`modal-close`, {
      req(main_env$EAL$page == 7)
      removeModal()
      main_env$local_rv$last_modified <- row()

      main_env$local_rv$role_choices$Other <- unique(c(
        main_env$local_rv$role_choices$Other,
        input$`role-other`
      ))

      updateSelectizeInput(
        session,
        "role",
        choices = main_env$local_rv$role_choices,
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
      ifelse(main_env$EAL$page == 7,
             main_env$local_rv$trigger(),
             FALSE
      )
    }, {
      req(main_env$EAL$page == 7)
      req(main_env$local_rv$last_modified != row())

      updateSelectizeInput(
        session,
        "role",
        choices = main_env$local_rv$role_choices,
        selected = input$role
      )
    },
    ignoreInit = TRUE,
    label = session$ns("update-role")
    )
  })
}
