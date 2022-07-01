#' @import shiny
#' @importFrom shinyTree shinyTree
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs hidden
#'
#' @noRd
CatVarsUI <- function(id) {
  ns <- NS(id)

  return(
    fluidPage(
      selectInput(
        ns("files"),
        "Select file",
        choices = c()
      ),
      textOutput(ns("invalid_description")),
      DataEditR::dataEditUI(ns("data_edit"))
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom shinyjs toggleState
#' @importFrom shinyFeedback hideFeedback showFeedbackSuccess showFeedbackDanger
#' @importFrom dplyr filter slice select
#'
#' @noRd
CatVars <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    if (main_env$dev) .browse_dev(main_env, 4)

    # Set variables ====
    ## Set files selection ----
    observeEvent(main_env$EAL$page, {
        req(main_env$EAL$page == 4)

        updateSelectInput(
          session,
          "files",
          choices = names(main_env$local_rv$cv_tables)
        )
      },
      priority = -1
    )

    ## Get file ----
    .content <- reactive({
      req(main_env$EAL$page == 4)
      req(isTruthy(input$files))

      main_env$local_rv$cv_tables[[input$files]]
    })

    # Manage data edit table ====
    catvar_edited <- DataEditR::dataEditServer(
      "data_edit",
      .content,
      col_edit = FALSE,
      col_names = c("attributeName", "code", "definition"),
      col_readonly = c("attributeName", "code"),
      row_edit = FALSE,
      quiet = TRUE
    )

    # Get input ====
    invalid_description <- reactiveVal("")
    observeEvent(input$`data_edit-x`, {
      .condition <- sapply(catvar_edited()$definition, isTruthy)

      if (all(.condition)) {
        # save -- isolate to not trigger .content()
        isolate(main_env$local_rv$cv_tables[[input$files]] <- catvar_edited())
        invalid_description("")
      } else {
        invalid_description(sprintf(
          "Invalid description at l. %s.",
          paste(which(!.condition), collapse = ", ")
        ))
      }

      ## Set completeness ----
      main_env$EAL$completed <- all(.condition)

      ## Send feedback ----
      checkFeedback(input, "files", all(.condition), type = "danger")
    })

    ## Show invalid description ----
    output$invalid_description <- renderText({
      invalid_description()
    })
  })
}
