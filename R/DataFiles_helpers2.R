#' @import shiny
#'
#' @noRd
# insertDataFileInput <- function(id, main_env) {
#   # Add row
#   .id <- unns(id)
#   # create the UI -- no NS !!!
#   new_ui <- DataFileInputUI(id, main_env)
#   # insert the UI
#   insertUI(selector = "#inserthere_eal2", ui = new_ui, immediate = TRUE)
#   # create the server
#   DataFileInput(.id, main_env)
# }

#' @import shiny
#' @importFrom shinyjs hidden
#'
#' @noRd
# insertModule(
#   session$ns("2"),
#   "#inserthere_eal2",
#   moduleUI = DataFileInputUI2,
#   moduleUI.args = list(main_env = main_env),
#   module = DataFileInput2,
#   module.args = list(main_env = main_env)
# )
DataFileInputUI2 <- function(id, main_env) {
  # Setup
  ref <- unns(id)
  .value <- main_env$local_rv$data.files[
    which(main_env$local_rv$data.files$id == ref),
  ]

  # ui <- tagList(
  ui <- tags$div(
    style = "display: inline-block;width: calc(100% - 50px);",
    # id = NS(id, "container"),
    # class = "inputBox",
    tags$div(
      class = "topInputRow",
      # Collapse
      actionLink(NS(id, "collapse"), "", icon("chevron-right")),
      tags$span(
        style = "width: calc(100% - 50px); margin: 0 5px 0;",
        # Show name
        tags$div(
          textOutput(NS(id, "name")),
          style = "margin-top: 20px; padding: 6px; height: 40px;"
        )
      )
      # Remove UI
      # , actionButton(NS(id, "remove"), "", icon("trash"), class = "redButton")
    ), # end of header
    shinyjs::hidden(
      tags$div(
        id = NS(id, "content"),
        # class = "contentRow",
        tagList(
          fluidRow(
            column(
              6,
              textInput(
                NS(id, "data.name"),
                "Data table name",
                value = .value$name
              )
            ),
            column(
              6,
              urlInput_UI(
                NS(id, "data.url"),
                label = "Data remote location"
              )
            ),
          ),
          fluidRow(
            column(
              12,
              textAreaInput(
                NS(id, "data.description"),
                "Data Table Description",
                value = if (isTruthy(.value$description)) {
                  .value$description
                } else {
                  sprintf("Content of %s", .value$name)
                },
                width = "100%"
              )
            )
          )
        )
      )
    ) # end of hidden content
  )

  return(ui)
}

#' @import shiny
#' @importFrom shinyjs toggle
#' @importFrom shinyFeedback hideFeedback showFeedbackWarning showFeedbackSuccess
#'
#' @noRd
DataFileInput2 <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    # Setup ----
    row <- reactive({
      which(main_env$local_rv$data.files$id == id)
    })

    # [U] Collapse ====
    observeEvent(input$collapse, {
        shinyjs::toggle(
          id = "content",
          anim = TRUE,
          animType = "slide",
          time = 0.25,
          condition = input$collapse %% 2 == 1
        )

        updateActionButton(
          session,
          "collapse",
          icon = icon(
            ifelse(input$collapse %% 2 == 0, "chevron-right", "chevron-down")
          )
        )
      },
      label = sprintf("EAL2 %s", unns(id))
    )

    # [U] Verbose name ====
    output$name <- renderText({
      .name <- main_env$local_rv$data.files$name[row()]
      browser()
      validate(
        need(.name != "", "No provided name")
      )

      return(.name)
    })

    # [U] Remove ====
    observeEvent(input$remove, {
        message(sprintf("Removing %s", session$ns("container")))
        # remove the UI
        # removeUI(
        #   selector = sprintf("#%s", session$ns("container")),
        #   # immediate = TRUE,
        #   session = session
        # )
        # erase file from 'data_objects' dir
        file.remove(main_env$local_rv$data.files[row(), "datapath"])
        # remove data from local variables
        main_env$local_rv$data.files <- main_env$local_rv$data.files[-row(), ]
      },
      label = sprintf("EAL2: remove %s", unns(id))
    )

    # [I] Inputs ====
    # Data name
    observeEvent(input$data.name, {
        isolate(
          main_env$local_rv$data.files[row(), "table.name"] <- input$data.name
        )
        shinyFeedback::hideFeedback("data.name")
        if (isFALSE(isContentTruthy(input$data.name))) {
          shinyFeedback::showFeedbackWarning("data.name", "Incomplete")
        } else {
          shinyFeedback::showFeedbackSuccess("data.name")
        }
      },
      ignoreInit = FALSE,
      label = sprintf("EAL2: input name %s", unns(id))
    )

    # Data URL
    observeEvent(input$data.url, {
        isolate(
          main_env$local_rv$data.files[row(), "url"] <- urlInput("data.url")
        )
        shinyFeedback::hideFeedback("data.url")
        if (isFALSE(isContentTruthy(input$data.url))) {
          shinyFeedback::showFeedbackWarning("data.url", "Incomplete")
        } else {
          shinyFeedback::showFeedbackSuccess("data.url")
        }
      },
      ignoreInit = FALSE,
      label = sprintf("EAL2: input url %s", unns(id))
    )

    # Description
    observeEvent(input$data.description, {
        isolate(
          main_env$local_rv$data.files[row(), "description"] <- input$data.description
        )
        shinyFeedback::hideFeedback("data.description")
        if (isFALSE(isContentTruthy(input$data.description))) {
          shinyFeedback::showFeedbackWarning("data.description", "Incomplete")
        } else {
          shinyFeedback::showFeedbackSuccess("data.description")
        }
      },
      ignoreInit = FALSE,
      label = sprintf("EAL2: input description %s", unns(id))
    )
  })
}
