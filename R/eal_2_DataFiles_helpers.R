#' @import shiny
#'
#' @noRd
insertDataFileInput <- function(id, main_env) {
  # create the UI
  new_ui <- DataFileInputUI(id, main_env)
  # insert the UI
  insertUI(selector = "#inserthere_eal2", ui = new_ui, immediate = TRUE)
  # create the server
  DataFileInput(unns(id), main_env)
}

#' @import shiny
#' @importFrom shinyjs hidden
#'
#' @noRd
DataFileInputUI <- function(id, main_env) {
  # Setup
  ns <- NS(id)
  ref <- unns(id)
  .value <- main_env$local_rv$data_files[
    which(main_env$local_rv$data_files$id == ref),
  ]

  ui <- tags$div(
    id = ns("container"),
    shinydashboard::box(
      id = ns("box"),
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      # Header ----
      title = tags$div(
        id = ns("box-header-title"),
        class = "box-title-row",
        tags$span(
          class = "box-title-form",
          # Show name
          tags$div(
            tags$b(.value$name),
            style = "margin-top: 20px; padding: 6px; height: 40px; width: 100%;"
          )
        ),
        # Remove UI
        actionButton(ns("remove"), "", icon("trash"), class = "redButton")
      ),
      # Content ----
      tags$div(
        id = ns("content"),
        # class = "contentRow",
        tagList(
          fluidRow(
            column(
              6,
              textInput(
                ns("data.name"),
                "Data table name",
                value = .value$name
              )
            ),
            column(
              6,
              urlInput_UI(
                ns("data.url"),
                label = "Data remote location"
              )
            ),
          ),
          fluidRow(
            column(
              12,
              textAreaInput(
                ns("data_description"),
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
      ) # end of content
    ) # end of box
  )

  return(ui)
}

#' @import shiny
#' @importFrom shinyjs toggle
#' @importFrom shinyFeedback hideFeedback showFeedbackWarning
#'  showFeedbackSuccess
#'
#' @noRd
DataFileInput <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    # Setup ----
    row <- reactive({
      which(main_env$local_rv$data_files$id == id)
    })

    # [U] Collapse ====
    shinyjs::onclick("box-header-title", {
      shinyjs::runjs(
        sprintf(
          "$('#%s').closest('.box').find('[data-widget=collapse]').click();",
          session$ns("box")
        )
      )
    })

    # [U] Remove ====
    observeEvent(input$remove, {
        message(sprintf("Removing %s", session$ns("container")))
        # remove the UI
        removeUI(
          selector = sprintf("#%s", session$ns("container")),
          session = session
        )
        # erase file from 'data_objects' dir
        file.remove(main_env$local_rv$data_files[row(), "datapath"])
        # remove data from local variables
        main_env$local_rv$data_files <- main_env$local_rv$data_files[-row(), ]
      },
      label = sprintf("EAL2: remove %s", unns(id))
    )

    # [I] Inputs ====
    # Data name
    observeEvent(input$data.name, {
        isolate(
          main_env$local_rv$data_files[row(), "table.name"] <- input$data.name
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
          main_env$local_rv$data_files[row(), "url"] <- urlInput("data.url")
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
    observeEvent(input$data_description, {
        isolate(
          main_env$local_rv$data_files[row(), "description"] <-
            input$data_description
        )
        checkFeedback(
          input, "data_description",
          type = "warning", text = "Incomplete"
        )
      },
      ignoreInit = FALSE,
      label = sprintf("EAL2: input description %s", unns(id))
    )
  })
}
