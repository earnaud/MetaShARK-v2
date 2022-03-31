#' @import shiny
#' 
#' @noRd
insertDataFileInput <- function(id, main.env){
  # create the UI
  new.ui <- DataFileInputUI(id, main.env)
  # insert the UI
  insertUI(selector = "#inserthere_eal2", ui = new.ui, immediate = TRUE)
  # create the server
  DataFileInput(unns(id), main.env)
}

#' @import shiny
#' @importFrom shinyjs hidden
#' 
#' @noRd
DataFileInputUI <- function(id, main.env) {
  # Setup
  ns <- NS(id)
  ref <- unns(id)
  .value <- main.env$local.rv$data.files[
    which(main.env$local.rv$data.files$id == ref),
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
            style="margin-top: 20px; padding: 6px; height: 40px; width: 100%;"
          )
        ),
        # Remove UI
        actionButton(ns("remove"), "", icon("trash"), class = "redButton")
      ),
      # title = tags$span(
      #   class = "box-title-form",
      #   # style="width: calc(100% - 100px); margin: 0 5px 0;",
      #   # Show name
      #   tags$div(
      #     tags$b(.value$name),
      #     # style="margin-top: 20px; padding: 6px; height: 40px;"
      #   ),
      #   # Remove UI
      #   actionButton(ns("remove"), "", icon("trash"), class = "redButton")
      # ), # end of header
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
              URL_Input_UI(
                ns("data.url"),
                label = "Data remote location"
              )
            ),
          ),
          fluidRow(
            column(
              12,
              textAreaInput(
                ns("data.description"),
                "Data Table Description",
                value = if(isTruthy(.value$description)) 
                  .value$description else
                    sprintf("Content of %s", .value$name),
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
#' @importFrom shinyFeedback hideFeedback showFeedbackWarning showFeedbackSuccess
#' 
#' @noRd
DataFileInput <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Setup ----
    row <- reactive({which(main.env$local.rv$data.files$id == id)})
    
    # [U] Collapse ====
    shinyjs::onclick("box-header-title", {
      shinyjs::runjs(
        sprintf("$('#%s').closest('.box').find('[data-widget=collapse]').click();",
                session$ns("box"))
      )
    })
    # observeEvent(input$collapse, {
    #   shinyjs::toggle(
    #     id = "content",
    #     anim = TRUE,
    #     animType = "slide",
    #     time = 0.25,
    #     condition = input$collapse %% 2 == 1
    #   )
    #   
    #   updateActionButton(
    #     session, 
    #     "collapse", 
    #     icon = icon(
    #       ifelse(input$collapse %% 2 == 0, "chevron-right", "chevron-down")
    #     )
    #   )
    # },
    # label = sprintf("EAL2 %s", unns(id))
    # )
    
    # [U] Verbose name ====
    # output$name <- renderText({
    #   
    #   .name <- main.env$local.rv$data.files$name[row()]
    #   
    #   validate(
    #     need(.name != "", "No provided name")
    #   )
    #   
    #   return(.name)
    # })
    
    # [U] Remove ====
    observeEvent(input$remove, {
      message(sprintf("Removing %s", session$ns("container")))
      # remove the UI
      removeUI(
        selector = sprintf("#%s", session$ns("container")),
        # immediate = TRUE,
        session = session
      )
      # erase file from 'data_objects' dir
      file.remove(main.env$local.rv$data.files[row(),"datapath"])
      # remove data from local variables
      main.env$local.rv$data.files <- main.env$local.rv$data.files[-row(),]
    },
    label = sprintf("EAL2: remove %s", unns(id))
    )
    
    # [I] Inputs ====
    # Data name
    observeEvent(input$data.name, {
      isolate(
        main.env$local.rv$data.files[row(), "table.name"] <- input$data.name
      )
      shinyFeedback::hideFeedback("data.name")
      if(isFALSE(isContentTruthy(input$data.name)))
        shinyFeedback::showFeedbackWarning("data.name", "Incomplete")
      else
        shinyFeedback::showFeedbackSuccess("data.name")
    },
    ignoreInit = FALSE,
    label = sprintf("EAL2: input name %s", unns(id))
    )
    
    # Data URL
    observeEvent(input$data.url, {
      isolate(
        main.env$local.rv$data.files[row(), "url"] <- URL_Input("data.url")
      )
      shinyFeedback::hideFeedback("data.url")
      if(isFALSE(isContentTruthy(input$data.url)))
        shinyFeedback::showFeedbackWarning("data.url", "Incomplete")
      else
        shinyFeedback::showFeedbackSuccess("data.url")
    },
    ignoreInit = FALSE,
    label = sprintf("EAL2: input url %s", unns(id))
    )
    
    # Description
    observeEvent(input$data.description, {
      isolate(
        main.env$local.rv$data.files[row(), "description"] <- input$data.description
      )
      shinyFeedback::hideFeedback("data.description")
      if(isFALSE(isContentTruthy(input$data.description)))
        shinyFeedback::showFeedbackWarning("data.description", "Incomplete")
      else
        shinyFeedback::showFeedbackSuccess("data.description")
    },
    ignoreInit = FALSE,
    label = sprintf("EAL2: input description %s", unns(id))
    )
    
  })
}
