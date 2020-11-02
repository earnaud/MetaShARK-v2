#' @import shiny
#'
#' @noRd
CatVarsUI <- function(id, main.env) {
  ns <- NS(id)

  return(
    fluidPage(
      fluidRow(
        # Navigation
        fluidRow(
          column(
            1,
            actionButton(
              NS(id, "file_prev"),
              "",
              icon("chevron-left")
            )
          ),
          column(
            10,
            uiOutput(
              NS(id, "current_file"),
              inline = TRUE
            )
          ),
          column(
            1,
            actionButton(
              NS(id, "file_next"),
              "",
              icon("chevron-right")
            )
          ),
          style = "padding: 5px;"
        ),
        # content form
        uiOutput(NS(id, "edit_catvar"))
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom shinyjs toggleState
#' @importFrom shinyBS bsCollapse
#' @importFrom shinyFeedback hideFeedback showFeedbackSuccess showFeedbackDanger
#' @importFrom dplyr %>%
#'
#' @noRd
CatVars <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    
    # Variables initialization (deprecated)

    # Navigation buttons ----
    # Previous file
    observeEvent(input$file_prev, {
      req(main.env$EAL$page == 4)
      req(main.env$local.rv$current$index > 1)
      
      main.env$local.rv$current$index <- main.env$local.rv$current$index - 1
    },
    label = "EAL4: previous file"
    )

    # Next file
    observeEvent(input$file_next, {
      req(main.env$EAL$page == 4)
      req(main.env$local.rv$current$index < length(main.env$local.rv$cv.files))
      
      main.env$local.rv$current$index <- main.env$local.rv$current$index + 1
    },
    label = "EAL4: next file"
    )

    # En/disable buttons
    observe({
      req(main.env$EAL$page == 4)
      
      shinyjs::toggleState(
        "file_prev", 
        condition = main.env$local.rv$current$index > 1
      )
      shinyjs::toggleState(
        "file_next",
        condition = main.env$local.rv$current$index < length(main.env$local.rv$cv.files)
      )
    })
    
    # update table
    observeEvent({
      input$file_next
      input$file_prev
      main.env$EAL$page
    }, {
      req(main.env$EAL$page == 4)
      req(main.env$local.rv$current$index > 0)
      
      # shortcut for read variable
      main.env$local.rv$current$file <- basename(main.env$local.rv$cv.files[[main.env$local.rv$current$index]])
      .file.name <- main.env$local.rv$current$file
      
      # Changes
      # - remove NA from current table
      .table <- main.env$local.rv$cv.tables[[.file.name]]
      .table[is.na(.table)] <- ""
      main.env$local.rv$cv.tables[[.file.name]] <- .table
    },
    ignoreNULL = FALSE,
    label = "EAL4: update table",
    priority = -1
    )
    
    # Current file
    output$current_file <- renderUI({
      req(main.env$EAL$page == 4)
      tags$div(
        main.env$local.rv$current$file,
        class = "ellipsis",
        style = paste0(
          "display: inline-block;
          font-size:14pt;
          text-align:center;
          width:100%;
          background: linear-gradient(90deg, #3c8dbc ",
          round(100 * main.env$local.rv$current$index / length(main.env$local.rv$cv.files)),
          "%, white ",
          round(100 * main.env$local.rv$current$index / length(main.env$local.rv$cv.files)),
          "%);"
        )
      )
    })

    # Form ====
    
    # * UI ----
    output$edit_catvar <- renderUI({
      req(main.env$EAL$page == 4)

      .current.file <- main.env$local.rv$current$file
      isolate({
        .current.table <- main.env$local.rv$cv.tables[[.current.file]]
      })
      
      req(isContentTruthy(.current.table))
      
      do.call(
        shinyBS::bsCollapse,
        args = c(
          ... = lapply(
            unique(.current.table$attributeName),
            function(attribute)
              isolate({
                CatVarsInputUI(session$ns(attribute), attribute, main.env)
              })
          ),
          id = NS(id, "collapse")
        ) # end of bsCollapse
      ) # end do.call
    }) # end of renderUI

    # * Server ----
    observeEvent({
      input$file_next
      input$file_prev
      main.env$EAL$page
    }, {
      req(main.env$EAL$page == 4)
      
      # Shortcuts
      .current.file <- main.env$local.rv$current$file
      isolate({
        .current.table <- main.env$local.rv$cv.tables[[.current.file]]
      })
      
      req(isContentTruthy(.current.table))
      
      lapply(
        unique(.current.table$attributeName),
        function(attribute){
          CatVarsInput(attribute, attribute, main.env)
        }
      )
    },
    ignoreNULL = FALSE,
    label = "EAL4: set server",
    priority = -2
    ) # end of observeEvent
    
    # Saves ----
    observe({
      req(main.env$EAL$page == 4)
      invalidateLater(1000)
      req(isContentTruthy(main.env$local.rv$cv.files))
      
      main.env$EAL$completed <- sapply(
        seq(names(main.env$local.rv$cv.tables)),
        function(.file.index) {
          .file <- names(main.env$local.rv$cv.tables)[.file.index]
          .table <- main.env$local.rv$cv.tables[[.file]]
          sapply(seq(.table$definition), function(.row.index) {
            # Check for a completed form
            .valid <- isTruthy(.table[.row.index, "definition"])
            
            # Add feedback to input form
            input.id <- paste0(
              .table[1, "attributeName"],
              "-", .table[1, "code"]
            )
            shinyFeedback::hideFeedback(input.id)

            if(isTRUE(.valid))
              shinyFeedback::showFeedbackSuccess(input.id)
            else
              shinyFeedback::showFeedbackDanger(
                input.id,
                text = "Invalid description provided."
              )
            
            # Return
            return(.valid)
          })
        }
      ) %>% all
    },
    label = "EAL4: continuous save"
    )

    # Process data (deprecated)
  })
}
