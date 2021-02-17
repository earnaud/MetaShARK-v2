#' @import shiny
#'
#' @noRd
CatVarsUI <- function(id) {
  ns <- NS(id)
  
  return(
    fluidPage(
      wipRow("This step might take some minutes to setup. (To be improved soon)"),
      fluidRow(
        uiOutput(NS(id, "edit_catvar")) %>%
          shinycssloaders::withSpinner()
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
    if (main.env$dev){
      observeEvent(
        main.env$dev.browse(), 
        {
          if (main.env$current.tab() == "fill" &&
              main.env$EAL$page == 4) {
            browser()
          }
        }
      )
    }
    
    # Form ====
    
    # * UI ----
    output$edit_catvar <- renderUI({
      req(main.env$EAL$page == 4)
      
      # validity check
      validate(
        need(
          isContentTruthy(main.env$local.rv$cv.tables),
          "No valid table provided."
        )
      )
      
      isolate({
        do.call(
          tabsetPanel,
          args = c(
            id = session$ns("tabset"),
            lapply(
              names(main.env$local.rv$cv.tables),
              main.env = main.env,
              # Table input - tab
              function(table.name, main.env) {
                # Set variables
                table <- main.env$local.rv$cv.tables[[table.name]]
                .id <- session$ns(table.name)
                
                # Render UI
                tabPanel(
                  title = table.name,
                  value = table.name,
                  # Create a container of collapsibles
                  do.call(
                    shinyBS::bsCollapse,
                    args = c(
                      id = NS(.id, "collapse"),
                      ... = lapply(
                        unique(table$attributeName),
                        function(attribute){
                          CatVarsInputUI(
                            id = NS(.id, gsub("-", "", attribute)),
                            attribute = attribute,
                            table.name = table.name,
                            main.env = main.env
                          )
                        }
                      )
                    )
                  ) # end do.call:bsCollapse
                ) 
              }
            ) # end lapply
          ) # end args
        ) # end do.call: tabsetpanel
      })
    }) # end of renderUI
    
    # * Server ----
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page == 4)
      
      # Get inputs
      sapply(
        names(main.env$local.rv$cv.tables), 
        main.env = main.env,
        id = id,
        # Table input - tab
        # not a module ! just multiple calls
        function(id, table.name, main.env) {
          table <- main.env$local.rv$cv.tables[[table.name]]
          
          # Set server
          sapply(
            unique(table$attributeName),
            function(attribute)
              CatVarsInput(
                # sub-namespace
                id = NS(table.name, attribute %>% gsub("-", "", .)),
                attribute = attribute,
                table.name = table.name,
                main.env = main.env
              )
          )
        }
      )
    },
    ignoreNULL = FALSE,
    label = "EAL4: set server",
    priority = -2
    ) # end of observeEvent
    
    # Completed ----
    observe({
      req(main.env$EAL$page == 4)
      
      invalidateLater(1000)
      # main.env$local.rv$trigger$depend()
      
      req(
        length(main.env$local.rv$cv.files) > 0 &&
          !any(sapply(main.env$local.rv$cv.files, identical, y = data.frame()))
      )
      
      if(isTruthy(input$tabset)){
        file.name = input$tabset
        shinyBS::updateCollapse(
          session,
          NS(file.name, "collapse"),
          style = do.call(
            args = list(file.name = file.name),
            function(file.name) {
              lapply(
                names(main.env$local.rv$completed[[file.name]]),
                file.name = file.name,
                function(file.name, attribute){
                  .valid <- main.env$local.rv$completed[[file.name]][[attribute]] %>%
                    listReactiveValues() %>%
                    unlist() %>%
                    all()
                  if(!.valid)
                    devmsg(.valid, tag = paste(file.name, attribute))
                  ifelse(
                    .valid,
                    "success",
                    "warning"
                  )
                }) %>%
                setNames(nm = names(main.env$local.rv$completed[[file.name]]))
            }
          )
        )
      }
      
    },
    priority = -1,
    label = "EAL4: collapse update"
    )
    
    observe({
      req(main.env$EAL$page == 4)
      
      invalidateLater(1000)
      
      main.env$EAL$completed <- main.env$local.rv$completed %>%
        listReactiveValues() %>%
        unlist %>%
        all %>%
        isTRUE
    },
    priority = -1,
    label = "EAL4: completeness check"
    )
    # Process data (deprecated)
  })
}
