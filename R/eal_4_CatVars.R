#' @import shiny
#'
#' @noRd
CatVarsUI <- function(id) {
  ns <- NS(id)
  
  return(
    fluidPage(
      fluidRow(
        column(
          4,
          tags$h3("Variables list"),
          shinyTree::shinyTree(
            ns("tree")
          ),
          style = "
            overflow: scroll;
            max-height: 60vh;
            background-color: #e4e7ec;
          ") %>%
          shinycssloaders::withSpinner(),
        column(
          5, offset = 1,
          tags$div(
            id = ns("form"),
            tags$b(textOutput(ns("code"))),
            shinyjs::hidden(
              tags$div(
                id = ns("warning_missing_value"),
                helpText("This variable shall probably be described as a missing value in Attributes step.")
              )
            ),
            textAreaInput(
              inputId = ns("description"),
              label = NULL,
              value = ""
            )
          ),
          shinyjs::hidden(
            tags$div(
              id = ns("no_form"),
              helpText("Please select an attribute code first.")
            )
          )
        ),
        # Empty space
        column(2)
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom shinyjs toggleState
#' @importFrom shinyBS bsCollapse
#' @importFrom shinyFeedback hideFeedback showFeedbackSuccess showFeedbackDanger
#' @importFrom dplyr %>% filter slice select
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
        },
        label = "EAL4: dev"
      )
    }
    
    # Tree ====
    
    # * Render tree ----
    output$tree <- shinyTree::renderEmptyTree()
    outputOptions(output, "tree", suspendWhenHidden = FALSE)
    
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page == 4)
      req(isContentTruthy(main.env$local.rv$tree.content))
      devmsg("update tree", tag = "catvars")
      shinyTree::updateTree(
        session = session, 
        treeId = "tree", 
        data = main.env$local.rv$tree.content
      )
    },
    priority = -1,
    label = "EAL4: update tree")
    
    # * Get tree input ----
    # shinyTree selection
    .selected <- reactive({
      req(main.env$EAL$page == 4)
      if(isContentTruthy(input$tree)){
        get_selected(input$tree)
      } else {
        return(NULL)
      }
    },
    label = "EAL4: selected node")
    
    # shinyTree path exploration
    .ancestor <- reactive({
      req(main.env$EAL$page == 4)
      if(isContentTruthy(.selected())){
        attr(.selected()[[1]], "ancestry")
      } else {
        return(NULL)
      }
    },
    label = "EAL4: ancestry node")
    
    # boolean to know if a code is selected in the tree
    .code.selected <- reactive({
      req(main.env$EAL$page == 4)
      isTRUE(
        isContentTruthy(.selected()) &&
          length(.ancestor()) == 2
      )
    },
    label = "EAL4: selected code")
    
    output$code <- renderText({
      validate(
        need(.code.selected(), "")
      )
      paste("Description of", .selected()[[1]][1])
    })
    
    # UI setup
    observe({
      req(main.env$EAL$page == 4)
      shinyjs::toggle("form", condition = .code.selected())
      shinyjs::toggle("no_form", condition = !.code.selected())
      shinyjs::toggle("warning_missing_value", condition = grepl("empty", .selected()))
      
      if(.code.selected()) {
        if(grepl("empty", .selected())) {
          .index <- as.numeric(gsub("\\[(.*):empty\\]","\\1", .selected()[[1]][1]))
          .value <- main.env$local.rv$cv.tables[[.ancestor()[1]]] %>% 
            filter(attributeName == .ancestor()[2]) %>%
            slice(.index) %>%
            select(definition) %>% 
            unlist %>% 
            unname
        } else {
          .value <- main.env$local.rv$cv.tables[[.ancestor()[1]]] %>% 
            filter(attributeName == .ancestor()[2]) %>%
            filter(code == .selected()[[1]][1]) %>%
            select(definition) %>% 
            unlist %>% 
            unname # important !
        }
        if(!isContentTruthy(.value))
          browser()
        updateTextAreaInput(
          session = session,
          "description",
          value = .value
        )
      }
    },
    label = "EAL4: UI setup")
    
    # Form ====
    
    # * Get form input ----
    observeEvent(input$description, {
      req(main.env$EAL$page == 4)
      .table <- main.env$local.rv$cv.tables[[.ancestor()[1]]]
      .row.id <- which(
        .table$attributeName == .ancestor()[2] &&
          .table$code == .selected())
      .table[.row.id, "definition"] <- input$description
      main.env$local.rv$cv.tables[[.ancestor()[1]]] <- .table
      
      # Check value
      checkFeedback(input, "description")
    },
    label = "EAL4: get input")
    
    # * Completed ----
    observe({
      req(main.env$EAL$page == 4)
      req(isContentTruthy(main.env$local.rv$cv.tables))
      
      sapply(
        names(main.env$local.rv$cv.tables),
        function(.file.name) {
          sapply(
            unique(main.env$local.rv$cv.tables[[.file.name]]$attributeName),
            file.name = .file.name,
            function(.attribute.name, file.name) {
              main.env$local.rv$completed[[file.name]][[.attribute.name]] <- 
                isContentTruthy(
                  main.env$local.rv$cv.tables[[file.name]] %>%
                    filter(attributeName == .attribute.name) %>%
                    select(definition)
                )
            }
          )
        }
      )
    }, priority = -1,
    label = "EAL4: check completed")
    
  })
}
