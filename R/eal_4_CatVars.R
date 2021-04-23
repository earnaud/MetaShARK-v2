#' @import shiny
#'
#' @noRd
CatVarsUI <- function(id) {
  ns <- NS(id)
  
  return(
    fluidPage(
      # fluidRow(
      # uiOutput(NS(id, "edit_catvar")) %>%
      #   shinycssloaders::withSpinner()
      # ),
      fluidRow(
        column(
          4,
          tags$h3("Variables list"),
          shinyTree::shinyTree(
            # TODO add colors
            ns("tree") #,
            # types = "{ 'red-node': {'a_attr' : { 'style' : 'color:red' }},
            #   'green-node': {'a_attr' : { 'style' : 'color:green' }} }"
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
              id = "no_form",
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
        }
      )
    }
    
    # Tree ====
    
    # * Compute tree ----
    # treeContent <- eventReactive({
    #   main.env$EAL$page
    #   main.env$local.rv$cv.tables
    # }, {
    #   req(main.env$EAL$page == 4)
    #   .tables <- isolate(main.env$local.rv$cv.tables)
    #   req(isContentTruthy(.tables))
    #   
    #   lapply(
    #     names(.tables),
    #     function(.file.name) {
    #       # files
    #       structure(lapply(
    #         unique(.tables[[.file.name]]$attributeName),
    #         file.name = .file.name,
    #         function(.attribute.name, file.name){
    #           codes <- .tables[[file.name]] %>% 
    #             filter(attributeName == .attribute.name) %>% 
    #             select(code) %>% 
    #             unlist
    #           untruthy.codes <- which(!sapply(codes, isContentTruthy))
    #           codes.names <- replace(
    #             codes, 
    #             untruthy.codes,
    #             sprintf("[%s:empty]", untruthy.codes)
    #           )
    #           structure(lapply(
    #             codes,
    #             function(.code) {
    #               return(
    #                 structure(
    #                   .code,
    #                   # sttype="default",
    #                   sticon=""
    #                 )
    #               )
    #             }
    #           ),
    #           sticon = "fa fa-columns"
    #           ) %>% 
    #             setNames(codes.names)
    #         }
    #       ) %>%
    #         setNames(nm = unique(.tables[[.file.name]]$attributeName)), 
    #       # sttype = "root",
    #       sticon = "fa fa-file",
    #       stopened = TRUE
    #       )
    #     }
    #   ) %>% 
    #     setNames(nm = names(.tables))
    #   
    # })
    
    # * Render tree ----
    output$tree <- shinyTree::renderEmptyTree()
    outputOptions(output, "tree", suspendWhenHidden = FALSE)
    
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page == 4)
      req(isContentTruthy(main.env$local.rv$tree.content))
      devmsg("update tree", tag = "catvars")
      shinyTree::updateTree(session = session, treeId = "tree", data = main.env$local.rv$tree.content)
    },
    priority = -1,
    label = "catvars update tree")
    
    # * Get tree input ----
    # shinyTree selection
    .selected <- reactive({
      req(main.env$EAL$page == 4)
      if(isContentTruthy(input$tree)){
        get_selected(input$tree)
      } else {
        return(NULL)
      }
    })
    
    # shinyTree path exploration
    .ancestor <- reactive({
      req(main.env$EAL$page == 4)
      if(isContentTruthy(.selected())){
        attr(.selected()[[1]], "ancestry")
      } else {
        return(NULL)
      }
    })
    
    # boolean to know if a code is selected in the tree
    .code.selected <- reactive({
      req(main.env$EAL$page == 4)
      isTRUE(
        isContentTruthy(.selected()) &&
          length(.ancestor()) == 2
      )
    })
    
    output$code <- renderText({
      validate(
        need(.code.selected(), "")
      )
      paste("Description of", .selected()[[1]][1])
    })
    
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
            filter(code == .selected()) %>%
            select(definition) %>% 
            unlist %>% 
            unname # important !
        }
        updateTextAreaInput(
          session = session,
          "description",
          value = .value
        )
      }
    })
    
    # Form ====
    
    # * Get form input ----
    observeEvent(input$description, {
      req(main.env$EAL$page == 4)
      .table <- main.env$local.rv$cv.tables[[.ancestor()[1]]]
      .row.id <- which(
        .table$attributeName == .ancestor()[2] &&
          .table$code == .selected())
      .table[.row.id, "code"] <- input$description
      main.env$local.rv$cv.tables[[.ancestor()[1]]] <- .table
    })
    
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
    }, priority = -1)
    
  })
}
