# UI ====

#' @noRd
#'
#' @import shiny
tabPage <- function(title, ui, navTagList = NULL){
  tabPanelBody(
    value = title, 
    tags$span(
      div("EML Assembly Line", style = "padding-right: 15px"),
      # uiOutput(NS(id, "chain")),
      style = "display: inline-flex"
    ),
    if(is.null(navTagList))
      fluidRow(
        column(12, ui)
      )
    else
      fluidRow(
        column(10, ui),
        column(2, navTagList)
      )
  )
}

#' @import shiny
#'
#' @noRd
pagesUI <- function(id, parent.id, main.env){
  steps <- isolate(main.env$VALUES$steps)
  .nb <- length(steps)
  
  .ui.args <- vector("list", .nb)
  sapply(
    seq_along(steps),
    function(i, main.env) {
      page <- steps[i]
      
      .ui.args[[i]] <<- tabPage(
        title = page,
        ui = do.call(
          what = switch(i,
            "SelectDPUI",
            "DataFilesUI",
            "AttributesUI",
            "CatVarsUI",
            "GeoCovUI",
            "TaxCovUI",
            "PersonnelUI",
            "MiscUI",
            "MakeEMLUI"
          ),
          args = list(
            id = NS(parent.id, page),
            main.env = main.env
          )
        ),
        navTagList = if (i > 1)
          tagList(
            quitButton(id),
            saveButton(id),
            if (i != 2) prevTabButton(id, i),
            if (i != .nb) nextTabButton(id, i),
            tags$hr(),
            actionButton(NS(id, "help"), "Help", icon("question-circle"))
          )
        else 
          NULL
      )
    },
    main.env = main.env)
  
  .ui.args$id = NS(id, "wizard")
  .ui.args$type = "hidden"
  
  do.call("tabsetPanel", .ui.args)
}

# Server ====

#' @noRd
#' 
#' utility function for pages
changePage <- function(from, to) {
  observeEvent(input[[paste(from, to, sep = "_")]], {
    EAL$page <- EAL$page + to - from
    if(to > from)
      EAL$.next <- .EAL$.next+1
    if(from > to)
      EAL$.prev <- .EAL$.prev+1
  })
}

#' Wizard pages server
#' 
#' @noRd
pagesServer <- function(id, steps) {
  moduleServer(id, function(input, output, session) {
    steps <- main.env$VALUES$steps
    EAL <- main.env$EAL
    
    ids <- seq_along(steps)
    lapply(ids[-1], function(i) onQuit(i)) # modules
    lapply(ids[-1], function(i) onSave(i)) # modules
    lapply(ids[-1], function(i) changePage(i, i-1)) # observers
    lapply(ids[-length(steps)], function(i) changePage(i, i+1)) # observers
    
    # * Chain ====
    # output$chain <- renderUI({
    #   validate(
    #     need(.EAL$page > 1, "")
    #   )
    #   
    #   return(
    #     tags$span(
    #       tagList(
    #         lapply(seq(.EAL$history)[-1], function(ind) {
    #           .step.name <- .EAL$history[ind]
    #           
    #           if (.step.name != "Taxonomic Coverage") {
    #             .style <- "color: dodgerblue;"
    #             .description <- paste(.step.name, "(mandatory)")
    #           } else {
    #             .style <- "color: lightseagreen;"
    #             .description <- paste(.step.name, "(facultative)")
    #           }
    #           
    #           return(
    #             actionLink(
    #               ns(paste0("chain_", .step.name)),
    #               "",
    #               if (.step.name == .EAL$current) {
    #                 icon("map-marker")
    #               } else {
    #                 icon("circle")
    #               },
    #               style = .style
    #             ) %>% shinyBS::tipify(
    #               title = .description
    #               # , placement = "bottom"
    #               # , trigger = "hover"
    #             )
    #           ) # end of return
    #         }),
    #         paste0(
    #           "Step ", .EAL$page,
    #           "/", length(steps),
    #           ": ", .EAL$current
    #         )
    #       ),
    #       style = "position: right"
    #     )
    #   )
    # })
    # 
    # observe({
    #   validate(
    #     need(
    #       exists("main.env") && isTruthy(names(input)),
    #       "Not initialized"
    #     ),
    #     need(
    #       isTruthy(.EAL$history),
    #       "No history available"
    #     ),
    #     need(
    #       any(sapply(
    #         .EAL$history,
    #         grepl,
    #         x = names(input)
    #       ) %>% unlist()) &&
    #         length(.EAL$history) > 1,
    #       "No history available"
    #     )
    #   )
    #   
    #   sapply(seq(.EAL$history)[-1], function(.ind) {
    #     id <- paste0("chain_", .EAL$history[.ind])
    #     
    #     observeEvent(input[[id]], {
    #       req(input[[id]] && .ind != .EAL$page)
    #       .EAL$page <- .ind
    #       saveReactive() # Set this up correctly
    #       
    #       # trigger changes
    #       if(.ind > .EAL$page)
    #         EAL$.next <- EAL$.next + 1
    #       if(.ind < EAL$page)
    #         EAL$.prev <- EAL$.prev + 1
    #     })
    #   })
  })
}

# * Quit ====

#' @noRd
#'
#' @import shiny
quitButton <- function(id) {
  actionButton(
    NS(id, "quit"),
    "Quit",
    icon = icon("sign-out-alt"),
    width = "100%"
  )
}

#' @noRd
#'
#' @import shiny
#' @importFrom shinyjs onclick disable enable
onQuit <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    save.variable <- main.env$save.variable
    
    # modal dialog for quitting data description
    quitModal <- modalDialog(
      title = "You are leaving data description.",
      "Are you sure to leave? Some of your metadata have maybe not been saved.",
      easyClose = FALSE,
      footer = tagList(
        actionButton(NS(id, "cancel"), "Cancel"),
        actionButton(NS(id, "save_quit"), "Save & Quit"),
        actionButton(
          NS(id, "simple_quit"),
          "Quit",
          icon("times-circle"),
          class = "redButton"
        )
      )
    )
    
    # quits simply
    observeEvent(input$cancel,
      {
        req(input$quit)
        req(input$cancel)
        removeModal()
      },
      label = "Cancel quit"
    )
    
    # show modal on 'quit' button clicked
    observeEvent(input$quit,
      {
        req(input$quit)
        showModal(quitModal)
      },
      label = "EAL quit?"
    )
    
    # calls saveRDS method and quits
    observeEvent(input$save_quit_button,
      {
        req(input$quit)
        req(input$save_quit_button)
        
        # Save work at this state
        saveReactive(save.variable)
      },
      priority = 1,
      label = "EAL save+quit"
    )
    
    # quits simply
    observeEvent({
      input$simple_quit
      input$save_quit_button
    }, {
      req(input$quit)
      removeModal()
      
      # Clean & reset variables
      main.env$EAL$history <- "SelectDP"
      main.env$EAL$page <- 1
    },
      priority = 0,
      label = "EAL quit",
      ignoreInit = TRUE
    )
  })
}

# * Save ====

#' @noRd
#'
#' @import shiny
saveButton <- function(id) {
  actionButton(
    NS(id, "save"),
    "Save",
    icon = icon("save", class = "regular"),
    width = "100%"
  )
}

#' @noRd
#'
#' @import shiny
#' @importFrom shinyjs onclick disable enable
onSave <- function(id,  main.env){
  moduleServer(id, function(input, output, session) {
    observeEvent(input$save,
      {
        req(input$save)
        NSB$SAVE <- NSB$SAVE + 1
      },
      label = "NSB save"
    )
  })
}

# * Previous ====

#' @noRd
#'
#' @import shiny
prevTabButton <- function(id, i) {
  actionButton(
    NS(id, paste(i, i-1, sep = "_")),
    "Previous",
    icon = icon("arrow-left"),
    width = "100%"
  )
}

# * Next ====

#' @noRd
#'
#' @import shiny
nextTabButton <- function(id, i) {
  actionButton(
    NS(id, paste(i, i+1, sep = "_")),
    "Next",
    icon = icon("arrow-right"),
    width = "100%"
  )
}
