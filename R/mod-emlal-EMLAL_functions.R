#' @title navSidebarUI
#'
#' @description UI part for shiny navigation sidebar module
#'
#' @param id shiny id
#' @param class css class style. Default is set to "navSidebar":
#' .navSidebar {
#'   width: 100%;
#'   text-align: center;
#'   padding: 0;
#' }
#' @param .prev logical. Do you want a "Previous" actionButton? (prevTabButton)
#' @param .next logical. Do you want a "Next" actionButton? (nextTabButton)
#' @param ... UI tags set at the bottom of the navigation side bar
#'
#' @importFrom shiny NS HTML tags
navSidebarUI <- function(id, class = "navSidebar", 
  .prev = TRUE, .next=TRUE) {
  ns <- NS(id)
  
  div(
    id = "navsidebar",
    verticalLayout(
      tags$h4("Navigation"),
      quitButton(id),
      saveButton(id),
      if(.prev) prevTabButton(id) else NULL,
      if(.next) nextTabButton(id) else FALSE,
      # arguments,
      uiOutput(ns("NSB_customUI"))
    ),
    class = class
  )
}

#' @describeIn navSidebarUI
#'
#' @importFrom shiny NS actionButton icon
quitButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("quit"), "Quit",
    icon = icon("sign-out-alt"),
    width = "100%"
  )
}

#' @describeIn navSidebarUI
#'
#' @importFrom shiny NS actionButton icon
saveButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("save"), "Save",
    icon = icon("save", class = "regular"),
    width = "100%"
  )
}

#' @describeIn navSidebarUI
#'
#' @importFrom shiny NS actionButton icon
nextTabButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("nextTab"), "Next",
    icon = icon("arrow-right"),
    width = "100%"
  )
}

#' @describeIn navSidebarUI
#'
#' @importFrom shiny NS actionButton icon
prevTabButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("prevTab"), "Previous",
    icon = icon("arrow-left"),
    width = "100%"
  )
}

## Associated server functions -----------------------------------------------------

#' @title navSidebar
#'
#' @description server part for shiny navigation sidebar module (see [navSidebar()]).
#' The functions are very specific and thus are not exported.
#'
#' @param id shiny server id
#' @param globals MetaShARK globals variable
#' @param savevar MetaShARK savevar variable
#'
#' @importFrom shiny callModule
navSidebar <- function(id, globals, savevar) {
  NSB <- reactiveValues(
    SAVE = 0,
    NEXT = 0,
    PREV = 0,
    taglist = tagList()
  )
  
  NSB <- callModule(onQuit, id, globals, savevar, NSB)
  NSB <- callModule(onSave, id, savevar, NSB)
  NSB <- callModule(prevTab, id, globals,  NSB)
  NSB <- callModule(nextTab, id, globals, NSB)
  
  callModule(
    function(input, output, session, x = NSB$tagList) {
      output$NSB_customUI <- renderUI({x})
    },
    id
  )
  
  return(NSB)
}

#' @describeIn navSidebar
#'
#' @importFrom shiny modalDialog tagList modalButton actionButton
#' icon observeEvent req showModal removeModal
#' @importFrom shinyjs onclick
onQuit <- function(input, output, session,
  globals, savevar, NSB) {
  ns <- session$ns
  
  # modal dialog for quitting data description
  quitModal <- modalDialog(
    title = "You are leaving data description",
    "Are you sure to leave? Some of your metadata have maybe not been saved.",
    footer = tagList(
      modalButton("Cancel"),
      actionButton(
        ns("save_quit_button"),
        "Save & Quit"
      ),
      actionButton(
        ns("quit_button"),
        "Quit",
        icon("times-circle"),
        class = "redButton"
      )
    )
  )
  
  # show modal on 'quit' button clicked
  onclick("quit", {
    req(input$quit)
    showModal(quitModal)
  })
  
  # calls saveRDS method and quits
  onclick("save_quit_button", {
    req(input$quit)
    req(input$save_quit_button)
    removeModal()
    
    NSB$tagList <- tagList()
    NSB$SAVE <- NSB$SAVE+1
    saveReactive(savevar)
    globals$EMLAL$HISTORY <- "SelectDP"
    globals$EMLAL$NAVIGATE <- 1
    
    file.remove(
      list.files(
        savevar$emlal$SelectDP$dp_data_path,
        pattern = "preview_"
      )
    )
  })
  
  # quits simply
  onclick("quit_button", {
    req(input$quit)
    removeModal()
    
    NSB$tagList <- tagList()
    globals$EMLAL$HISTORY <- "SelectDP"
    globals$EMLAL$NAVIGATE <- 1
    
    file.remove(
      list.files(
        savevar$emlal$SelectDP$dp_data_path,
        pattern = "preview___"
      )
    )
  })
  
  return(NSB)
}

#' @describeIn navSidebar
#'
#' @importFrom shiny observeEvent
#' @importFrom shinyjs onclick
onSave <- function(input, output, session, savevar, NSB) {
  
  onclick("save", {
    req(input$save)
    NSB$SAVE <- NSB$SAVE+1
  })
  
  observeEvent(NSB$SAVE, {
    saveReactive(savevar)
  }, priority = -1, ignoreInit = TRUE)
  
  return(NSB)
}

#' @describeIn navSidebar
#'
#' @importFrom shiny observeEvent
#' @importFrom shinyjs onclick enable disable
nextTab <- function(input, output, session,
  globals, NSB) {
  observe({
    req(isTRUE(globals$EMLAL$COMPLETE_CURRENT) ||
        isFALSE(globals$EMLAL$COMPLETE_CURRENT))
    
    if (isFALSE(globals$EMLAL$COMPLETE_CURRENT)) {
      disable("nextTab")
    } else if (isTRUE(globals$EMLAL$COMPLETE_CURRENT)) {
      enable("nextTab")
    }
  })
  
  onclick("nextTab", {
    req(isTRUE(globals$EMLAL$COMPLETE_CURRENT))
    if(!globals$EMLAL$CURRENT %in% c("Geographic Coverage", "Taxonomic Coverage")){
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + 1
      NSB$tagList <- tagList()
    }
    NSB$NEXT <- NSB$NEXT+1
  })
  
  return(NSB)
}
#' @describeIn navSidebar
#'
#' @importFrom shiny observeEvent
#' @importFrom shinyjs onclick
prevTab <- function(input, output, session,
  globals, NSB) {
  
  onclick("prevTab", {
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE - 1
    NSB$tagList <- tagList()
    NSB$PREV <- NSB$PREV+1
  })
  
  return(NSB)
}

# Misc -----------------------------------------------------

# R to JS boolean
r2js.boolean <- function(condition) {
  if (is.character(condition)) condition <- as.logical(condition)
  return(tolower(as.character(condition)))
}

#' @importFrom shiny is.reactive
printReactiveValues <- function(values) {
  sapply(
    names(values),
    function(nn) {
      if (is.reactive(values[[nn]])) {
        values[[nn]]()
      } else {
        values[[nn]]
      }
    }
  )
}
