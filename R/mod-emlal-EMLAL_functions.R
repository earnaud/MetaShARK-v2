#' @title Navigation sidebar
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
navSidebar <- function(id, class = "navSidebar", 
                       .prev = TRUE, .next = TRUE, 
                       ...){
  ns <- NS(id)
  
  # variable initialization
  preBut <- if(.prev) prevTabButton(id) else HTML(NULL)
  nexBut <- if(.next) nextTabButton(id) else HTML(NULL)
  arguments <- list(...)
  tags$div(tags$h4("Navigation"),
      quitButton(id),
      saveButton(id),
      preBut,
      nexBut,
      arguments,
      class = class
  )
}

#' @describeIn navSidebar Quit button.
#' 
#' @importFrom shiny NS actionButton icon
quitButton <- function(id){
  ns <- NS(id)
  actionButton(ns("quit"), "Quit",
               icon = icon("sign-out-alt"))
}

#' @describeIn navSidebar Save button.
#' 
#' @importFrom shiny NS actionButton icon
saveButton <- function(id){
  ns <- NS(id)
  actionButton(ns("save"), "Save",
               icon = icon("save",class="regular"))
}

#' @describeIn navSidebar Next button.
#' 
#' @importFrom shiny NS actionButton icon
nextTabButton <- function(id){
  ns <- NS(id)
  actionButton(ns("nextTab"),"Next",
               icon = icon("arrow-right"))
}

#' @describeIn navSidebar Prev button.
#' 
#' @importFrom shiny NS actionButton icon
prevTabButton <- function(id){
  ns <- NS(id)
  actionButton(ns("prevTab"),"Previous",
               icon = icon("arrow-left"))
}

## Associated server functions ----

#' @title Navigation server
#' 
#' @description server part for shiny navigation sidebar module (see [navSidebar()]). 
#' The functions are very specific and thus are not exported.
#' 
#' @importFrom shiny modalDialog tagList modalButton actionButton icon observeEvent req showModal removeModal 
onQuit <- function(input, output, session, 
                   globals, toSave, path, filename){
  ns <- session$ns
  
  # modal dialog for quitting data description
  quitModal <- modalDialog(
    title = "You are leaving data description",
    "Are you sure to leave? Some of your metadata have maybe not been saved.",
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("save_quit_button"), "Save & Quit"),
      actionButton(ns("quit_button"), "Quit",icon("times-circle"),
                   class = "redButton")
    )
  )
  
  # show modal on 'quit' button clicked
  observeEvent(input$quit,{
    req(input$quit)
    showModal(quitModal)
  }, priority = -1)
  
  # calls saveRDS method and quits
  observeEvent(input$save_quit_button,{
    req(input$quit)
    removeModal()
    saveReactive(toSave, path, filename)
    globals$EMLAL$NAVIGATE <- 1
  })
  
  # quits simply
  observeEvent(input$quit_button,{
    req(input$quit)
    removeModal()
    globals$EMLAL$NAVIGATE <- 1
  })
}

#' @describeIn onQuit
#' 
#' @importFrom shiny observeEvent 
onSave <- function(input, output, session, 
                   toSave, path, filename){
  ns <- session$ns
  
  observeEvent(input$save,{
    saveReactive(toSave, path, filename)
  }, priority = -1)
}

#' @describeIn onQuit
#' 
#' @importFrom shiny observeEvent 
nextTab <- function(input,output,session,
                    globals, previous){
  observeEvent(input$nextTab,{
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE+1
    globals$EMLAL$PREVIOUS <- previous
  }, priority = -1)
  
}
#' @describeIn onQuit
#' 
#' @importFrom shiny observeEvent 
prevTab <- function(input,output,session,
                    globals, previous){
  observeEvent(input$prevTab,{
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-1
    globals$EMLAL$PREVIOUS <- previous
  }, priority = -1)
}

# Files management ----

#' @title create DP directory
#' 
#' @description EML assembly line convenience function for templating directories and
#' avoiding duplicating it. A similarly named and located directory will be whatever deleted.
#' 
#' @importFrom shiny showModal modalDialog span modalButton 
#' @importFrom EMLassemblyline template_directories
createDPFolder <- function(DP.location, DP.name, data.location){
  if(dir.exists(paste0(DP.location, DP.name))){
    unlink(paste0(DP.location,DP.name),recursive = TRUE)
    showModal(modalDialog(
      title = "Information: directory deleted",
      span(paste0(DP.location, DP.name), "has been deleted and replaced by a new empty data package."),
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  }
  
  template_directories(
    path = DP.location, 
    dir.name = DP.name
  )
}

# EAL Templates ----

# very local function
saveInput <- function(RV){
  # save attributes
  RV$attributesTable[RV$current_attribute,] <- printReactiveValues(
    RV$attributes
  )[names(RV$attributesTable)]
  
  return(RV)
}

# build a unique id from file, attribute and colname - attribute_tables
buildInputID <- function(filename, attribute, colname){
  paste(filename, attribute, colname, sep = "_")
}

# Misc ----

# R to JS boolean
r2js.boolean <- function(condition){
  if(is.character(condition)) condition = as.logical(condition)
  return(tolower(as.character(condition)))
}

#' @importFrom shiny is.reactive
printReactiveValues <- function(values){
  sapply(names(values), 
         function(nn) 
           if(is.reactive(values[[nn]]))
             values[[nn]]()
         else
           values[[nn]]
  )
}

# increase a variable by 1
passcat <- function(i){
  i <<- i+1
  cat(i,"\n")
}