# EMLAL.R

## Navigation UI ----

# quit dp edition
quitButton <- function(id){
  ns <- NS(id)
  actionButton(ns("quit"), "Quit",
               icon = icon("sign-out-alt"))
}

# save dp snapshot
saveButton <- function(id){
  ns <- NS(id)
  actionButton(ns("save"), "Save",
               icon = icon("save",class="regular"))
}

# next Tab
nextTabButton <- function(id){
  ns <- NS(id)
  actionButton(ns("nextTab"),"Next",
               icon = icon("arrow-right"))
}

# previous Tab
prevTabButton <- function(id){
  ns <- NS(id)
  actionButton(ns("prevTab"),"Previous",
               icon = icon("arrow-left"))
}

# navigation sidebar
navSidebar <- function(id, class = "navSidebar", 
                       .prev = TRUE, .next = TRUE, ...){
  ns <- NS(id)

  # variable initialization
  nexBut <- if(.next) nextTabButton(id) else HTML(NULL)
  preBut <- if(.prev) prevTabButton(id) else HTML(NULL)
  arguments <- list(...)
  div(h4("Navigation"),
      quitButton(id),
      saveButton(id),
      nexBut,
      preBut,
      arguments,
      class = class
  )
}

## Associated server functions ----

# on quit button 
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
  })
  
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

# on save button
# toSave is a structured list of reactiveValues (aka savevar in various modules)
onSave <- function(input, output, session, 
                   toSave, path, filename){
  ns <- session$ns
  
  observeEvent(input$save,{
    saveReactive(toSave, path, filename)
  })
}

# set the globals navigation ..
# .. one step after
nextTab <- function(input,output,session,
                    globals, previous){
  observeEvent(input$nextTab,{
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE+1
    globals$EMLAL$PREVIOUS <- previous
  }, priority = -1)
  
}
# .. one step before
prevTab <- function(input,output,session,
                    globals, previous){
  observeEvent(input$prevTab,{
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-1
    globals$EMLAL$PREVIOUS <- previous
  })
}

# Files management ----
# choose directory function
chooseDirectory = function(caption = 'Select data directory', default = "~/") {
  if (exists('utils::choose.dir')) {
    choose.dir(caption = caption) 
  } else {
    tk_choose.dir(default = default, caption = caption)
  }
}

# create DP directory
createDPFolder <- function(DP.location, DP.name, data.location){
  if(dir.exists(paste0(DP.location, DP.name))){
    unlink(paste0(DP.location,DP.name),recursive = TRUE)
    showModal(modalDialog(
      title = "Information: directory deleted",
      span(paste0(DP.location, DP.name), "has been deleted and replaced by a new empty data package."),
      footer = mmodalButton("Close"),
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
  RV$attributesTable[
    RV$current_attribute,
    ] <- printReactiveValues(RV$attributes)[
      names(RV$attributesTable)
      ]
  
  # (re)set local save reactive value with NULL values
  sapply(colnames(RV$attributesTable), function(nn){
    RV$attributes[[nn]] <- NULL
  })
  
  return(RV)
}

# build a unique id from file, attribute and colname - attribute_tables
buildInputID <- function(filename, attribute, colname){
  paste(filename, attribute, colname, sep = "_")
}

# customUnits server
customUnits <- function(input, output, session,
                        savevar){
  ns <- session$ns
  if(is.null(savevar$emlal$templateDP$customUnitsTable))
    savevar$emlal$templateDP$customUnitsTable <- fread(paste(savevar$emlal$selectDP$dp_path,
                                                             savevar$emlal$selectDP$dp_name,
                                                             "metadata_templates",
                                                             "custom_units.txt",
                                                             sep = "/")
    )
  # end of fread
  customUnitsTable <- savevar$emlal$templateDP$customUnitsTable
  # Custome Units Reactive Values
  curv <- reactiveValues()
  
  lapply(colnames(customUnitsTable),
         function(field){
           id <- ns(paste0("custom_",field))
           cat("Observe:",id,"\n")
           curv[[id]] <- reactive({ input[[id]] })
         })
  
  # output
  cat("Output:",names(curv),"\n")
  
  return(curv)
}

# Misc ----

# R to JS boolean
r2js.boolean <- function(condition){
  if(is.character(condition)) condition = as.logical(condition)
  return(tolower(as.character(condition)))
}

printReactiveValues <- function(values){
  sapply(names(values), 
         function(nn) 
           if(is.reactive(values[[nn]]))
             values[[nn]]()
         else
           values[[nn]]
  )
}


# # custom units inputs of EAL templates
# customUnitsUI <- function(input_id, customUnitsTable){
#   ns <- NS(input_id)
#   tagList(
#     column(1),
#     column(11,
#            lapply(c("id","unitType","parentSI","multiplierToSI","description"),
#                   function(field){
#                     id <- ns(paste0("custom_",field))
#                     cat("Enter:",id,"\n")
#                     switch(field,
#                            id = textInput(id,
#                                           span("Type an ID for your custom unit", class = "redButton"),
#                                           placeholder = "e.g.  gramsPerSquaredMeterPerCentimeter "),
#                            unitType = textInput(id,
#                                                 span("Type the scientific dimension using this unit in your dataset", class = "redButton"),
#                                                 placeholder = "e.g. mass, areal mass density per length"),
#                            parentSI = selectInput(id,
#                                                   span("Select the parent SI from which your unit is derivated",class = "redButton"),
#                                                   unique(get_unitList()$units$parentSI)),
#                            multiplierToSI = numericInput(id,
#                                                          span("Type the appropriate numeric to multiply a value by to perform conversion to SI",class = "redButton"),
#                                                          value = 1),
#                            description = textAreaInput(id,
#                                                        "Describe your custom unit")
#                     )
#                   }), # end of lapply
#     )
#   ) # end of taglist
# }