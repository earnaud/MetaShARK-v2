# fill.R

### IMPORTS ###
source("modules/fill/fill_functions.R")
library("data.table")

cat("* Loading Fill Guidelines: \n")
fillGuideline = as.list(readRDS("resources/fillGuideline.RData"))
cat("** Fill Guideline successfully loaded !\n")
minFillGuideline = as.data.frame(read.table("resources/minFillGuideline.tsv" ))
cat("** Minimized Fill Guideline successfully loaded !\n")

### UI ###
fillUI <- function(id, IM){
  ns <- NS(id)
  
  div(
    fluidRow(
      box(column(4,
                 selectInput(ns("item"),
                             "Select an item to fill:",
                             minFillGuideline$abrev,
                             selectize = FALSE)),
          column(8,
                 htmlOutput(ns("location")),
                 style="max-height: 10%;"),
          width = 12)
    ),
    fluidRow(
      uiOutput(ns("gui"))
    )
  )
}



### SERVER ###
fill <- function(input, output, session, IM){
  # This commented part is useful to get str() applied on a list
  # output$accessedPath <- reactive({gsub("((\\.\\. *)*\\$ )","\n\\1",
  #                                       gsub(" +"," ",
  #                                            capture.output(str(followPath(systemGuideline,input$item)))))})

  output$location <- renderText({ 
    row <- which(minFillGuideline[,2] == input$item)
    return(HTML(paste0("<b>SystemGuideline Location: </b>", as.character(minFillGuideline[row,1]))))
  })
  
  output$gui <- renderUI({
    row <- which(minFillGuideline[,2] == input$item)
    accessed <- followPath(systemGuideline, as.character(minFillGuideline[row,1]))
    
    ## processing
    rendered <- fillExplore(li = accessed, 
                            id = accessed$`R-Attributes`$XSDPATH,
                            lastName = input$item)
    # [additional processes]
    # browser()
    
    return(rendered)
  })
}