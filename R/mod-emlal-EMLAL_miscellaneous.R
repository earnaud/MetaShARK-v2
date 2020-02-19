#' @title MiscellaneousUI
#' 
#' 
MiscellaneousUI <- function(id, help_label = NULL, server=FALSE){
  ns <- NS(id)
  
  fluidRow(
    # file selection
    column(4,
      tags$b(paste0("Select '", gsub(".*-(.*)$","\\1", id), "' file.")),
      tags$br(),
      if(isTRUE(server))
        fileInput(
          ns("file"), 
          "",
          multiple = FALSE,
          buttonLabel = "Load file",
          icon = icon("file")
        )
      else
        shinyFilesButton(
          ns("file"), 
          "Load file",
          paste0("Select '", gsub(".*-(.*)$","\\1", id), "' file."),
          multiple = FALSE,
          icon = icon("file")
        ),
      textOutput(ns("selected"))
    ),
    # Content edition
    column(8,
      tagList(
        tags$b("Content"),
        help_label,
        textAreaInput(
          ns("content"), 
          "", 
          value = "",
          width = "100%",
          resize = "vertical"
        )
      )
    )
  ) # end of fluidRow
  
}

#' @title Miscellaneous
#' 
#' @import shinyFiles
Miscellaneous <- function(
  input, output, session, savevar, 
  rv
){
  # Variable initialization ----
  ns <- session$ns
  volumes = c(Home = fs::path_home(), getVolumes()())
  
  # Get content ----
  observeEvent(input$content,{
    req(input$content)
    rv$content <- input$content
  })
  
  # Get file ----
  if(isTRUE(server)){
    rv$file <- eventReactive(input$file, {
      req(input$file)
      input$file$datapath
    })
  }
  else{
    shinyFileChoose(input, "file",
      roots = volumes,
      session = session
    )
    rv$file <- eventReactive(input$file, {
      req(input$file)
      parseFilePaths(volumes, input$file)$datapath
    })
  }
    
  observeEvent(rv$file(),{
    req(rv$file())
    updateTextAreaInput(
      session,
      "content",
      value = readPlainText(rv$file())
    )
  })
  
  # UI Verbose ----
  output$selected <- renderText({
    paste(
      basename(rv$file()),
      "\n(in:", dirname(rv$file()), ")"
    )
  })
  
  # Output ----
  return(rv)
}