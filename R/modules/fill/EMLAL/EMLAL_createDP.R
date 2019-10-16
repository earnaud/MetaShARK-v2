# EMLAL_create.R

## 2. CREATE DATA PACKAGE
createDPUI <- function(id, title, IM){
  ns <- NS(id)
  
  return(
      fluidPage(
        column(10,
               h4("Data files"),
               HTML("When selecting your files, you can't select
                    folders. You can delete file(s) from your 
                    selection by ticking their box and clicking 
                    the 'Remove' button.<br>"),
               div(
                 shinyFilesButton(ns("add_data_files"),
                                  "Load files",
                                  "Select data file(s) from your dataset",
                                  multiple = TRUE,
                                  icon = icon("plus-circle")),
                 style = "display: inline-block; vertical-align: top;"
               ),
               actionButton(ns("remove_data_files"),"Remove",
                            icon = icon("minus-circle"), 
                            style = redButtonStyle),
               uiOutput(ns("data_files"))
        ), # end of column 1
        column(2,
               h4("Navigation"),
               quitButton(id, style = rightButtonStyle),
               saveButton(id, style = rightButtonStyle),
               nextTabButton(id, style = rightButtonStyle),
               textOutput(ns("warning_data_size")),
               textOutput(ns("overwrite")),
               style = "text-align: center; padding: 0;"
        ) # end of column 2
      ) # end fluidPage
    ) # end return
}

createDP <- function(input, output, session, IM, savevar, globalRV){
  # ns <- session$ns
  
  # Variable initialization ----
  rv <- reactiveValues(
    # to save
    data_files = data.frame()
    # local only
  )
  volumes <- c(Home = HOME, getVolumes()())
  updateFileListTrigger <- makeReactiveTrigger()
  
  # On arrival on screen
  observeEvent(globalRV$previous, {
    # dev: might evolve in `switch` if needed furtherly
    rv$data_files <- if(globalRV$previous == "create") # from create button in selectDP
                          data.frame()
                        else
                          savevar$emlal$createDP$dp_data_files
    
    updateFileListTrigger$trigger()
  })
  
  # Navigation buttons ----
  callModule(onQuit, IM.EMLAL[4],
             # additional arguments
             globalRV, savevar,
             savevar$emlal$selectDP$dp_path, 
             savevar$emlal$selectDP$dp_name)
  callModule(onSave, IM.EMLAL[4],
             # additional arguments
             savevar, 
             savevar$emlal$selectDP$dp_path, 
             savevar$emlal$selectDP$dp_name)
  callModule(nextTab, IM.EMLAL[4],
             globalRV, "create")
  
  # Data file upload ----
  # Add data files
  shinyFileChoose(input, "add_data_files",
                  roots = volumes,
                  # defaultRoot = HOME,
                  session = session)
  
  observeEvent(input$add_data_files,{
    # validity checks
    req(input$add_data_files)
    
    # actions
    loadedFiles <- as.data.frame(
      parseFilePaths(volumes, input$add_data_files)
    )
    
    if(identical(rv$data_files, data.frame()))
      rv$data_files <- loadedFiles
    else{
      for(filename in loadedFiles$name){
        if(!grepl("\\.",filename))
          message(filename," is a folder.")
        else
          rv$data_files <- unique(rbind(rv$data_files,
                                        loadedFiles[loadedFiles$name == filename,])
          )
      }
    }
    
    # variable modifications
    savevar$emlal$createDP$dp_data_files <- rv$data_files
  })
  
  # Remove data files
  observeEvent(input$remove_data_files, {
    
    # validity check
    req(input$select_data_files)
    
    # actions
    rv$data_files <- rv$data_files[
      rv$data_files$name != input$select_data_files
      ,]
  })
  
  # Display data files
  output$data_files <- renderUI({
    
    updateFileListTrigger$depend()

    # actions
    if(!identical(rv$data_files, data.frame()) &&
       !is.null(rv$data_files)){
      enable("nextTab")
      checkboxGroupInput(ns("select_data_files"),
                         "Select files to delete (all files here will be kept otherwise)",
                         choices = rv$data_files$name)
    }
    else{
      disable("nextTab")
      return(NULL)
    }
  })
  
  # Warnings ----
  # data size
  output$warning_data_size <- renderText({
    if(sum(rv$data_files$size) > THRESHOLD$dp_data_files)
      paste("WARNING:", sum(rv$data_files$size),
            "bytes are about to be duplicated for data package assembly")
    else
      ""
  })
  
  # overwrite files
  output$warning_overwrite <- renderText({
    if(identical(dir(paste0(path,"/",dp,"/data_objects/")),
                 character(0))
       )
      paste("WARNING:", "Selected files will overwrite
            already loaded ones.")
    else
      ""
  })
  
  # Process files ----
  # Template table
  observeEvent(input$nextTab, {
    # variable initialization
    dp <- savevar$emlal$selectDP$dp_name
    path <- savevar$emlal$selectDP$dp_path
    
    # actions
    # -- copy files to <dp>_emldp/<dp>/data_objects
    browser()
    sapply(rv$data_files$datapath,
           file.copy, 
           to = paste0(path,"/",dp,"/data_objects/"),
           overwrite = TRUE)
    # -- modify paths in save variable
    tmp <- savevar$emlal$createDP$dp_data_files
    tmp$datapath <- sapply(rv$data_files$name,
             function(dpname){
               force(dpname)
               paste0(path,"/",dp,"/data_objects/",dpname)
             })
    tmp$metadatapath <- sapply(rv$data_files$name,
             function(dpname){
               force(dpname)
               paste0(path,"/",dp,"/metadata_templates/",
                      sub("(.*)\\.[a-zA-Z0-9]*$",
                          "attributes_\\1.txt",
                          dpname)
                      )
             })
    savevar$emlal$createDP$dp_data_files <- tmp
    
    # EMLAL templating function
    template_table_attributes(
      path = paste0(path,"/",dp,"/metadata_templates"),
      data.path = paste0(path,"/",dp,"/data_objects"),
      data.table = rv$data_files$name,
    )
  }, priority = 1)
  
  # Output ----
  return(savevar)
}
