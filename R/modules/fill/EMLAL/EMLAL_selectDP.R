# EMLAL_functions.R

### UI ###
selectDPUI <- function(id, title, width=12, IM){
  ns <- NS(id)
  
  # UI output
  return(
    fluidPage(
      title = "Organize data packages",
      # Data package location
      with_tippy(
        fluidRow(
          column(4,
                 # actionButton(ns("dp_location"), "Choose directory")
                 shinyDirButton(ns("dp_location"),"Choose directory",
                                "DP save location", icon = icon("folder-open")
                                )
          ),
          column(8,
                 textOutput(ns("dp_location")),
                 style = "text-align: right;"
          ),
          style=paste(inputStyle, "text-overflow: ellipsis;")
        ),
        "This is the location where your data packages will be
        saved. A folder will be created, respectively named 
        after your input."
      ),
      fluidRow(
        # Use existing DP
        column(ceiling(width/2),
               h4("Edit existing data package",
                  style="text-align:center"),
               uiOutput(ns("dp_list")),
               actionButton(ns("dp_load"), "Load"),
               actionButton(ns("dp_delete"),"Delete",
                            style = redButtonStyle)
        ),
        # Create DP
        column(floor(width/2),
               h4("Create new data package",
                  style="text-align:center"),

               # Data package title
               textInput(ns("dp_name"), "Data package name",
                         placeholder = paste0(Sys.Date(),"_project")),
               textOutput(ns("warning_dp_name")),
               with_tippy(
                 div(id = "license-help",
                     selectInput(ns("license"),
                                 "Select an Intellectual Rights License:",
                                 c("CCBY","CC0"),
                                 multiple = FALSE)
                 ),
                 HTML("License: <br>
                      <b>CC0:</b> public domain. <br>
                      <b>CC-BY-4.0:</b> open source with authorship. <br>
                      For more details, visit Creative Commons.")
               ),
               # DP creation
               actionButton(ns("dp_create"),"Create")
       ) # end column2

      ) # end fluidRow
      
    ) # end fluidPage
    
  ) # end return
  
}

selectDP <- function(input, output, session, IM, DP.path, savevar, globalRV){
  
  # variable initialization ----
  ns <- session$ns
  parent_ns = NS(IM.EMLAL[1])
  
  # local values - to save will communicate with other modules
  rv <- reactiveValues(
    # to save
    # Default DP location
    dp_location = DP.path,
    dp_name = NULL,
    # local only
    dp_list = NULL,
    dp_license = NULL,
    warning_dp_name = NULL
  )
  volumes <- c(Home = HOME, base = getVolumes()())
  
  # DP location ----
  
  # chose DP location
  shinyDirChoose(input, ns("dp_location"),
                 roots = volumes,
                 # defaultRoot = HOME,
                 session = session)
  
  # update reactive value  
  observeEvent(input$dp_location, {
    # validity checks
    req(input$dp_location)
    
    # variable initialization
    save <- rv$dp_location
    
    # actions
    # rv$dp_location <- input$dp_location
    rv$dp_location <- parseDirPath(volumes, input$dp_location)
    if(is.na(rv$dp_location))
      rv$dp_location <- save
  })
  
  # Render selected DP location
  output$dp_location <- renderText({
    rv$dp_location
  })
  
  # DP load ----
  # fetch list of DP at selected location
  observeEvent(rv$dp_location, {
    dpList <- list.files(rv$dp_location, pattern = "_emldp$")
    if(length(dpList) != 0) rv$dp_list <- sub("_emldp","",dpList)
    else rv$dp_list <- NULL
  })
  
  # Render list of DP at selected location
  output$dp_list <- renderUI({
    if(!is.null(rv$dp_list))
       radioButtons(ns("dp_list"),
                    NULL,
                    choiceNames = c("None selected",rv$dp_list),
                    choiceValues = c("", rv$dp_list)
                    )
    else{
      disable("dp_load")
      disable("dp_delete")
      "No EML data package was found at this location."
    }

  })
  
  # toggle Load and Delete buttons
  observeEvent(input$dp_list, {
    if(input$dp_list != ""){
      enable("dp_load")
      enable("dp_delete")
    }
    else{
      disable("dp_load")
      disable("dp_delete")
    }
  })  
  
  # DP create ----
  
  # name inout
  observeEvent(input$dp_name, {
    
    # check for name validity
    rv$warning_dp_name <- c(
      # check for long enough name
      if(nchar(input$dp_name) < 3)
        "Please type a name with at least 3 characters."
      else
        NULL,
      # check for valid characters name
      if(!grepl("^[[:alnum:]_-]+$",input$dp_name) 
         && nzchar(input$dp_name))
        "Only authorized characters are alphanumeric, '_' (underscore) and '-' (hyphen)."
      else
        NULL,
      # check for double name
      if(!is.null(rv$dp_list) 
         && input$dp_name %in% rv$dp_list
         && input$dp_name != "")
        paste0(input$dp_name, " exists already at this location !")
      else
        NULL
    )
    
  })
  
  output$dp_list <- renderUI({
    if(!is.null(rv$dp_list))
       radioButtons(ns("dp_list"),
                    NULL,
                    choiceNames = c("None selected",rv$dp_list),
                    choiceValues = c("", rv$dp_list)
                    )
    else{
      disable("dp_load")
      disable("dp_delete")
      "No EML data package was found at this location."
    }
  })

  # warings for input name - toggle Create button
  output$warning_dp_name <- renderText({
    if(is.null(rv$warning_dp_name)){
      rv$dp_name <- input$dp_name
      enable("dp_create")
      return(NULL)
    }
    else{
      disable("dp_create")
      return(paste(rv$warning_dp_name, collapse = "\n"))
    }
  })
  
  # license choice
  rv$dp_license <- reactive({ input$license })
  
  # DP management - on clicks----
  
  # Create DP
  observeEvent(input$dp_create, {
    req(input$dp_name)
    
    # variable operation - legibility purpose
    dp <- rv$dp_name
    path <- paste0(rv$dp_location,dp,"_emldp")
    license <- rv$dp_license()
    
    # save in empty dedicated variable
    savevar$emlal <- initReactive("emlal", savevar)
    savevar$emlal$selectDP$dp_name <- dp
    savevar$emlal$selectDP$dp_path <- path
    
    # verbose
    message("Creating:",path,"\n", sep = "")
    
    # actions
    rv$dp_list <- c(rv$dp_list,dp)
    
    globalRV$navigate <- globalRV$navigate+1
    globalRV$previous <- "create"
    
    dir.create(path)
    saveReactive(savevar, path, dp) # initial "commit"
    template_directories(
      path,
      dp
    )
    template_core_metadata(
      path,
      license
    )
  })
  
  # Load DP
  observeEvent(input$dp_load, {
    req(input$dp_list)
    
    # variable operation - legibility purpose
    dp <- input$dp_list
    path <- paste0(rv$dp_location,dp,"_emldp")
    
    # verbose
    message("Loading:",path,"\n", sep = "") # to replace by loading DP
    
    # actions
    savevar$emlal <- initReactive("emlal", savevar)
    savevar$emlal <- readRDS(paste0(path,"/",dp,".rds"))$emlal
    globalRV$navigate <- ifelse(savevar$emlal$step > 1, # resume where max reached
                                savevar$emlal$step,
                                globalRV$navigate+1)
    globalRV$previous <- "load"
  })
  
  # Delete DP
  observeEvent(input$dp_delete, {
    req(input$dp_list)
    
    # variable operation - legibility purpose
    dp <- input$dp_list
    
    # actions
    showModal(
      modalDialog(
        title = "Delete data package?",
        paste("Are you sure to delete", dp, "?"),
        footer = tagList(
          modalButton("No"),
          actionButton(
            ns("delete_confirm"),"Yes",
            style = redButtonStyle
          )
        ) # end footer
      ) # end modalDialog
    ) # end showModal
  })
  
  # If deletion is confirmed
  observeEvent(input$delete_confirm,{
    # variable operation - legibility purpose
    dp <- input$dp_list
    path <- paste0(rv$dp_location,dp,"_emldp")
    
    # verbose
    message("Deleting:",path,"\n", sep = "") # to replace by deleting DP
    
    # actions
    unlink(path, recursive = TRUE)
    rv$dp_list <- rv$dp_list[rv$dp_list != dp]
    removeModal()
  })
  
  # Output ----
  return(savevar)
}