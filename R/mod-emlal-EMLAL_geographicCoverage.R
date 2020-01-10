#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @importFrom shiny NS fluidPage fluidRow column tagList tags actionButton
#' @importFrom shinyjs hidden
geocovUI <- function(id, title, dev, data.files, coordPattern) {
  ns <- NS(id)
  
  return(
    fluidPage(
      # Features UI ----
      column(
        10,
        fluidRow(
          tags$h4(title),
          tags$p("This step is optional."),
          tags$p("Make sure all of your locations are stored in a
            single file, under 3 columns: one for the site description
            and two others for latitude and longitude. Southern latitude
            and western longitude can be noted with negative values."),
          bsCollapse(
            id = ns("method"),
            bsCollapsePanel(
              title = "Use dataset's geographic variables",
              value = 1,
              tagList(
                selectInput(
                  ns("site"), 
                  "Choose a column for sites:", 
                  c("None" = "")
                ),
                selectizeInput(
                  ns("latitude"),
                  "Choose a column for latitude:", 
                  c("None" = ""),
                  options = list(
                    placeholder = 'Please select an option below',
                    onInitialize = I('function() { this.setValue(""); }'),
                    maxItems = 2
                  )
                ),
                div(
                  id = ns("latiWarn"),
                  textOutput(ns("latitude-warning"))
                ),
                selectizeInput(
                  ns("longitude"), 
                  "Choose a column for longitude:", 
                  c("None" = ""),
                  options = list(
                    placeholder = 'Please select an option below',
                    onInitialize = I('function() { this.setValue(""); }'),
                    maxItems = 2
                  )
                ),
                div(
                  id = ns("longWarn"),
                  textOutput(ns("longitude-warning"))
                )
              )
            ),
            bsCollapsePanel(
              title = "Fill geographic template",
              value = 2,
              fluidRow(
                column(2,
                  actionButton(ns("addui"), "", icon("plus")),
                ),
                column(10,
                  hidden(
                    div(
                      id = ns("slider_tips"),
                      HTML("You can detail a more precise number by using the left/right (or down/up) arrows 
                      of your keyboard. Precision can be given at 0.01°. Items can be removed by
                      using the Delete (&#9003) keyboard."),
                      style = "position: left"
                    )
                  )
                )
              ),
              tags$div(id="inserthere")
            )
          )
        )
      ), # end of column1
      # Navigation UI ----
      column(
        2,
        navSidebar(ns("nav"),
          ... = tagList(
            if (dev) actionButton(ns("check"), "Dev Check")
          )
        )
      ) # end of column2
    ) # end of fluidPage
  ) # end of return
}

#' @title Geographic coverage
#'
#' @description server part for the Geographic Coverage module
#'
#' @importFrom shiny observeEvent callModule
#' @importFrom shinyBS updateCollapse
#' @importFrom stringr str_extract_all
#' @importFrom dplyr %>%
geocov <- function(input, output, session, savevar, globals) {
  ns <- session$ns
  
  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }
  
  # Variable initialization ----
  
  # Reactive Values
  rv = reactiveValues(
    warnings = reactiveValues(
      latWarnings = NULL,
      lonWarnings = NULL
    ),
    fileChoices = character(),
    columns = reactiveValues(
      complete = logical(),
      geographicDescription = character(),
      northBoundingCoordinate = numeric(),
      southBoundingCoordinate = numeric(),
      eastBoundingCoordinate = numeric(),
      westBoundingCoordinate = numeric()
    ),
    custom = reactiveValues(
      complete = logical(),
      id = character(),
      geographicDescription = character(),
      northBoundingCoordinate = numeric(),
      southBoundingCoordinate = numeric(),
      eastBoundingCoordinate = numeric(),
      westBoundingCoordinate = numeric()
    )
  )
  
  # Get all data in an object to avoid possible multiple loadings
  filesData <- lapply(savevar$emlal$DPfiles$dp_data_files$datapath, fread)
  names(filesData) <- basename(savevar$emlal$DPfiles$dp_data_files$datapath)
  
  # Pre-fill UI fields ----
  saved_table <- fread(
    paste(
      savevar$emlal$selectDP$dp_path,
      savevar$emlal$selectDP$dp_name,
      "metadata_templates",
      "geographic_coverage.txt",
      sep = "/"
    )
  )
  
  if(all(dim(saved_table) != 0)) {
    show(ns("slider_tips"))
    
    invisible(
      sapply(1:dim(saved_table)[1], function(ui_id){
        ui_id <- paste0(ui_id,"_loaded")
        rv <- insertGeoCovInput(ui_id, rv, ns)
        return(NULL)
      })
    )
  }
  
  # Use columns ----
  
  # Prepare content
  data.files <- savevar$emlal$DPfiles$dp_data_files$datapath
  data.content <- lapply(data.files, fread)
  names(data.content) <- basename(data.files)
  
  # format extracted content
  data.content.coordinates <- lapply(
    names(data.content),
    function(data.filename){
      df <- data.content[[data.filename]]
      df.num <- unlist(
        lapply(df, function(df.col){
          all(grepl(globals$PATTERNS$LATLON, df.col))
        })
      )
      df[,..df.num]
    }
  )
  names(data.content.coordinates) <- basename(data.files)
  
  # format choices for selectInputs
  columns <- geoCovColumn(data.content, data.files)
  columns.coordinates <- geoCovColumn(data.content.coordinates, data.files)
  
  # set selectInput choices
  updateSelectInput(
    session,
    "site",
    choices = c(None = "", columns)
  )
  
  updateSelectizeInput(
    session,
    "latitude",
    choices = columns.coordinates
  )
  updateSelectizeInput(
    session,
    "longitude",
    choices = columns.coordinates
  )
  
  # ** Site description ----
  observeEvent(input$site, { 
    toRead <- input$site %>%
      gsub(")$","",.) %>%
      strsplit(., " (", TRUE) %>%
      unlist
    rv$columns$geographicDescription <- filesData[[ toRead[2] ]][[ toRead[1] ]]
  })
  
  # ** Latitude ----
  observeEvent(input$latitude, {
    # validity check
    latCols <- input$latitude
    # TODO re-check this test
    validate(
      need(latCols != "NA", "No valid column selected.")
    )
    
    # initialize variables
    latCols <- organizeCoordinates(latCols, filesData)
    latPattern <- globals$PATTERNS$LATLON
    latWarnings <- NULL
    
    # extract queried 
    tmp <- extractCoordinates(latCols, latPattern, latWarnings, c("N", "S"), rv)
    rv <- tmp$rv
    rv$warnings$latWarnings <- tmp$warnings
  }, ignoreInit = TRUE, priority = 1)
  
  # displays warnings
  output$`latitude-warning` <- renderText({
    validate(
      need(is.null(rv$warnings$latWarnings), rv$warnings$latWarnings)
    )
    return("Correct")
  })
    
  # ** Longitude ----
  observeEvent(input$longitude, {
    # validity check
    lonCols <- input$longitude
    validate(
      need(lonCols != "NA", "No valid column selected.")
    )
    
    # initialize variables
    lonCols <- organizeCoordinates(lonCols, filesData)
    lonPattern <- globals$PATTERNS$LATLON
    lonWarnings <- NULL
    
    # extract queried 
    tmp <- extractCoordinates(lonCols, lonPattern, lonWarnings, c("E", "W"), rv)
    rv <- tmp$rv
    rv$warnings$lonWarnings <- tmp$warnings
    
  }, ignoreInit = TRUE, priority = 1)
  
  # displays warnings
  output$`longitude-warning` <- renderText({
    validate(
      need(is.null(rv$warnings$lonWarnings), rv$warnings$lonWarnings)
    )
    return(NULL)
  })
  
  # * Update inputs ----
  # define column choices according to selectInput
  observe({
    # Default: all choices availabe
    fileChoices <- columns.coordinates
    
    # If already a choice is done, reduce the choices available
    if(length(input$latitude) >= 1 ||
        length(input$longitude) >= 1){
      chosenFile <- gsub(".*\\((.*)\\)","\\1", isolate(input$latitude))
      fileChoices <- columns.coordinates[chosenFile]
    }
    
    rv$fileChoices <- fileChoices
  })
    
  observe({
    req(rv$fileChoices)
    
    isolate({
      # Update inputs
      updateSelectizeInput(
        session,
        "latitude",
        choices = rv$fileChoices,
        selected = input$latitude
      )
      updateSelectizeInput(
        session,
        "longitude",
        choices = rv$fileChoices,
        selected = input$longitude
      )
    })
  },
    # ignoreInit = TRUE, ignoreNULL = FALSE, 
    priority = 0)
  
  # Fill custom ----
  observeEvent(input$addui, {
    show("slider_tips")
  }, once = TRUE)
  
  observeEvent(input$addui, {
    rv$custom <- insertGeoCovInput(as.numeric(input$addui), rv$custom, ns)
  })
  
  # NSB ----
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    nextTab, "nav",
    globals, "geocov"
  )
  callModule(
    prevTab, "nav",
    globals
  )
  
  # Complete ----
  observe({
    if(isTruthy(rv$columns$geographicDescription) &&
        isTruthy(rv$columns$northBoundingCoordinate) &&
        isTruthy(rv$columns$southBoundingCoordinate) &&
        isTruthy(rv$columns$eastBoundingCoordinate) &&
        isTruthy(rv$columns$westBoundingCoordinate)
    ){
      rv$columns$complete <- TRUE
    } else {
      rv$columns$complete <- FALSE
    }
  })
  
  observe({
    if(isTruthy(rv$custom$geographicDescription) &&
        isTruthy(rv$custom$northBoundingCoordinate) &&
        isTruthy(rv$custom$southBoundingCoordinate) &&
        isTruthy(rv$custom$eastBoundingCoordinate) &&
        isTruthy(rv$custom$westBoundingCoordinate)
    ){
      rv$custom$complete <- TRUE
    } else {
      rv$custom$complete <- FALSE
    }
  })
  
  nextTabModal <- modalDialog(
    "You are getting ready to proceed.",
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("confirm"),"Proceed")
    )
  )
  
  observeEvent(input$confirm, {
    removeModal()
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE+1
  })
  
  # Process data ----
  observeEvent(input[["nav-prevTab"]], {
    if(tail(globals$EMLAL$HISTORY, 1) == "customUnits")
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-1
    else if(tail(globals$EMLAL$HISTORY, 1) == "template")
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-2
  })
  observeEvent(input[["nav-nextTab"]], {
    # TODO temporize nextTab passage
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-1
    showModal(nextTabModal)
  })
  
  # Output ----
  return(savevar)
}
