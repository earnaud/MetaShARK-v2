#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @importFrom shiny NS fluidPage fluidRow column tagList tags actionButton
#' @importFrom shinyjs hidden
GeoCovUI <- function(id, title, dev, data.files, coordPattern) {
  ns <- NS(id)
  
  return(
    fluidPage(
      fluidRow(
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
                  placeholder = "Please select an option below",
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
                  placeholder = "Please select an option below",
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
              column(
                2,
                actionButton(ns("addui"), "", icon("plus")),
              ),
              column(
                10,
                hidden(
                  tags$div(
                    id = ns("slider_tips"),
                    HTML("You can detail a more precise number by using the left/right (or down/up) arrows 
                      of your keyboard. Precision can be given at 0.01°. Items can be removed by
                      using the Delete (&#9003) keyboard."),
                    style = "position: left"
                  )
                )
              )
            ),
            tags$div(id = "inserthere")
          )
        )
      )
    ) # end of fluidPage
  ) # end of return
}

#' @title Geographic coverage
#'
#' @description server part for the Geographic Coverage module
#'
#' @importFrom shiny observeEvent callModule showNotification reactiveValues
#' @importFrom shinyBS updateCollapse
#' @importFrom stringr str_extract_all
#' @importFrom dplyr %>%
#' @importFrom EMLassemblyline template_geographic_coverage
#' @importFrom shinyjs onclick show
GeoCov <- function(input, output, session, 
  savevar, globals, NSB) {
  ns <- session$ns
  if(globals$dev)
    onclick("dev", {
      req(globals$EMLAL$NAVIGATE == 5)
      browser()
    }, asis=TRUE)
  
  # Variable initialization -----------------------------------------------------
  
  # Reactive Values
  rv <- reactiveValues(
    warnings = reactiveValues(
      latWarnings = NULL,
      lonWarnings = NULL
    ),
    fileChoices = character(),
    columns = reactiveValues(
      complete = FALSE,
      geographicDescription = character(),
      northBoundingCoordinate = numeric(),
      southBoundingCoordinate = numeric(),
      eastBoundingCoordinate = numeric(),
      westBoundingCoordinate = numeric()
    ),
    custom = reactiveValues(
      complete = FALSE,
      id = numeric(),
      geographicDescription = character(),
      northBoundingCoordinate = numeric(),
      southBoundingCoordinate = numeric(),
      eastBoundingCoordinate = numeric(),
      westBoundingCoordinate = numeric()
    )
  )
  
  # Pre-fill -----------------------------------------------------
  saved_table <- fread(
    paste0(savevar$emlal$SelectDP$dp_metadata_path, "/geographic_coverage.txt"),
    data.table = FALSE, stringsAsFactors = FALSE
  )
  
  # Retrieve geographic coverage
  if (all(dim(saved_table) != 0)) {
    savevar$emlal$GeoCov <- saved_table
    NSB$tagList <- tags$p("Geographic coverage already set.")
  }
  else
    NSB$tagList <- tagList()
  
  # Fill columns -----------------------------------------------------
  
  # Prepare content
  data.files <- savevar$emlal$DataFiles$datapath
  data.content <- lapply(data.files, readDataTable, stringsAsFactors = FALSE)
  names(data.content) <- basename(data.files)
  
  # format extracted content - keep latlon-valid columns
  data.content.coordinates <- lapply(
    names(data.content),
    function(data.filename) {
      df <- data.content[[data.filename]]
      df.num <- unlist(
        lapply(df, function(df.col) {
          all(grepl(globals$PATTERNS$LATLON, df.col))
        })
      )
      df[, df.num]
    }
  )
  names(data.content.coordinates) <- basename(data.files)
  
  # format choices for selectInputs
  columns <- GeoCovColumn(data.content, data.files)
  columns.coordinates <- GeoCovColumn(data.content.coordinates, data.files)
  
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
  
  # ** Site description -----------------------------------------------------
  observeEvent(input$site, {
    toRead <- input$site %>%
      gsub(")$", "", .) %>%
      strsplit(., " (", TRUE) %>%
      unlist()
    rv$columns$geographicDescription <- data.content[[toRead[2]]][[toRead[1]]]
  })
  
  # ** Latitude -----------------------------------------------------
  observeEvent(input$latitude,
    {
      # validity check
      latCols <- input$latitude
      validate(
        need(latCols != "NA", "No valid column selected.")
      )
      
      # initialize variables
      latCols <- organizeCoordinates(latCols, data.content)
      latPattern <- globals$PATTERNS$LATLON
      latWarnings <- NULL
      
      # extract queried
      tmp <- extractCoordinates(latCols, latPattern, latWarnings, c("N", "S"), rv)
      rv <- tmp$rv
      rv$warnings$latWarnings <- tmp$warnings
    },
    ignoreInit = TRUE,
    priority = 1
  )
  
  # displays warnings
  output$`latitude-warning` <- renderText({
    validate(
      need(is.null(rv$warnings$latWarnings), rv$warnings$latWarnings)
    )
    return(NULL)
  })
  
  # ** Longitude -----------------------------------------------------
  observeEvent(input$longitude,
    {
      # validity check
      lonCols <- input$longitude
      validate(
        need(lonCols != "NA", "No valid column selected.")
      )
      
      # initialize variables
      lonCols <- organizeCoordinates(lonCols, data.content)
      lonPattern <- globals$PATTERNS$LATLON
      lonWarnings <- NULL
      
      # extract queried
      tmp <- extractCoordinates(lonCols, lonPattern, lonWarnings, c("E", "W"), rv)
      rv <- tmp$rv
      rv$warnings$lonWarnings <- tmp$warnings
    },
    ignoreInit = TRUE,
    priority = 1
  )
  
  # displays warnings
  output$`longitude-warning` <- renderText({
    validate(
      need(is.null(rv$warnings$lonWarnings), rv$warnings$lonWarnings)
    )
    return(NULL)
  })
  
  # * Update inputs -----------------------------------------------------
  # define column choices according to selectInput
  observe({
    # Default: all choices availabe
    fileChoices <- columns.coordinates
    
    # If already a choice is done, reduce the choices available
    if (length(input$latitude) >= 1 ||
        length(input$longitude) >= 1) {
      chosenFile <- gsub(".*\\((.*)\\)", "\\1", isolate(input$latitude))
      fileChoices <- columns.coordinates[chosenFile]
    }
    
    rv$fileChoices <- fileChoices
  })
  
  observe(
    {
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
    priority = 0
  )
  
  # Fill custom -----------------------------------------------------
  onclick("addui", {
    show("slider_tips")
    rv$custom <- insertGeoCovInput(as.numeric(input$addui), rv$custom, ns)
  })
  
  # Saves -----------------------------------------------------
  observe({
    rv$columns$complete <-isTruthy(rv$columns$geographicDescription) &&
      isTruthy(rv$columns$northBoundingCoordinate) &&
      isTruthy(rv$columns$southBoundingCoordinate) &&
      isTruthy(rv$columns$eastBoundingCoordinate) &&
      isTruthy(rv$columns$westBoundingCoordinate)
  })
  
  observe({
    rv$custom$complete <-isTruthy(rv$custom$geographicDescription) &&
      isTruthy(rv$custom$northBoundingCoordinate) &&
      isTruthy(rv$custom$southBoundingCoordinate) &&
      isTruthy(rv$custom$eastBoundingCoordinate) &&
      isTruthy(rv$custom$westBoundingCoordinate)
  })
  
  observe({
    globals$EMLAL$COMPLETE_CURRENT <- any(rv$custom$complete, rv$columns$complete)
  })
  
  observeEvent(NSB$SAVE, {
    req(tail(globals$EMLAL$HISTORY,1) == "Geographic Coverage")
    
    savevar <- saveReactive(
      savevar, 
      rv = c(GeoCov = rv),
      .write = FALSE
    )
    showNotification(
      "Geographic Coverage has been saved",
      type = "message"
    )
  }, ignoreInit = TRUE)
  
  # Process data -----------------------------------------------------
  observeEvent(NSB$PREV, {
    req(globals$EMLAL$CURRENT == "Geographic Coverage")
    
    if (!"Categorical Variables" %in% globals$EMLAL$HISTORY)
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE - 1
  }, ignoreInit = TRUE)
  
  observeEvent(NSB$NEXT, {
    req(globals$EMLAL$CURRENT == "Geographic Coverage")
    
    # Create modal
    choices <- c(
      "Columns selection" = if (rv$columns$complete) 1 else NULL,
      "Custom edition" = if (rv$custom$complete) 2 else NULL #,
    )
    
    req(isTruthy(choices))
    
    nextTabModal <- modalDialog(
      title = "Proceed Geographic Coverage",
      tagList(
        "You are getting ready to proceed. Please select one of the following:",
        radioButtons(
          ns("method"),
          "Method for Geographic Coverage:",
          choices = choices
        ),
        actionButton(ns("modal-dev"), "Dev")
      ),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("confirm"), "Proceed")
      )
    )
    
    showModal(nextTabModal)
  },
    priority = 1,
    ignoreInit = TRUE
  )
  
  onclick("confirm", {
    removeModal()
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + 1
    NSB$tagList <- tagList()
    
    savevar <- saveReactive(
      savevar, 
      rv = c(GeoCov = rv)
    )
    
    showNotification(
      "Geographic Coverage has been written.",
      type = "message"
    )
  })
  
  # Output -----------------------------------------------------
  return(savevar)
}