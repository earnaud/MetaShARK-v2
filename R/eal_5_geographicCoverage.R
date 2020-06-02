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
        tags$p("You can fill in geographic coverage through two
          methods: either  by chosing columns from your file, or 
          using the custom input UI. If both are filled, the software 
          will prefer to save the columns, but on proceeding to next
          step, the choice will be yours."),
        tags$p("Make sure all of your locations are stored in a
            single file, under 3 to 5 columns: one for the site description
            and one or two others for latitude and longitude. Southern latitude
            and western longitude shall be noted with negative values."),
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
      site.name = character(),
      lat.col = character(),
      lon.col = character()
    ),
    custom = reactiveValues(
      complete = FALSE,
      id = numeric(),
      coordinates = data.frame(
        geographicDescription = character(),
        northBoundingCoordinate = numeric(),
        southBoundingCoordinate = numeric(),
        eastBoundingCoordinate = numeric(),
        westBoundingCoordinate = numeric(),
        stringsAsFactors = FALSE
      )
    )
  )
  
  # Pre-fill -----------------------------------------------------
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
  
  # * Read saved values ----
  if(isTRUE(grepl("columns", names(savevar$emlal$GeoCov)))){
    site.name <- savevar$emlal$GeoCov$columns$site.name
    lat.col <- savevar$emlal$GeoCov$columns$lat.col
    lon.col <- savevar$emlal$GeoCov$columns$lon.col
    
    if(any(grepl(site.name[1], columns)))
      rv$columns$site.name <- site.name
    if(any(grepl(lat.col[1], columns.coordinates)))
      rv$columns$lat.col <- lat.col
    if(any(grepl(lon.col[1], columns.coordinates)))
      rv$columns$lon.col <- lon.col
  }
  if(isTRUE(grepl("custom", names(savevar$emlal$GeoCov)))){
    saved_table <- savevar$emlal$GeoCov$custom$coordinates
    if (all(dim(saved_table) != 0)) {
      rv$custom$coordinates <- saved_table
    }
  }
  
  # * Initialize UI ----
  observeEvent(names(input), {
    if(length(rv$columns$site.name) > 0){
      init.site.name <- paste(
        rv$columns$site.name, 
        collapse = "///"
      )
      names(init.site.name) <- rv$columns$site.name[1]
    }
    else
      init.site.name <- c(None = "")
    if(length(rv$columns$lat.col) > 0){
      init.lat.col <- paste(
        rv$columns$lat.col, 
        collapse = "///"
      )
      names(init.lat.col) <- rv$columns$lat.col[1]
    }
    else
      init.lat.col <- c(None = "")
    if(length(rv$columns$lon.col) > 0){
      init.lon.col <- paste(
        rv$columns$lon.col, 
        collapse = "///"
      )
      names(init.lon.col) <- rv$columns$lon.col[1]
    }
    else
      init.lon.col <- c(None = "")
    
    updateSelectInput(
      session,
      "site",
      choices = c(None = "", columns),
      selected = init.site.name
    )
    updateSelectizeInput(
      session,
      "latitude",
      choices = columns.coordinates,
      selected = init.lat.col
    )
    updateSelectizeInput(
      session,
      "longitude",
      choices = columns.coordinates,
      selected = init.lon.col
    )
  }, ignoreInit = FALSE, once = TRUE)
  
  # Input management -----------------------------------------------------
  {
  # * Site description -----------------------------------------------------
  observeEvent(input$site, {
    if(length(input$site) == 0)
      rv$columns$site.name <- NULL
    
    req(isTruthy(input$site))
    
    rv$columns$site.name <- input$site %>%
      strsplit(., "///", TRUE) %>%
      unlist
    names(rv$columns$site.name) <- c("col","file")
  }, ignoreInit = TRUE)
  
  # * Latitude -----------------------------------------------------
  observeEvent(input$latitude, {
    latCols <- input$latitude
    
    # validity check
    if(length(latCols) == 0 ||
        (is.character(latCols) && latCols == "NA")){
      rv$columns$lat.col <- NULL
      rv$fileChoices <- columns.coordinates
    }
    
    # Update inputs. If already a choice is done, reduce the choices available
    if (length(latCols) >= 1) {
      rv$columns$lat.col <- strsplit(latCols, "///") %>% unlist
      names(rv$columns$lat.col) <- c("col","file")
      chosenFile <- rv$columns$lat.col[2]
      rv$fileChoices <- columns.coordinates[chosenFile]
    }
  }, ignoreInit = TRUE, priority = 1, ignoreNULL = FALSE)
  
  # * Longitude -----------------------------------------------------
  observeEvent(input$longitude, {
    lonCols <- input$longitude
    
    # validity check
    if(length(lonCols) == 0 ||
        (is.character(lonCols) && lonCols == "NA")){
      rv$columns$lon.col <- NULL
      rv$fileChoices <- columns.coordinates
    }
    
    # Update inputs. If already a choice is done, reduce the choices available
    if (length(lonCols) >= 1) {
      rv$columns$lon.col <- strsplit(lonCols, "///") %>% unlist
      names(rv$columns$lon.col) <- c("col","file")
      chosenFile <- rv$columns$lon.col[2]
      rv$fileChoices <- columns.coordinates[chosenFile]
    }
  }, ignoreInit = TRUE, priority = 1, ignoreNULL = FALSE)
  
  # * Update inputs -----------------------------------------------------
  observeEvent(rv$fileChoices, {
    req(rv$fileChoices)
    
    up.lat.col <- rv$columns$lat.col
    if(length(up.lat.col) > 0){
      up.lat.col <- paste(
        up.lat.col, 
        collapse = "///"
      )
      names(up.lat.col) <- rv$columns$lat.col[1]
    }
    up.lon.col <- rv$columns$lon.col
    if(length(up.lon.col) > 0){
      up.lon.col <- paste(
        up.lon.col, 
        collapse = "///"
      )
      names(up.lon.col) <- rv$columns$lon.col[1]
    }
    
    isolate({
      updateSelectizeInput(
        session,
        "latitude",
        choices = rv$fileChoices,
        selected = up.lat.col
      )
      updateSelectizeInput(
        session,
        "longitude",
        choices = rv$fileChoices,
        selected = up.lon.col
      )
    })
  }, priority = 0)
  
  # * Warnings ----
  observeEvent(rv$warnings, {
    browser()
  }, ignoreInit = TRUE)
  
  }
  # Fill custom -----------------------------------------------------
  # * Setup ----
  if(dim(rv$custom$coordinates)[1] > 0){
    sapply(1:nrow(rv$custom$coordinates), function(ind){
      id <- -ind
      rv$custom <- insertGeoCovInput(
        as.numeric(id), # from -n to -1
        rv$custom,
        ns,
        default = rv$custom$coordinates[ind]
      )
    })
  }
  
  # * Manage input ----
  onclick("addui", {
    show("slider_tips")
    rv$custom <- insertGeoCovInput(
      as.numeric(input$addui), # from 1 to n
      rv$custom,
      ns
    )
  })
  
  # Saves -----------------------------------------------------
  observeEvent({
    rv$columns$site.name
    rv$columns$lat.col
    rv$columns$lon.col
  },{
    rv$columns$complete <-isTruthy(rv$columns$site.name) &&
      isTruthy(rv$columns$lat.col) &&
      isTruthy(rv$columns$lon.col)
  }, ignoreNULL = FALSE)
  
  observeEvent(rv$custom$coordinates,{
    rv$custom$complete <- checkTruth(rv$custom$coordinates)
  }, ignoreNULL = FALSE)
  
  observeEvent({
    rv$columns$complete
    rv$custom$complete
  },{
    globals$EMLAL$COMPLETE_CURRENT <- any(rv$custom$complete, rv$columns$complete)
  })
  
  observeEvent(NSB$SAVE, {
    req(tail(globals$EMLAL$HISTORY,1) == "Geographic Coverage")
    
    savevar <- saveReactive(
      savevar,
      rv = list(GeoCov = rv),
      .method = if(isTRUE(rv$columns$complete))
        "columns"
      else if(isTRUE(rv$custom$complete))
        "custom"
      else
        "",
      .values = list(
        data.content = data.content,
        data.content.coordinates = data.content.coordinates
      ),
      globals = globals
    )
    showNotification(
      "Geographic Coverage has been saved",
      type = "message"
    )
  }, ignoreInit = TRUE)
  
  # Process data -----------------------------------------------------
  # * Previous ----
  observeEvent(NSB$PREV, {
    req(globals$EMLAL$CURRENT == "Geographic Coverage")
    
    if (!"Categorical Variables" %in% globals$EMLAL$HISTORY)
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE - 1
  }, ignoreInit = TRUE)
  
  # * Next ----
  observeEvent(NSB$NEXT, {
    req(globals$EMLAL$CURRENT == "Geographic Coverage")
    
    # Create modal
    choices <- c(
      "Columns selection" = if (rv$columns$complete) "columns" else NULL,
      "Custom edition" = if (rv$custom$complete) "custom" else NULL #,
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
  }, priority = 1, ignoreInit = TRUE)
  
  onclick("confirm", {
    removeModal()
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + 1
    NSB$tagList <- tagList()
    
    savevar <- saveReactive(
      savevar, 
      rv = list(GeoCov = rv),
      .method = input$method,
      .values = list(
        data.content = data.content,
        data.content.coordinates = data.content.coordinates
      ),
      globals = globals
    )
    showNotification(
      "Geographic Coverage has been written.",
      type = "message"
    )
  })
  
  # Output -----------------------------------------------------
  return(savevar)
}