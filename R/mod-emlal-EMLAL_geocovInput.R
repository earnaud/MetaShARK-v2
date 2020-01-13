#' @title Geographic coverage modular Input
#'
#' @description UI part for the Geographic Coverage modular input, used
#' in Geographic Coverage module to input a custom entry in the dataset.
#'
#' @importFrom shiny NS fluidPage fluidRow column tagList tags actionButton
#' @importFrom shinyjs hidden
# @importFrom shinyBS bsTooltip
geoCovInputUI <- function(id, site_id, rmv_id, old_id) {
  ns <- NS(id)
  
  div_id <- id
  
  tags$div(
    id = site_id,
    fluidRow(
      column(2, "Description", style = "text-align: right"),
      column(9, textInput(ns("site_description"), "", site_id)),
      column(1,
        actionButton(
          ns(rmv_id), 
          "",
          icon("trash"), 
          class = "danger"
        )
      )
    ),
    fluidRow(
      column(
        6,
        sliderInput(
          ns(paste0("lat_", site_id)),
          tags$h4("Latitude"),
          min = -90, max = 90,
          value = c(-45,45),
          step = 0.01
        ),
        fluidRow(
          column(2, "South", style = "text-align: left"),
          column(2, "North", style = "text-align: right", offset = 8)
        )
      ),
      column(
        6,
        sliderInput(
          ns(paste0("lon_", site_id)),
          tags$h4("Longitude"),
          min = -180, max = 180,
          value = c(-90,90),
          step = 0.01
        ),
        fluidRow(
          column(2, "West", style = "text-align: left"),
          column(2, "East", style = "text-align: right", offset = 8)
        )
      )
    ),
    class = "inputBox"
  )
}

#' @title geoCovInput
#' 
#' @describeIn geoCovInputUI
#' 
#' @importFrom shiny observeEvent removeUI
#' @importFrom dplyr slice %>%
geoCovInput<- function(input, output, session, 
  rv, rmv_id, site_id, ref) {
  
  # Metadata acquisition ----
  localRV <- reactiveValues(
    id = ref,
    geographicDescription = "",
    northBoundingCoordinate = 0,
    southBoundingCoordinate = 0,
    eastBoundingCoordinate = 0,
    westBoundingCoordinate = 0
  )
  
  # site description
  observeEvent(input$site_description, {
    req(input$site_description)
    localRV$geographicDescription <- input$site_description
  }, priority = 1)
  # latitude
  observeEvent(input[[paste0("lat_", site_id)]], {
    req(input[[paste0("lat_", site_id)]])
    localRV$northBoundingCoordinate <- max( input[[paste0("lat_", site_id)]] )
    localRV$southBoundingCoordinate <- min( input[[paste0("lat_", site_id)]] )
  }, priority = 1)
  # longitude
  observeEvent(input[[paste0("lon_", site_id)]], {
    req(input[[paste0("lon_", site_id)]])
    localRV$eastBoundingCoordinate <- max( input[[paste0("lon_", site_id)]] )
    localRV$westBoundingCoordinate <- min( input[[paste0("lon_", site_id)]] )
  }, priority = 1)
  
  # Metadata save ----
  observeEvent({
    input$site_description
    input[[paste0("lat_", site_id)]]
    input[[paste0("lon_", site_id)]]
  }, {
    # Fetch correct index
    ind <- match(ref, rv$id)
    if(is.na(ind))
      ind <- length(rv$id)+1
    
    # print values into rv at selected index
    actualValues <- printReactiveValues(localRV)
    sapply(names(rv), function(rvid){
      rv[[rvid]][ind] <- actualValues[rvid]
      # rv[[rvid]][ind]
    })
    
  }, ignoreInit = TRUE, priority = 0)
  
  # Remove UI ----
  observeEvent(input[[rmv_id]], {
    # remove the UI
    removeUI(selector = paste0("#", site_id), immediate = TRUE)
    
    # unload the UI
    ind <- match(ref, rv$id)
    sapply(names(rv), function(rvid){
      rv[[rvid]] <- rv[[rvid]][-ind]
    })
    
  }, ignoreInit = TRUE, once = TRUE, autoDestroy = TRUE)
  
  # Output  
  return(rv)
}

#' @title insertGeoCovInput
#' 
#' @description Leads all the process of rendering a GeoCovInput
#' GUI and its associated server
#' 
#' @importFrom dplyr bind_rows
#' @importFrom shiny callModule
insertGeoCovInput <- function(id, rv, ns){
  # !!! warning: rv = rv$custom here !!!
  
  # initialize IDs ----
  div_id <- id
  site_id <- paste0("site_", div_id)
  rmv_id <-  paste0("rmv_", div_id)
  
  # Proper module server ----
  # create the UI 
  newUI <- geoCovInputUI(ns(div_id), site_id, rmv_id, id)
  
  # insert the UI
  insertUI(selector = "#inserthere", ui = newUI)
  
  # create the server
  rv <- callModule(geoCovInput, div_id,
    rv, rmv_id, site_id, id)
  
  # Output ----
  return(rv)
}

#' @title geoCovColumn
geoCovColumn <- function(data.content, data.files){
  columns <- lapply(
    names(data.content),
    function(data.filename){
      tmp <- paste0(colnames(data.content[[data.filename]]), " (", data.filename, ")")
      names(tmp) <- colnames(data.content[[data.filename]])
      return(tmp)
    })
  names(columns) <- basename(data.files)
  return(columns)
}

#' @title organizeCoordinates
organizeCoordinates <- function(coordCols, filesData){
  # initialize variables
  coordCols <- lapply(coordCols, function(col){ 
    tmp <- gsub(")","", col) %>%
      strsplit(., " (", TRUE) %>%
      unlist(., recursive = FALSE)
    return(
      filesData[[ tmp[2] ]][[ tmp[1] ]]
    )
  })
}

#' @title extractCoordinates
extractCoordinates <- function(coordCols, Pattern, localWarnings, coordTags, rv){
  # Extract proper coordinates
  coordinates <- lapply(
    coordCols,
    stringr::str_extract_all,
    Pattern, 
    simplify = TRUE
  ) %>% 
    lapply(., gsub, 
      pattern = ",",
      replacement = ".") %>%
    as.data.frame(stringsAsFactors = FALSE)
  coordinates[] <- lapply(coordinates, as.numeric)
  
  # Check for having only two columns
  if(dim(coordinates)[2] > 2){
    coordinates <- coordinates[,1:2]
    localWarnings <- c(localWarnings,
      "One of your column provides more than one coordinate per row. This might be a range. Only the two first values have been kept.")
  }
  
  if(dim(coordinates)[2] == 2){
    
    # assign West and East to columns
    coordinates[
      coordinates[,1] <= coordinates[,2],
      ] <- rev(coordinates[
        coordinates[,1] <= coordinates[,2],
        ])
    
    # Rename columns
    colnames(coordinates) <- coordTags
    if(all(coordinates[,1] <= coordinates[,2]))
      colnames(coordinates) <- rev(coordTags)
    
    if(identical(coordTags, c("N","S"))){
      rv$columns$northBoundingCoordinate <- coordinates$N
      rv$columns$southBoundingCoordinate <- coordinates$S
    }
    if(identical(coordTags, c("E","W"))){
      rv$columns$eastBoundingCoordinate <- coordinates$E
      rv$columns$westBoundingCoordinate <- coordinates$W
    }
    
    # Still wait for a second column.
  } else {
    localWarnings <- c(localWarnings,
      "Two numbers must be provided in either one or two columns.")
  }
  
  return(
    list(
      rv = rv,
      warnings = paste(localWarnings, collapse = " ")
    )
  )
  
}