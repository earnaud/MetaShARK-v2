coordinateInput_UI <- function(inputId, value) {
  # value <- c(min(value), max(value))
  
  stopifnot("lat" %in% names(value) && "lon" %in% names(value))
  ns <- NS(inputId)
  
  tags$div(
    class = "coordinate-input",
    numericInput(
      ns("lat"),
      NULL,
      value$lat,
      min = -90,
      max = 90
    ),
    tags$span("°N - "),
    numericInput(
      ns("lon"),
      NULL,
      value$lon,
      min = -180,
      max = 180
    ),
    tags$span("°E")
  )
}

coordinateInput <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    
    # Get values ----
    rv <- reactive({
      # IMPORTANT 
      # corrections were commented out since they prevented an area 
      # to cross longitude 180 and latitudes 90 and -90
      .lat <- input$lat
      # .lat[which(.lat < -180)] <- .lat[which(.lat < -180)] %% -180
      # .lat[which(.lat > 180)] <- .lat[which(.lat > 180)] %% 180
      
      .lon <- input$lon
      # .lon[which(.lon < -180)] <- .lon[which(.lon < -180)] %% -180
      # .lon[which(.lon > 180)] <- .lon[which(.lon > 180)] %% 180
      
      list(lat = .lat, lon = .lon)
    })
    
    # Correct inputs ----
    observe({
      req(isContentTruthy(rv()))
      if(rv()$lat != input$lat)
        updateNumericInput(session, "lat", value = rv()$lat)
    }, priority = -1)
    
    observe({
      req(isContentTruthy(rv()))
      if(rv()$lon != input$lon)
        updateNumericInput(session, "lon", value = rv()$lon)
    }, priority = -1)
    
    # Return values ----
    return(rv)
  })
}