coordinateInput_UI <- function(id, value) {
  stopifnot("lat" %in% names(value) && "lon" %in% names(value))
  ns <- NS(id)

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

coordinateInput <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {

    # Get values ----

    # IMPORTANT
    # corrections were commented out since they prevented an area
    rv <- reactive({
      req(isTruthy(input$lat) && isTruthy(input$lon))
      # to cross longitude 180 and latitudes 90 and -90
      .lat <- if (isTruthy(input$lat)) input$lat else 0
      .lon <- if (isTruthy(input$lon)) input$lon else 0

      list(lat = .lat, lon = .lon)
    })

    # Correct inputs ----
    observe({
        req(isContentTruthy(rv()$lat))
        if (!identical(rv()$lat, input$lat)) {
          updateNumericInput(session, "lat", value = rv()$lat)
        }
      },
      priority = -1
    )

    observe({
        req(isContentTruthy(rv()$lon))
      
        if (!identical(rv()$lon, input$lon)) {
          updateNumericInput(session, "lon", value = rv()$lon)
        }
      },
      priority = -1
    )

    # Return values ----
    return(rv)
  })
}
