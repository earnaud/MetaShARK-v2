customGeoCov_UI <- function(id, lat = c(0,0), lon = c(0,0)) {
  ns <- NS(id)
  fluidRow(
    column(
      5,
      shiny::sliderInput(ns("latitude"), "Latitude", min = -90, max = 90, value = lat),
      shiny::sliderInput(ns("longitude"), "Longitude", min = -180, max = 180, value = lon)
    ),
    column(
      7,
      leaflet::leafletOutput(ns("leaflet"))
    )
  )
}

customGeoCov <- function(id, additional_arguments) {
  moduleServer(id, function(input, output, session) {
    
    output$leaflet <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>%
        leaflet::removeShape(session$ns("area")) %>%
        leaflet::addRectangles(
          input$longitude[1],
          input$latitude[1],
          input$longitude[2],
          input$latitude[2],
          layerId = session$ns("area")
        )
    })
  })
}