insertCustomGeoCov <- function(id, main.env, default = NULL) {
  .cid <- as.character(id)
  # create the UI
  new.ui <- customGeoCov_UI(id, default = default)
  # insert the UI
  insertUI(selector = "#inserthere", ui = new.ui)
  # create the server
  main.env[[.cid]] <- reactiveValues()
  main.env[[.cid]] <- customGeoCov(unns(id), main.env)
}

customGeoCov_UI <- function(id, lat = c(30,60), lon = c(-15,35)) {
  ns <- NS(id)
  
  tags$div(
    shinyWidgets::numericRangeInput(ns("latitude"), "Latitude", value = lat),
    shinyWidgets::numericRangeInput(ns("longitude"), "Longitude", value = lon),
    colorPickerInput(ns("color")),
    class = "inputBox"
  )
}

customGeoCov <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Get area ====
    area <- eventReactive({
      input$latitude
      input$longitude
    }, {
      # get latitude
      .lat <- if(isContentTruthy(input$latitude)) {
        if(length(input$latitude) == 2)
          input$latitude else if(length(input$latitude) == 1)
            rep(input$latitude, 2) else
              c(30,60)
      } else
        c(30,60)
      .lat[which(.lat < -180)] <- .lat[which(.lat < -180)] %% -180
      .lat[which(.lat > 180)] <- .lat[which(.lat > 180)] %% 180
      
      # get longitude
      .lng <- if(isContentTruthy(input$longitude)) {
        if(length(input$longitude) == 2)
          input$longitude else if(length(input$longitude) == 1)
            rep(input$longitude, 2) else
              c(-15,35)
      } else
        c(-15,35)
      .lng[which(.lng < -180)] <- .lng[which(.lng < -180)] %% -180
      .lng[which(.lng > 180)] <- .lng[which(.lng > 180)] %% 180
      
      # structure data
      list(lat = .lat, lng = .lng)
    })
    
    # Get color ====
    color <- reactive({
      input$color
    })
    
    observe({
      main.env[[id]] <- c(area(), color = color())
    })
    
  })
}

#' @importFrom scales brewer_pal
#' @importFrom shinyWidgets spectrumInput
colorPickerInput <- function(inputId) {
  shinyWidgets::spectrumInput(
    inputId = inputId,
    label = "Pick a color:",
    choices = list(
      list('black', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
      as.list(scales::brewer_pal(palette = "Blues")(9)),
      as.list(scales::brewer_pal(palette = "Greens")(9)),
      as.list(scales::brewer_pal(palette = "Spectral")(11)),
      as.list(scales::brewer_pal(palette = "Dark2")(8))
    ),
    selected = "#03F",
    options = list(`toggle-palette-more-text` = "Show more")
  )
}

#'
testCustomInput <- function() {
  ui <- fluidPage(
    actionButton("dev", "dev", width = "100%"),
    fluidRow(
      column(
        5,
        shinyWidgets::actionBttn(
          "add", "", icon("plus"), style = "simple", color="primary"
        ),
        tags$div(id="inserthere")
      ),
      column(
        7,
        leaflet::leafletOutput("leaflet")
      )
    )
  )
  
  server <- function(input, output, session) {
    customGeoCov("geocov")
    
    rv <- reactiveValues(
      count = 0,
      map = reactiveValues()
    )
    
    # Add UIs ====
    observeEvent(input$add, {
      rv$count <<- rv$count+1
      rv <- insertGeoCovInput(rv$count, rv$map)
    })
    
    # Render leaflet ====
    map <- reactive({
      leaflet::leaflet() |>
        leaflet::addTiles() #|>
        # leaflet::addRectangles(
        #   area()$lng[1],
        #   area()$lat[1],
        #   area()$lng[2],
        #   area()$lat[2],
        #   color = color()
        # )
    }) |> 
      debounce(1000)
    
    output$leaflet <- leaflet::renderLeaflet({
      map()
    })
  }
  
  shinyApp(ui, server)
}
