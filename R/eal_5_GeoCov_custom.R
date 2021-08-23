customGeoCov_UI <- function(id, lat = c(30,60), lon = c(-15,35)) {
  ns <- NS(id)
  
  tagList(
    shinyWidgets::numericRangeInput(ns("latitude"), "Latitude", value = lat),
    shinyWidgets::numericRangeInput(ns("longitude"), "Longitude", value = lon),
    colorPickerInput(ns("color"))
    #, column(
    #   7,
    #   leaflet::leafletOutput(ns("leaflet"))
    # )
  )
}

customGeoCov <- function(id) {
  moduleServer(id, function(input, output, session) {
   
  })
}

#' @importFrom scales brewer_pal
colorPickerInput <- function(inputId) {
  shinyWidgets::spectrumInput(
    inputId = inputId,
    label = "Pick a color:",
    choices = list(
      list('black', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
      as.list(brewer_pal(palette = "Blues")(9)),
      as.list(brewer_pal(palette = "Greens")(9)),
      as.list(brewer_pal(palette = "Spectral")(11)),
      as.list(brewer_pal(palette = "Dark2")(8))
    ),
    selected = "#03F",
    options = list(`toggle-palette-more-text` = "Show more")
  )
}