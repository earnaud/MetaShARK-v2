#' @importFrom scales brewer_pal
#' @importFrom shinyWidgets spectrumInput
colorPickerInput <- function(inputId, label = "Pick a color:", width = "100%") {
  shinyWidgets::spectrumInput(
    inputId = inputId,
    label = label,
    choices = list(
      list('black', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
      as.list(scales::brewer_pal(palette = "Blues")(9)),
      as.list(scales::brewer_pal(palette = "Greens")(9)),
      as.list(scales::brewer_pal(palette = "Spectral")(11)),
      as.list(scales::brewer_pal(palette = "Dark2")(8))
    ),
    selected = "#03F",
    options = list(`toggle-palette-more-text` = "Show more"),
    width = width
  )
}