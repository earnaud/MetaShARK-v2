wipRow <- function(...) {
  tags$div(
    tags$div(style = "height: 10px", class = "topInputRow wip"),
    ...,
    class = "inputBox"
  )
}
