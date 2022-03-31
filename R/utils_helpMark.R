#' @import shiny
#' @noRd
helpMark <- function(help_text) {
  tags$div(icon("question-circle"), class="helper", `data-help`=help_text)
}

helpLabel <- function(label_text, help_text) {
  tags$span(label_text, helpMark(help_text), style="display: inline-flex")
}
