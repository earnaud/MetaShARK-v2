#' @import shiny
printReactiveValues <- function(values) {
  sapply(
    names(values),
    function(nn) {
      if (is.reactive(values[[nn]])) {
        values[[nn]]()
      } else {
        values[[nn]]
      }
    }
  )
}
