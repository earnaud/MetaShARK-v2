#' @title isContentTruthy
#'
#' @description check if `x` is truthy (as shiny::isTruthy) or not.
#' Returns the argument if truthy, or the `output` argument if not (default to NULL)
#'
#' @param x argument to check fo truthiness
#'
#' @import shiny
isContentTruthy <- function(x) {
  if (missing(x)) {
    stop("'x' is missing with no default.")
  }
  return(isTruthy(x) && isTruthy(unlist(x)))
}
