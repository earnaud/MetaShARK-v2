#' @title listReactiveValues
#'
#' @description Allows to turn a `reactiveValues`` object
#' into a list. Uses recursive method.
#'
#' @param rv reactiveValues to turn into list
#' @param lv (verbose purposes)
#'
#' @importFrom shiny isolate reactiveValuesToList is.reactivevalues
#' is.reactive
listReactiveValues <- function(rv, lv = 0, name = "root") {
  if (missing(rv)) {
    stop("No reactiveValues provided")
  }
  if (!is.reactivevalues(rv)) {
    stop("Provided `rv` is not a reactiveValues")
  }

  # Check for children
  children <- isolate(reactiveValuesToList(rv))
  n <- names(children)
  out <- lapply(seq_along(children), function(sub) {
    subrv <- children[[sub]]

    if (is.reactivevalues(subrv)) {
      listReactiveValues(subrv, lv + 1, name = n[sub])
    }
    else if (is.reactive(subrv)) {
      return(isolate(subrv()))
    }
    else {
      return(subrv)
    }
  })
  names(out) <- n

  return(out)
}
