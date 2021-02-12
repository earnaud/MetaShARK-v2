#' List Reactive Values
#'
#' Allows to turn a `reactiveValues`` object into a non-reactive list. Uses
#' recursive method.
#'
#' @param rv reactiveValues. Target to turn into list.
#' @param lv internal. Verbose purposes.
#' @param name internal. Current root node name
#'
#' @import shiny
#' 
#' @export
listReactiveValues <- function(rv, lv = 0, name = "root") {
  if (missing(rv)) {
    stop("No reactiveValues provided")
  }
  if (!is.reactivevalues(rv)) {
    return(rv)
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
