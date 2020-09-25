#' @title isContentTruthy
#'
#' @description check if `x` is truthy (as shiny::isTruthy) or not.
#' Returns the argument if truthy, or the `output` argument if not (default to NULL)
#'
#' @param x argument to check fo truthiness
#'
#' @import shiny
isContentTruthy <- function(x) {
  if (missing(x)) 
    stop("'x' is missing with no default.")
  
  return(isTruthy(x) && 
           ifelse(isTruthy(unlist(x)), sapply(unlist(x), isTruthy), FALSE))
}

isHTMLTruthy <- function(x) {
  if(missing(x))
    stop("'x' is missing with no default.")
  
  x <- gsub("<[^>]+>", "", x)
  return(isTruthy(x))
}

#' optional
#' 
#' Fill optional arguments of a function.
#' 
#' @param x value provided for the argument
#' @param type typed value (e.g. character()) in case option is invalid.
optional <- function(x, type = NULL) {
  if(isContentTruthy(x))
    return(x)
  else
    return(type)
}