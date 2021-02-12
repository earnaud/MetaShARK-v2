#' Check for data structures validity
#'
#' Check if `x` is truthy (as shiny::isTruthy) or not, but also 
#' checks for its potential content. This function uses `unlist()` and `all()` to
#' check every bit of the variable.
#'
#' @param x argument to check fo truthiness
#' 
#' @return 
#' A logical indicating whether or not the variable is Truthy.
#' 
#' @import shiny
isContentTruthy <- function(x) {
  if (missing(x)) 
    stop("'x' is missing with no default.")
  
  return(isTruthy(x) && 
           ifelse(isTruthy(unlist(x)), sapply(unlist(x), isTruthy), FALSE))
}

#' Check for HTML tags validity
#' 
#' Check if a HTML tag is well formed, according to opening and closing chevrons.
#' 
#' @param x HTML tag. Target of the evaluation.
#' 
#' @return 
#' A logical value indicating whether or not the tag is truthy.
#' 
#' @export
isHTMLTruthy <- function(x) {
  if(missing(x))
    stop("'x' is missing with no default.")
  
  x <- gsub("<[^>]+>", "", x)
  return(isTruthy(x))
}

#' optional
#' 
#' Fill optional arguments of a function if the given argument comes to be not
#' truthy (e.g. by getting it after a script execution and passing it to a 
#' function).
#' 
#' @param x value provided for the argument
#' @param type typed value (e.g. character()) in case option is invalid.
#' 
#' @export
optional <- function(x, type = NULL) {
  if(isContentTruthy(x))
    return(x)
  else
    return(type)
}