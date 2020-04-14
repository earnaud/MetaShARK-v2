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
listReactiveValues <- function(rv, lv = 0){
  if(missing(rv))
    stop("No reactiveValues provided")
  if(!is.reactivevalues(rv))
    stop("Provided `rv` is not a reactiveValues")
    
  # Check for children
  children <- isolate(reactiveValuesToList(rv))
  n <- names(children)
  out <- sapply(seq_along(children), function(sub){
    subrv <- children[[sub]]
    
    if(is.reactivevalues(subrv)){
      message(rep("#", lv+1), " - ", n[sub])
      x(subrv, lv+1)
    }
    else if(is.reactive(subrv)){
      message(rep(" ", lv+1), " > ", n[sub])
      return(isolate(subrv()))
    }  
    else {
      message(rep(" ", lv+1), " > ", n[sub])
      return(subrv)
    }
  })
  names(out) <- n
  
  return(out)
}
