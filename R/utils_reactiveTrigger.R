#' @title reactiveTrigger
#'
#' @description
#' Create a 'reactiveTrigger' object (NOT a proper R class) with two methods:
#' 1. depend() : must be written in a code chunk to execute on triggering
#' 2. trigger() : when executed, trigger the object (and all the "depending" code chunks)
#' This function is freely reused from Dean Attali's work: check it out, it is 
#' [awesome](https://github.com/daattali/advanced-shiny/tree/master/reactive-trigger)
#'
#' @import shiny
#' 
#' @export
makeReactiveTrigger <- function(dev = FALSE, label = "") {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      if(isTRUE(dev)) message("[dev] triggered: ", label)
      rv$a
      invisible()
    },
    trigger = function() {
      if(isTRUE(dev)) message("[dev] trigger: ", label)
      rv$a <- isolate(rv$a + 1)
    }
  )
}
