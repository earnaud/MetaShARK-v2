#' @title reactiveTrigger
#' 
#' @description create a 'reactiveTrigger' object (NOT an official R class) with two methods:
#' 1. depend() : must be written in a code chunk to execute on triggering
#' 2. trigger() : when executed, trigger the object (and all the "depending" code chunks)
makeReactiveTrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}
