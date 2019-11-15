#' @title makeReactiveTrigger
#'
#' @description produces a trigger generator object X in 2 parts:
#' - X$depend() : written in a shiny server code chunk, this chunk comes to
#' be triggered each time X$trigger() is evaluated
#' - X$trigger() : written in a shiny server code chunk, each time X$trigger()
#' is evaluated, code chunks containing X$depend() will be evaluated after.
#' All creadits for this function goes to Dean Attali
#'
#' @importFrom shiny reactiveValues isolate
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
