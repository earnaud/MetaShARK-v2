# Usage:
# create an instance like
# `myTrigger <- makeReactiveTrigger()`
# in the reactive chunk to be triggered:
# `myTrigger$depend()`
# and in the chunk causing the trigger:
# `myTrigger$trigger()`
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
