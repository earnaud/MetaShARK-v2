#' Clean modules
#' 
#' Just a quick way to clean modules when quitting EAL fill-in.
#' 
#' @noRd
cleanModules <- function(main.env) {
  # TODO maybe add more cleanings
  # Reset main.env$EAL
  main.env$EAL$history <- "SelectDP"
  main.env$EAL$old.page <- main.env$EAL$page
  main.env$EAL$page <- 1
  
  return(main.env)
}