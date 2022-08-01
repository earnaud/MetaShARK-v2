#' Clean modules
#'
#' Just a quick way to clean modules when quitting EAL fill-in.
#'
#' @noRd
cleanModules <- function(main_env) {
  # TODO maybe add more cleanings
  
  # Reset main_env$EAL
  main_env$EAL$history <- "SelectDP"
  main_env$EAL$old_page <- main_env$EAL$page
  main_env$EAL$page <- 1

  return(main_env)
}
