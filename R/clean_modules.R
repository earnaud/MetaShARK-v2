cleanModules <- function(main.env) {
  
  
  # Reset main.env$EAL
  main.env$EAL$history <- "SelectDP"
  main.env$EAL$old.page <- main.env$EAL$page
  main.env$EAL$page <- 1
  
  return(main.env)
}