annotationsUI <- function(id, title, dev) {
  ns <- NS(id)

  return(
    fluidPage(
      
    ) # end of fluidPage
  ) # end of return
}

annotations <- function(input, output, session, savevar, globals) {
  ns <- session$ns

  template_annotations()
  
  # Output ----
  return(savevar)
}