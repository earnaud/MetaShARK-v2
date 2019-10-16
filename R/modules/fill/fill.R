# fill.R

### UI ###
fillUI <- function(id, IM){
  ns <- NS(id)
  
  # h1("Under construction.")
  tabsetPanel(id = ns("tabs"),
    tabPanel(IM.EMLAL[2], EMLALUI(IM.EMLAL[1], IM.EMLAL)),
    tabPanel("MetaFIN", h1("Under Construction"))
  )
  
}



### SERVER ###
fill <- function(input, output, session, IM, globalRV){
  
  # variable initialization ----
  # save variable
  savevar <- initReactive()
  
  observeEvent(globalRV$navigate,{
    savevar$emlal$step <- max(
      globalRV$navigate,
      savevar$emlal$step
    )
    if(savevar$emlal$step > globalRV$navigateMAX)
      savevar$emlal$step <- globalRV$navigateMAX
  })
  
  return(savevar)
}