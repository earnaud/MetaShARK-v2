# sample.R

## 3. Create DP template
sampleDPUI <- function(id, title, IM){
  ns <- NS(id)
  
  column(2,
         h4("Navigation"),
         quitButton(ns(id), style = rightButtonStyle),
         saveButton(ns(id), style = rightButtonStyle),
         actionButton(ns("nextTab"),"Next",
                      icon = icon("arrow-right"),
                      style = rightButtonStyle),
         actionButton(ns("prevTab"),"Previous",
                      icon = icon("arrow-left"),
                      style = rightButtonStyle),
         style = "text-align: center; padding: 0;"
  )
}

sampleDP <- function(input, output, session, IM, savevar, globalRV){
  ns <- session$ns
  
  # Navigation buttons ----
  callModule(onQuit, IM.EMLAL[5],
             # additional arguments
             globalRV, savevar,
             savevar$emlal$selectDP$dp_path, 
             savevar$emlal$selectDP$dp_name)
  callModule(onSave, IM.EMLAL[5],
             # additional arguments
             savevar$emlal, 
             savevar$emlal$selectDP$dp_path, 
             savevar$emlal$selectDP$dp_name)
  observeEvent(input$nextTab, {
    globalRV$navigate <- globalRV$navigate+1
    globalRV$previous <- "template"
  })
  observeEvent(input$prevTab, {
    globalRV$navigate <- globalRV$navigate-1
    globalRV$previous <- "template"
  })
  
  # Output ----
  return(savevar)
}