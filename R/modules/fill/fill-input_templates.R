# UI for input and its verbose
# Usage:
# inputRow(<input_id>,<input function as closure>, <input arguments as list>)
inputRow <- function(id, input_fun, input_args){
  if(!is.list(input_args)) input_args <- as.list(input_args)
  ns <- NS(id)
  
  fluidRow(
    column(4,
           do.call(input_fun, 
                   c(inputId=ns("input"),input_args))),
    column(8,
           tags$b("Currently input:"),
           uiOutput(ns("out"))),
    style="border: 1px solid lightgrey; margin: 5px; padding: 5px; width: 100%;"
  )
}

# Usage:
# callModule(inputRowServer, <input_id>, <output function as closure>)
inputRowServer <- function(input, output, session,
                           output_fun, output_args){
  if(!is.list(output_args)) output_args <- as.list(output_args)
  output$out <- do.call(output_fun, output_args)
}