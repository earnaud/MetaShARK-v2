### templateShinyModule.R

# this document is not to be run

# snake_case terms are to be replaced with the wanted terms

ui_function <- function(id, arguments_){
  # necessary namespace
  ns = NS(id)
  
  # UI elements and their $arguments_
  # ui_element(ns(args), arguments_)
  
  # namespaced arguments will be able to react to server functions
  # who are called (cf. callModule) with the correct id
}

server_function <- function(input, output,session, arguments_){
  # anaything a server function would do without mentioning output
  # var
  
  # server_function is to be called in the main script using the
  # callModule() function with these arguments
  # - name of the server_function
  # - corresponding ui_element id
  # - arguments of the server_function
  # the output var is built in the main script
}

### main.R
library(shiny)

ui <- ui_layout(
  ui_function("id_")
)

server <- function(input, output, session){
  output <- callModule(server_function, "id_", arguments_)
}

shinyApp(ui,server)