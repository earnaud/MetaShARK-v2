appOptionsUI <- function(id, dev) {
  ns <- NS(id)
  
  tagList(
    h1("Options"),
    fluidRow(
      column(8,
             h2("Metacat setup"),
             textAreaInput(ns("auth_token"), "Authentication token", width = "120%"),
             checkboxInput(ns("test_metacat"), "Test MetaCat", value = TRUE),
             actionButton(ns("metacat_save"),"Save")
      ),
      column(4,
             tags$b("To fetch your authentication token:"),
             tags$ul(
               tags$li("Login into your metacat through user interface."),
               tags$li("Navigate in the upper-right menu corner and click 'My profile'."),
               tags$li("Click the 'settings' tab and the 'Authentication token' panel."),
               tags$li("Copy paste the authentication token into the dedicated area on the left.")
             )
      ),
      class = "inputBox"
    )
  )
}

appOptions <- function(input, output, session) {
  
  observeEvent(input$metacat_save,{
    if(input$test_metacat)
      options(dataone_test_token = input$auth_token)
    else
      options(dataone_token = input$auth_token)
    message("Metacat options set.")
  })
}