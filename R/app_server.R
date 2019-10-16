#' @import shiny
.app_server <- function(input, output,session) {
  globals <- .globalScript()
  
  ## DEV: do things by clicking a button
  observeEvent(input$check,{
    browser()
  })
  
  ## modules called ----
  # welcome - no server
  # fill
  # doc
  callModule(documentation, "documentation")
  # about
  callModule(about, "about")
  
  ## esthetics ----
  
  output$logo <- renderImage({ list(src = "media/MetaShARK_icon2.png",
                                    contentType = "image/png",
                                    width = "240px",
                                    height = "120px") },
                             deleteFile = FALSE)
}
