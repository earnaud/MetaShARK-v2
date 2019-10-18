#' @import shiny
#' @import EML EMLassemblyline
.app_server <- function(input, output,session) {
  # initialize global variable
  globals <- .globalScript()
  savevar <- NULL
  
  ## DEV: do things by clicking a button
  observeEvent(input$check,{
    browser()
  })
  observeEvent(input$close,{
    stopApp()
  })
  
  ## modules called ----
  observeEvent(input$side_menu,{
    savevar <- switch(input$side_menu,
           # welcome - no server
           # fill
           fill = callModule(fill, "fill", globals),
           # # doc
           documentation = callModule(documentation, "documentation"),
           # # about
           about = callModule(about, "about"),
           NULL
    )
  })
  
  ## esthetics ----
  output$logo <- renderImage({ list(src = "media/MetaShARK_icon2.png",
                                    contentType = "image/png",
                                    width = "120px",
                                    height = "60px") },
                             deleteFile = FALSE)
}
