#' @title .app_server
#' 
#' @description server part for the app's main script.
#' 
#' @importFrom shiny renderImage callModule observeEvent stopApp
#' @importFrom golem get_golem_options
.app_server <- function(input, output,session) {
  dev = golem::get_golem_options(which = 'dev')
  if(!is.logical(dev) || is.null(dev)) dev = FALSE
  # initialize global variables
  globals <- .globalScript(dev)
  savevar <- NULL

  ## esthetics ----
  output$logo <- renderImage({ list(src = "logo.png",
                                    contentType = "image/png",
                                    width = "200px",
                                    height = "40px") },
                             deleteFile = FALSE)
  
  ## DEV: do things by clicking a button
  observeEvent(input$check,{
    browser()
  })
  
  # Head bar server ----
  # Options
  observeEvent(input$appOptions, {
    updateTabItems(session, "side_menu", "appOptions")
  })
  
  # Exit App
  callModule(appOptions, "appOptions")
  # })
  observeEvent(input$close,{
    stopApp()
  })
  
  ## modules called ----
  observeEvent(input$side_menu,{
    savevar <- switch(input$side_menu,
           # welcome - no server
           # fill
           fill = callModule(fill, "fill", globals),
           # upload
           upload = callModule(upload, "upload", dev),
           # doc
           documentation = callModule(documentation, "documentation"),
           # about
           about = callModule(about, "about"),
           # default
           NULL
    )
  })

}
