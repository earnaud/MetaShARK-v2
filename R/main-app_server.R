#' @import shiny
#' @importFrom golem get_golem_options
.app_server <- function(input, output,session) {
  dev = golem::get_golem_options(which = 'dev')
  if(!is.logical(dev) || is.null(dev)) dev = FALSE
  # initialize global variables
  globals <- .globalScript(dev)
  savevar <- NULL

  # Head bar server
  # observeEvent(input$appOtions,{
  callModule(appOptions, "appOptions")
  # })
  observeEvent(input$close,{
    stopApp()
  })
  
  ## DEV: do things by clicking a button
  observeEvent(input$check,{
    browser()
  })
  observeEvent(input$appOptions, {
    updateTabItems(session, "side_menu", "appOptions")
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

  ## esthetics ----
  output$logo <- renderImage({ list(src = "media/MetaShARK_icon2.png",
                                    contentType = "image/png",
                                    width = "120px",
                                    height = "60px") },
                             deleteFile = FALSE)
}
