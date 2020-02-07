#' @title .app_server
#'
#' @description server part for the app's main script.
#'
#' @importFrom shiny renderImage callModule observeEvent stopApp
#' @importFrom shinydashboard updateTabItems
#' @importFrom golem get_golem_options
.app_server <- function(input, output, session) {
  dev <- golem::get_golem_options(which = "dev")
  if (!is.logical(dev) || is.null(dev)) dev <- FALSE
  # initialize global variables
  globals <- .globalScript(dev)
  savevar <- NULL

  ## DEV: do things by clicking a button
  if(dev){
    observeEvent(input$check, {
      browser()
    })
  }
  ## esthetics ----
  output$logo <- renderImage({
    list(
      src = "inst/app/www/logo.png",
      contentType = "image/png",
      width = "200px",
      height = "40px"
    )
  }, deleteFile = FALSE)

  # Head bar server ----
  # Options
  observeEvent(input$appOptions, {
    updateTabItems(session, "side_menu", "appOptions")
  })

  callModule(appOptions, "appOptions")

  # Exit App
  observeEvent(input$close, {
    stopApp()
  })

  ## modules called ----
  observeEvent(input$side_menu, {
    savevar <- switch(input$side_menu,
      # welcome - no server
      # fill
      fill = callModule(fill, "fill", globals),
      # upload
      upload = callModule(upload, "upload", dev, globals),
      # doc
      documentation = callModule(documentation, "documentation", globals),
      # about
      about = callModule(about, "about"),
      # default
      NULL
    )
  })
}
