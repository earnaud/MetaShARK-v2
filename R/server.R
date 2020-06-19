#' @title .app_server
#'
#' @description server part for the app's main script.
#'
#' @importFrom shiny renderImage callModule observeEvent stopApp
#' @importFrom shinydashboard updateTabItems
#' @importFrom golem get_golem_options
#' @importFrom shinyjs onclick
.app_server <- function(input, output, session) {

  # get app arguments
  app.args <- get_golem_options()
  dev <- app.args$dev
  main.env <- app.args$main.dev

  # initialize global variables
  savevar <- NULL

  # DEV -----------------------------------------------------
  if (dev) {
    onclick("dev",
      {
        req(input$side_menu != "fill")
        browser()
      },
      asis = TRUE
    )
  }

  # Head bar server -----------------------------------------------------
  # Options
  observeEvent(input$settings, {
    updateTabItems(session, "side_menu", "settings")
  })

  callModule(settings, "settings", main.env)

  # # Exit App - deprecated on server
  # observeEvent(input$close, {
  #   stopApp()
  # })

  ## modules called -----------------------------------------------------
  observeEvent(input$side_menu, {
    savevar <- switch(input$side_menu,
      # welcome
      # welcome = callModule(welcome, "welcome"),
      # fill
      fill = callModule(fill, "fill", main.env),
      # upload
      upload = callModule(upload, "upload", main.env),
      # doc
      documentation = callModule(documentation, "documentation"),
      # about
      about = callModule(about, "about"),
      # default
      NULL
    )
  })
}
