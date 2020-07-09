#' @title .app_server
#'
#' @description server part for the app's main script.
#'
#' @import shiny
#' @importFrom shinydashboard updateTabItems
#' @importFrom golem get_golem_options
#' @importFrom shinyjs onclick
.app_server <- function(input, output, session) {

  # get app arguments
  app.args <- golem::get_golem_options()
  dev <- app.args$dev
  main.env <- app.args$main.env

  # initialize global variables
  savevar <- NULL

  # DEV -----------------------------------------------------
  if (dev) {
    shinyjs::onclick("dev",
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

  ## modules called -----------------------------------------------------
  observeEvent(input$side_menu, {
    savevar <- switch(input$side_menu,
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
