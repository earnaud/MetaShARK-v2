#' @import shiny
#' @importFrom shinydashboard updateTabItems
#' @importFrom golem get_golem_options
#' @importFrom shinyjs onclick
#'
#' @noRd
.app_server <- function(input, output, session) {

  # get app arguments
  app.args <- golem::get_golem_options()
  main.env <- app.args$main.env

  # initialize global variables
  save.variable <- NULL

  if (main.env$dev) {
    shinyjs::onclick("dev",
      {
        browser()
      },
      asis = TRUE
    )
  }

  # Head bar server ----
  # Options
  observeEvent(input$settings, {
    updateTabItems(session, "side_menu", "settings")
  })

  settings("settings", main.env)

  ## modules called ----
  fill("fill", main.env)
  upload("upload", main.env)
  documentation("documentation")
  about("about")
}
