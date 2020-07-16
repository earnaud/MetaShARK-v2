#' @import shiny
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyjs onclick
#'
#' @noRd
appServer <- function(input, output, session) {
message("* server")
  # get variables
  main.env <- get("main.env", .GlobalEnv)

  if (main.env$dev) {
    shinyjs::onclick(
      "dev",
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

  ## modules called ----
  fill("fill", main.env)
  upload("upload", main.env)
  documentation("documentation")
  about("about")
  settings("settings", main.env)
}
