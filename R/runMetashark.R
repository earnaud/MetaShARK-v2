#' @title Run MetaShARK
#'
#' @description Main function for launching the MetaShARK application.
#'
#' @usage
#' runMetaShark(...)
#'
#' @param ...
#' options to pass to the application, ignored if missing or mistyped.
#' \describe{
#'   \item{wip}{logical. Shows WIP parts of the app.}
#'   \item{dev}{logical. Add development elements in the GUI.}
#' }
#'
#' @details
#' MetaShARK (METAdata SHiny Automated Resource & Knowledge) is a web app
#' which is designed to help its user as much as possible for filling ecological
#' metadata. It uses the EML standard (cf. NCEAS work) to allow a full and
#' precise description of input datasets.
#'
#' @examples
#' # run this to launch MetaShARK
#' runMetashark()
#' @author Elie Arnaud <elie.arnaud@mnhn.fr>
#'
#' @export
#' @import shiny
#' @importFrom golem with_golem_options
runMetashark <- function(...) {
  args <- list(...)
  args$dev <- isTRUE(args$dev)
  args$main.env <- .globalScript(args$dev)
  
  app <- with_golem_options(
    shinyApp(
      ui = .app_ui,
      server = .app_server,
      onStart = .headerScript
    ),
    golem_opts = c(args)
  )

  runApp(
    appDir = app
  )
}
