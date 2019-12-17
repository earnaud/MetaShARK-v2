#' @title Run MetaShARK
#'
#' @description Main function for launching the MetaShARK application.
#' MetaShARK (METAdata SHiny Automated Resource & Knowledge) is a web app
#' which is designed to help its user as much as possible for filling ecological
#' metadata. It uses the EML standard (cf. NCEAS work) to allow a full and
#' precise description of input datasets.
#'
#' @param ... options to pass to the application, ignored if missing or mistyped.
#' \enumerate{
#'   \item{dev}{logical. Shall the Dev buttons appear?}
#' }
#'
#' @examples
#' runMetashark()
#' @author Elie Arnaud <elie.arnaud@mnhn.fr>
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options use_favicon favicon
runMetashark <- function(...) {
  use_favicon("inst/app/www/favicon.png")
  favicon(ico = "inst/app/www/favicon.png")

  with_golem_options(
    app = shinyApp(
      ui = .app_ui, server = .app_server,
      onStart = .headerScript
    ),
    golem_opts = list(...)
  )
}
