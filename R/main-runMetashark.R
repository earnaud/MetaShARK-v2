#' @title Run MetaShARK
#'
#' @description Main function for launching the MetaShARK application.
#'
#' @usage
#' runMetaShark(...)
#'
#' @param server
#' Logical. Is the app deployed in server or not?
#' If FALSE (default), the app is deployed for a local desktop usage.
#' If TRUE, the app is deployed for a distant server usage. The main difference between
#' both is the way filesystems will be used.
#'
#' @param test
#' Logical. Is the application run with `{shinytest}`, or not. 
#' Default to FALSE.
#' 
#'
#' @param ... 
#' options to pass to the application, ignored if missing or mistyped.
#' \describe{
#'   \item{dev}{Logical. Add development elements in the GUI.}
#' }
#'
#' @details MetaShARK (METAdata SHiny Automated Resource & Knowledge) is a web app
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
#' @importFrom shiny shinyApp runApp
#' @importFrom golem with_golem_options
runMetashark <- function(server = FALSE, test = FALSE, ...) {
  args <- list(...)
  
  app <- with_golem_options(
    shinyApp(
      ui = .app_ui,
      server = .app_server,
      onStart = .headerScript
    ),
    golem_opts = c(server = server, args)
  )

  if(isFALSE(test))
    runApp(
      appDir = app
    )
  if(isTRUE(test))
    return(app)
}
