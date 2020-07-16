#' @title Run MetaShARK
#'
#' @description
#' Main function for launching the MetaShARK application.
#'
#' @usage
#' runMetashark(...)
#'
#' @param ...
#' options to pass to the application, ignored if missing or mistyped.
#' \describe{
#'   \item{wip}{logical. Shows WIP parts of the app. (default to FALSE)}
#'   \item{dev}{logical. Add development elements in the GUI. (default to FALSE)}
#'   \item{reactlog}{logical. Use reactlog? (default to TRUE)}
#' }
#'
#' @details
#' MetaShARK (METAdata SHiny Automated Resource & Knowledge) is a web app
#' which is designed to help its user as much as possible for filling ecological
#' metadata. It uses the EML standard (cf. NCEAS work) to allow a full and
#' precise description of input datasets.
#'
#' @author
#' Elie Arnaud <elie.arnaud@mnhn.fr>
#'
#' @examples
#' # run this to launch MetaShARK
#' runMetashark()
#'
#' @export
#' @import shiny
runMetashark <- function(...) {
  args <- list(...)
  args$dev <- isTRUE(args$dev)
  args$wip <- isTRUE(args$wip)
  args$reactlog <- if(is.null(args$reactlog)) TRUE else isTRUE(args$reactlog)
  
  .globalScript(args = args)
  on.exit(rm(main.env, envir=.GlobalEnv))
  
  runApp(shinyApp(ui = appUI, server = appServer))
}
