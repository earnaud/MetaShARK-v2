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
#'   \item{test}{logical. Recod tests? (default to FALSE)}
#' }
#'
#' @details
#' MetaShARK (METAdata SHiny Automated Resource & Knowledge) is a web app
#' which is designed to help its user as much as possible for filling ecological
#' metadata. It uses the EML standard (cf. NCEAS work) to allow a full and
#' precise description of input datasets.
#' 
#' For server setup, see [this git](https://github.com/earnaud/MetaShARK_docker)
#'
#' @author
#' Elie Arnaud <elie.arnaud@mnhn.fr>
#'
#' @examples
#' # run this to launch MetaShARK
#' runMetashark()
#' 
#' @import shiny
#' 
#' @export
runMetashark <- function(...) {
  invisible(require("shinyBS"))
  invisible(require("shinyTree"))

  args <- list(...)
  args$dev <- isTRUE(args$dev)
  args$wip <- isTRUE(args$wip)
  args$reactlog <- isTRUE(args$reactlog) || isTRUE(args$dev)
  args$test <- isTRUE(args$test)
<<<<<<< HEAD
  assign("metashark.args", args, envir = .GlobalEnv)

  # .globalScript(args = args)
  # on.exit(message("### end of MetaShARK session ###\n"))
  on.exit(rm("metashark.args", envir = .GlobalEnv))
=======
  options(shiny.reactlog = args$reactlog)

  .globalScript(args = args)
  on.exit(message("### end of MetaShARK session ###\n"))
  on.exit(rm(main.env, envir = .GlobalEnv))
>>>>>>> 21780e3c7e17505ab12284e63b960fbb7e749dc8
  
  if(args$test) browser()
  runApp(shinyApp(ui = ui, server = server))
}
