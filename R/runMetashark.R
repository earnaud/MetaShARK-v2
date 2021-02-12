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

  # Set args in .GlobalEnv
  args <- list(...)
  args$dev <- isTRUE(args$dev)
  args$wip <- isTRUE(args$wip)
  args$reactlog <- isTRUE(args$reactlog) || isTRUE(args$dev)
  args$test <- isTRUE(args$test)
  assign("metashark.args", args, envir = .GlobalEnv)
  on.exit(rm("metashark.args", envir = .GlobalEnv))
  
  # Set steps in .GlobalEnv for UI purposes
  assign("ui.steps", c(
    "SelectDP",
    "Data_Files",
    "Attributes",
    "Categorical_Variables",
    "Geographic_Coverage",
    "Taxonomic_Coverage",
    "Personnel",
    "Miscellaneous",
    "Make_EML"
  ), envir = .GlobalEnv)
  on.exit(rm("ui.steps", envir = .GlobalEnv))
  
  # Set resourcePaths
  addResourcePath("media", system.file("media/", package = "MetaShARK"))
  on.exit(removeResourcePath("media"))
  
  # Ensure correct encoding
  options(encoding = 'UTF-8')
  Sys.setlocale("LC_ALL", "en_US.utf8") 
  
  if(args$test){
    browser()
    shinytest::recordTest(
      shinytest::ShinyDriver$new(
        "R/"
      )
    )
  }
  runApp(shinyApp(ui = ui, server = server))
}
