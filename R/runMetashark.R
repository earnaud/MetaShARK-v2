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
#'   \item{use.profvis}{logical. Profile the app with {profvis}? (default to FALSE)}
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
#' 
#' ## Not run:
#' 
#' library(MetaShARK)
#' 
#' # run this to launch MetaShARK
#' runMetashark()
#' 
#' ## End (Not run)
#' 
#' @import shiny
#' 
#' @include about_module_functions.R about_module.R app_header.R app_server.R app_ui.R clean_modules.R documentation_module.R eal_1_SelectDP.R eal_2_DataFiles_helpers.R eal_2_DataFiles.R eal_3_Attributes_dates.R eal_3_Attributes_helpers.R eal_3_Attributes.R eal_4_CatVars_helpers.R eal_4_CatVars.R eal_5_GeoCov_custom.R eal_5_GeoCov_helpers.R eal_5_GeoCov.R eal_6_TaxCov.R eal_7_ORCID_helpers.R eal_7_Personnel_helpers.R eal_7_Personnel.R eal_7_roleInput.R eal_8_Misc_helpers.R eal_8_Misc.R eal_9_MakeEML.R eal_x_Annotations_helpers.R eal_x_Annotations.R fill_module_saves.R fill_module_setup.R fill_module.R fill_pages.R metafin.R runMetashark.R settings_rightSideBar.R template_functions.R upload_module_functions.R upload_module.R utils_additional_HTML.R utils_collapsible.R utils_devmsg.R utils_followPath.R utils_grep.R utils_insertModule.R utils_isContentTruthy.R utils_listReactiveValues.R utils_listToStructure.R utils_listToXML.R utils_reactiveDirReader.R utils_reactiveTrigger.R utils_readDataTable.R utils_readHTMLFromMD.R utils_readPlaintext.R utils_rebuild.R utils_URL_Input.R utils_writeText.R utils_xml_handling.R welcome_module.R
#' 
#' @export
runMetashark <- function(...) {
  # Required -- dot not remove
  require(shinyTree)
  require(shinyBS)
  
  # Set args ====
  # in .GlobalEnv = local options
  args <- list(...)
  args$launch.browser <- isTRUE(args$launch.browser)
  
  # Dev - debug options
  args$dev <- isTRUE(args$dev)
  args$wip <- isTRUE(args$wip) || isTRUE(args$dev) # needed for some UI
  args$reactlog <- isTRUE(args$reactlog) || isTRUE(args$dev)
  args$use.profvis <- isTRUE(args$use.profvis)
  args$use.test <- isTRUE(args$use.test)
  
  # Set args in environment
  assign("metashark.args", args, envir = .GlobalEnv)
  on.exit(rm("metashark.args", envir = .GlobalEnv))
  
  # Settings =====
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
  
  # Set max flow at 2 Go
  options(shiny.maxRequestSize = 2*1024^3)
  
  # Set window values
  if(isFALSE(args$use.test))
    options(shiny.port = 3838)
  
  # Launch ====
  if(isTRUE(args$use.profvis)) {
    profvis::profvis({
      runApp(
        shinyApp(ui = ui, server = server), 
        launch.browser = args$launch.browser
      )
    })
  } else if(isTRUE(args$use.test)) {
    options("shiny.testmode" = TRUE)
    shinyApp(ui = ui, server = server)
  } else {
    runApp(
      shinyApp(ui = ui, server = server), 
      launch.browser = args$launch.browser
    )
  }
}
