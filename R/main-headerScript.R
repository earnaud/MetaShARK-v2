#' @title .headerScript
#'
#' @description misc preparations commands
.headerScript <- function() {
  rm(list = ls())
  options(shiny.reactlog = TRUE)
}

#' @title .globalScript
#'
#' @description script to generate 'globals' reactiveValues
#'
#' @importFrom fs path_home
#' @importFrom utils combn
#' @importFrom dataone listFormats CNode
#' @importFrom shiny reactiveValues
#' @importFrom EML get_unitList
.globalScript <- function(dev = FALSE) {
  if (!is.logical(dev) || is.null(dev)) dev <- FALSE

  HOME <- path_home()
  DP.PATH <- paste0(HOME, "/dataPackagesOutput/emlAssemblyLine/")
  dir.create(DP.PATH, recursive = TRUE, showWarnings = FALSE)

  THRESHOLD <- list(
    dp_data_files = 500000
  )

  # Date time format strings
  DATE.FORMAT <- combn(rep(c("YYYY", "MM", "DD"), 3), 3)
  DATE.FORMAT <- unique(as.list(as.data.frame(DATE.FORMAT[, !apply(DATE.FORMAT, 2, function(y) any(duplicated(y)))], stringsAsFactors = FALSE)))
  DATE.FORMAT <- sapply(c("-", "/", ":"), function(sep) {
    sapply(DATE.FORMAT, paste, collapse = sep)
  })
  HOUR.FORMAT <- c(NA, gsub("YYYY", "hh", gsub("MM", "mm", gsub("DD", "ss", DATE.FORMAT))))
  DATE.FORMAT <- as.vector(rbind(DATE.FORMAT, gsub("Y{4}", "YY", DATE.FORMAT)))
  DATE.FORMAT <- DATE.FORMAT[order(DATE.FORMAT, decreasing = TRUE)]
  UNIT.LIST <- c("custom", get_unitList()$units$name)
  DATAONE.LIST <- dataone::listFormats(dataone::CNode())$MediaType

  # Build global variable
  globals <- reactiveValues(
    dev = dev,
    THRESHOLDS = reactiveValues(data_files_size_max = 500000),
    DEFAULT.PATH = DP.PATH,
    HOME = HOME,
    NS.INDEX = readRDS(system.file("resources/nsIndex.RData", package = "MetaShARK")),
    # Formats lists
    FORMAT = list(
      DATE = DATE.FORMAT,
      HOUR = HOUR.FORMAT,
      UNIT = UNIT.LIST,
      DATAONE = DATAONE.LIST
    ),
    # Regex patterns
    PATTERNS = list(
      # match one expression for latitude or longitude
      LATLON = "[+-]?[[:digit:]]+[.,]*[[:digit:]]*"
    ),
    # navigation variable in EMLAL module
    EMLAL = reactiveValues(
      HISTORY = character(),
      NAVIGATE = 1,
      MAX = 6
    )
  )

  # output
  return(globals)
}
