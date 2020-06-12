#' @title .headerScript
#'
#' @description Misc preparations commands
.headerScript <- function() {
  rm(list = ls())
  options(shiny.reactlog = TRUE)
  addResourcePath("media", system.file("media/", package = "MetaShARK"))
}

#' @title .globalScript
#'
#' @description script to generate `globals` reactiveValues
#'
#' @importFrom fs path_home
#' @importFrom dataone listFormats CNode
#' @importFrom shiny reactiveValues
#' @importFrom EML get_unitList
#' @importFrom data.table fread
#' @importFrom taxonomyCleanr view_taxa_authorities
#' @importFrom dplyr %>%
#' @importFrom jsonlite serializeJSON unserializeJSON read_json write_json
.globalScript <- function(dev = FALSE, server = FALSE, reactive = TRUE) {
  if (!is.logical(dev) || is.null(dev)) dev <- FALSE

  # Paths====
  HOME <- path_home()
  DP.PATH <- paste0(HOME, "/dataPackagesOutput/emlAssemblyLine/")
  dir.create(DP.PATH, recursive = TRUE, showWarnings = FALSE)
  TMP.PATH <- paste0(HOME, "/EMLAL_tmp/")
  unlink(TMP.PATH, recursive = TRUE) # clear the temp
  dir.create(TMP.PATH, recursive = TRUE, showWarnings = FALSE)

  # Sessionning
  DP.LIST.PATH <- paste0(DP.PATH, "index.json")
  if (isTRUE(file.exists(DP.LIST.PATH))) {
    DP.LIST <- read_json(DP.LIST.PATH, simplifyVector = TRUE) %>% unserializeJSON()
  }
  else {
    DP.LIST <- list(
      public = list( # user.name = "public"
        creator.orcid = "",
        data.packages = data.frame(
          path = "",
          name = ""
        )
      )
    )
    serializeJSON(
      DP.LIST
    ) %>% write_json(
      path = DP.LIST.PATH
    )
  }
  wwwPaths <- system.file("resources", package = "MetaShARK") %>%
    paste(., dir(.), sep = "/") %>%
    as.list()
  names(wwwPaths) <- basename(unlist(wwwPaths))

  # Values
  THRESHOLD <- list(
    dp_data_files = 500000
  )

  # Date time format strings ====
  # TODO better !
  DATE.FORMAT <- c(
    "YYYY",
    "YYYY-MM", "YYYY-MM-DD", "YYYY-DD-MM",
    "MM-YYYY", "DD-MM-YYYY", "MM-DD-YYYY"
  )

  # Unit types ====
  ALL.UNITS <- get_unitList()
  .units <- "custom"
  .names <- "custom"
  invisible(apply(ALL.UNITS$units[c("unitType", "name")], 1, function(row) {
    .units <<- c(.units, row["name"])
    .names <<- c(.names, paste(row, collapse = "/"))
  }))
  UNIT.LIST <- .units
  names(UNIT.LIST) <- .names

  # DataONE nodes
  DATAONE.LIST <- try(listFormats(CNode()))
  if (class(DATAONE.LIST) == "try-error") {
    DATAONE.LIST <- fread(wwwPaths$dataoneCNodesList.txt)
  } else {
    fwrite(DATAONE.LIST, wwwPaths$dataoneCNodesList.txt)
  }

  # Taxa authorities
  TAXA.AUTHORITIES <- try(view_taxa_authorities())
  if (class(TAXA.AUTHORITIES) == "try-error") {
    TAXA.AUTHORITIES <- fread(wwwPaths$taxaAuthorities.txt)
  } else {
    fwrite(TAXA.AUTHORITIES, wwwPaths$taxaAuthorities.txt)
  }

  # Semantics ====


  # Build global variable ====
  if (reactive) {
    globals <- reactiveValues(
      dev = dev,
      THRESHOLDS = reactiveValues(data_files_size_max = 500000),
      DEFAULT.PATH = DP.PATH,
      DP.LIST = DP.LIST,
      SESSION = reactiveValues(
        LOGGED = FALSE,
        ORCID.TOKEN = character()
      ),
      TEMP.PATH = TMP.PATH,
      HOME = HOME,
      PATHS = wwwPaths,
      # Formats lists
      FORMAT = list(
        DATE = DATE.FORMAT,
        UNIT = UNIT.LIST,
        DATAONE = DATAONE.LIST,
        AUTHORITIES = TAXA.AUTHORITIES
      ),
      # Regex patterns
      PATTERNS = list(
        # match one expression for latitude or longitude
        LATLON = "[+-]?[[:digit:]]+[.,]*[[:digit:]]*",
        NAME = "^[[:alpha:] \\'\\.\\-]+$",
        EMAIL = "^[^@]+@[^@]+\\.[[:alpha:]]",
        ORCID = "\\d{4}-\\d{4}-\\d{4}-(\\d{4}|\\d{3}X)"
      ),
      # navigation variable in EMLAL module
      EMLAL = reactiveValues(
        HISTORY = "SelectDP",
        NAVIGATE = 1,
        CURRENT = "SelectDP",
        ITERATOR = 0,
        COMPLETE_CURRENT = FALSE
      ),
      SETTINGS = reactiveValues(
        TOKENS = reactiveValues()
      ),
      SEMANTICS = reactiveValues(
        ONTOLOGIES = character()
      )
    )
  } else {
    globals <- list(
      dev = dev,
      THRESHOLDS = list(data_files_size_max = 500000),
      DEFAULT.PATH = DP.PATH,
      DP.LIST = DP.LIST,
      SESSION = list(
        LOGGED = FALSE,
        ORCID.TOKEN = character()
      ),
      TEMP.PATH = TMP.PATH,
      HOME = HOME,
      PATHS = wwwPaths,
      # Formats lists
      FORMAT = list(
        DATE = DATE.FORMAT,
        UNIT = UNIT.LIST,
        DATAONE = DATAONE.LIST,
        AUTHORITIES = TAXA.AUTHORITIES
      ),
      # Regex patterns
      PATTERNS = list(
        # match one expression for latitude or longitude
        LATLON = "[+-]?[[:digit:]]+[.,]*[[:digit:]]*",
        NAME = "^[[:alpha:] \\'\\.\\-]+$",
        EMAIL = "^[^@]+@[^@]+\\.[[:alpha:]]",
        ORCID = "\\d{4}-\\d{4}-\\d{4}-(\\d{4}|\\d{3}X)"
      ),
      # navigation variable in EMLAL module
      EMLAL = list(
        HISTORY = "SelectDP",
        NAVIGATE = 1,
        CURRENT = "SelectDP",
        ITERATOR = 0,
        COMPLETE_CURRENT = FALSE
      ),
      SETTINGS = list(
        TOKENS = list()
      ),
      SEMANTICS = list(
        ONTOLOGIES = character()
      )
    )
  }

  # output
  return(globals)
}
