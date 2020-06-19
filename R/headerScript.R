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
.globalScript <- function(dev = FALSE) {
  
  
  # Environment setup ====
  main.env <- new.env()
  
  if (!is.logical(dev) || is.null(dev)) 
    dev <- FALSE
  else
    dev <- TRUE
  
  assign("DEV", dev, main.env)
  
  # Paths====
  wwwPaths <- system.file("resources", package = "MetaShARK") %>%
    paste(., dir(.), sep = "/") %>%
    as.list()
  names(wwwPaths) <- basename(unlist(wwwPaths))
  PATHS <- reactiveValues(
    home = path_home(),
    eal.dp = paste0(path_home(), "/dataPackagesOutput/emlAssemblyLine/"),
    eal.dp.index = paste0(path_home(), "/dataPackagesOutput/emlAssemblyLine/index.json"),
    eal.tmp = paste0(path_home(), "/EMLAL_tmp/"),
    resources = wwwPaths
  )
  dir.create(isolate(PATHS$eal.dp), recursive = TRUE, showWarnings = FALSE)
  unlink(isolate(PATHS$eal.tmp), recursive = TRUE) # clear the temp
  dir.create(isolate(PATHS$eal.tmp), recursive = TRUE, showWarnings = FALSE)

  # Sessionning
  if (isTRUE(file.exists(isolate(PATHS$eal.dp.index)))) {
    DP.LIST <- read_json(isolate(PATHS$eal.dp.index), simplifyVector = TRUE) %>%
      unserializeJSON
  }
  else {
    DP.LIST <- list(
      public = list( # user.name = "public"
        creator.orcid = "public",
        data.packages = data.frame(
          path = character(),
          name = character()
        )
      )
    )
    serializeJSON(
      DP.LIST
    ) %>% write_json(
      path = isolate(PATHS$eal.dp.index)
    )
  }
  
  assign("PATHS", PATHS, envir = main.env)

  # Values ====
  VALUES <- reactiveValues(
    thresholds = reactiveValues(
      files_size_max = 500000
    )
  )
  
  assign("VALUES", VALUES, envir = main.env)
  
  # Formats ====
  # DataONE nodes
  .DATAONE.LIST <- try(listFormats(CNode()))
  if (class(.DATAONE.LIST) == "try-error") {
    .DATAONE.LIST <- fread(wwwPaths$dataoneCNodesList.txt)
  } else {
    fwrite(.DATAONE.LIST, wwwPaths$dataoneCNodesList.txt)
  }
  
  # Taxa authorities
  .TAXA.AUTHORITIES <- try(view_taxa_authorities())
  if (class(.TAXA.AUTHORITIES) == "try-error") {
    .TAXA.AUTHORITIES <- fread(wwwPaths$taxaAuthorities.txt)
  } else {
    fwrite(.TAXA.AUTHORITIES, wwwPaths$taxaAuthorities.txt)
  }
  
  # Unit types
  .all.units <- get_unitList()
  .units <- "custom"
  .names <- "custom"
  invisible(apply(.all.units$units[c("unitType", "name")], 1, function(row) {
    .units <<- c(.units, row["name"])
    .names <<- c(.names, paste(row, collapse = "/"))
  }))
  .all.units <- .units
  names(.all.units) <- .names
  
  FORMATS <- reactiveValues(
    dates = c(
      "YYYY",
      "YYYY-MM", "YYYY-MM-DD", "YYYY-DD-MM",
      "MM-YYYY", "DD-MM-YYYY", "MM-DD-YYYY"
    ),
    units = .all.units,
    dataone.list = .DATAONE.LIST,
    taxa.authorities = .TAXA.AUTHORITIES
  )
  
  assign("FORMATS", FORMATS, envir = main.env)

  # Semantics ====

  SEMANTICS <- reactiveValues(
    ontologies = character()
  )
  
  assign("SEMANTICS", SEMANTICS, envir = main.env)
  
  # Settings ====
  
  SETTINGS <- reactiveValues(
    logged = FALSE,
    orcid.token = character(),
    cedar.token = character(),
    metacat.token = character(),
    metacat.test = FALSE
  )
  
  assign("SETTINGS", SETTINGS, envir = main.env)
  
  # EAL ====
  
  EAL = reactiveValues(
    history = "SelectDP",
    navigate = 1,
    current = c(name = "SelectDP", completed = FALSE),
    iterator = 0
    #, COMPLETE_CURRENT = FALSE
  )
  
  assign("EAL", EAL, envir = main.env)
  
  # Patterns ====
  
  PATTERNS = reactiveValues(
    # match one expression for latitude or longitude
    coordinates = "[+-]?[[:digit:]]+[.,]*[[:digit:]]*",
    name = "^[[:alpha:] \\'\\.\\-]+$",
    email = "^[^@]+@[^@]+\\.[[:alpha:]]",
    ORCID = "\\d{4}-\\d{4}-\\d{4}-(\\d{4}|\\d{3}X)"
  )
  
  # Build global variable ====
  # if (reactive) {
  #   globals <- reactiveValues(
  #     dev = dev,
  #     THRESHOLDS = reactiveValues(data_files_size_max = 500000),
  #     DEFAULT.PATH = DP.PATH,
  #     DP.LIST = DP.LIST,
  #     SESSION = reactiveValues(
  #       LOGGED = FALSE,
  #       ORCID.TOKEN = character()
  #     ),
  #     TEMP.PATH = TMP.PATH,
  #     HOME = HOME,
  #     PATHS = wwwPaths,
  #     # Formats lists
  #     FORMAT = list(
  #       DATE = DATE.FORMAT,
  #       UNIT = UNIT.LIST,
  #       DATAONE = DATAONE.LIST,
  #       AUTHORITIES = TAXA.AUTHORITIES
  #     ),
  #     # Regex patterns
  #     PATTERNS = list(
  #       # match one expression for latitude or longitude
  #       LATLON = "[+-]?[[:digit:]]+[.,]*[[:digit:]]*",
  #       NAME = "^[[:alpha:] \\'\\.\\-]+$",
  #       EMAIL = "^[^@]+@[^@]+\\.[[:alpha:]]",
  #       ORCID = "\\d{4}-\\d{4}-\\d{4}-(\\d{4}|\\d{3}X)"
  #     ),
  #     # navigation variable in EMLAL module
      # EMLAL = reactiveValues(
      #   HISTORY = "SelectDP",
      #   NAVIGATE = 1,
      #   CURRENT = "SelectDP",
      #   ITERATOR = 0,
      #   COMPLETE_CURRENT = FALSE
      # ),
  #     SETTINGS = reactiveValues(
  #       TOKENS = reactiveValues()
  #     ),
  #     SEMANTICS = reactiveValues(
  #       ONTOLOGIES = character()
  #     )
  #   )
  # } else
  #   {
  #   globals <- list(
  #     dev = dev,
  #     THRESHOLDS = list(data_files_size_max = 500000),
  #     DEFAULT.PATH = DP.PATH,
  #     DP.LIST = DP.LIST,
  #     SESSION = list(
  #       LOGGED = FALSE,
  #       ORCID.TOKEN = character()
  #     ),
  #     TEMP.PATH = TMP.PATH,
  #     HOME = HOME,
  #     PATHS = wwwPaths,
  #     # Formats lists
  #     FORMAT = list(
  #       DATE = DATE.FORMAT,
  #       UNIT = UNIT.LIST,
  #       DATAONE = DATAONE.LIST,
  #       AUTHORITIES = TAXA.AUTHORITIES
  #     ),
  #     # Regex patterns
  #     PATTERNS = list(
  #       # match one expression for latitude or longitude
  #       LATLON = "[+-]?[[:digit:]]+[.,]*[[:digit:]]*",
  #       NAME = "^[[:alpha:] \\'\\.\\-]+$",
  #       EMAIL = "^[^@]+@[^@]+\\.[[:alpha:]]",
  #       ORCID = "\\d{4}-\\d{4}-\\d{4}-(\\d{4}|\\d{3}X)"
  #     ),
  #     # navigation variable in EMLAL module
  #     EMLAL = list(
  #       HISTORY = "SelectDP",
  #       NAVIGATE = 1,
  #       CURRENT = "SelectDP",
  #       ITERATOR = 0,
  #       COMPLETE_CURRENT = FALSE
  #     ),
  #     SETTINGS = list(
  #       TOKENS = list()
  #     ),
  #     SEMANTICS = list(
  #       ONTOLOGIES = character()
  #     )
  #   )
  # }
  
  # output ====
  
  return(main.env)
}
