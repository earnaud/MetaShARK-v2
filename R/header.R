#' @importFrom fs path_home
#' @importFrom dataone listFormats CNode
#' @import shiny
#' @importFrom EML get_unitList
#' @importFrom data.table fread
#' @importFrom taxonomyCleanr view_taxa_authorities
#' @importFrom dplyr %>%
#' @importFrom jsonlite serializeJSON unserializeJSON read_json write_json
#'
#' @noRd
.globalScript <- function(
  args = list(
    dev = FALSE, 
    wip = FALSE
  )
) {
  
  # Environment setup ====
  main.env <- new.env()
  
  assign(
    "dev",
    isTRUE(args$dev),
    main.env
  )
  assign(
    "wip",
    isTRUE(args$wip),
    main.env
  )
  assign(
    "reactlog",
    isTRUE(args$reactlog),
    main.env
  )
  
  
  # Paths====
  wwwPaths <- system.file("resources", package = "MetaShARK") %>%
    paste(., dir(.), sep = "/") %>%
    as.list
  names(wwwPaths) <- basename(unlist(wwwPaths))
  PATHS <- reactiveValues(
    home = "~",
    eal.dp = paste0("~/dataPackagesOutput/emlAssemblyLine/"),
    eal.dp.index = paste0("~/dataPackagesOutput/emlAssemblyLine/index.txt"),
    eal.tmp = paste0("~/EMLAL_tmp/"),
    resources = wwwPaths
  )
  dir.create(isolate(PATHS$eal.dp), recursive = TRUE, showWarnings = FALSE)
  dir.create(isolate(PATHS$eal.tmp), recursive = TRUE, showWarnings = FALSE)
  
  assign(
    "PATHS", 
    PATHS, 
    envir = main.env
  )
  
  # Sessionning ====
  if (isTRUE(file.exists(isolate(PATHS$eal.dp.index)))) {
    DP.LIST <- data.table::fread(isolate(PATHS$eal.dp.index), sep = "\t")
  }
  else {
    DP.LIST <- data.frame(
      creator.orcid = character(),
      name = character(),
      title = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
    data.table::fwrite(DP.LIST, isolate(PATHS$eal.dp.index), sep = "\t")
  }
  assign(
    "DP.LIST",
    DP.LIST,
    envir = main.env
  )
  makeReactiveBinding(
    "DP.LIST", 
    env = main.env
  )
  
  # Values ====
  assign(
    "VALUES", 
    reactiveValues(
      thresholds = reactiveValues(
        files.size.max = 500000
      ),
      steps = c("SelectDP", "Data Files", "Attributes", "Categorical Variables",
        "Geographic Coverage", "Taxonomic Coverage", "Personnel", "Miscellaneous",
        "Make EML")
    ), 
    envir = main.env
  )
  
  # Formats ====
  # DataONE nodes
  .DATAONE.LIST <- try(listFormats(dataone::CNode()))
  if (class(.DATAONE.LIST) == "try-error") {
    .DATAONE.LIST <- data.table::fread(wwwPaths$dataoneCNodesList.txt)
  } else {
    data.table::fwrite(.DATAONE.LIST, wwwPaths$dataoneCNodesList.txt)
  }
  
  # Taxa authorities
  .TAXA.AUTHORITIES <- try(taxonomyCleanr::view_taxa_authorities())
  if (class(.TAXA.AUTHORITIES) == "try-error") {
    .TAXA.AUTHORITIES <- data.table::fread(wwwPaths$taxaAuthorities.txt)
  } else {
    data.table::fwrite(.TAXA.AUTHORITIES, wwwPaths$taxaAuthorities.txt)
  }
  
  # Unit types
  .all.units <- EML::get_unitList()
  .units <- "custom"
  .names <- "custom"
  invisible(apply(.all.units$units[c("unitType", "name")], 1, function(row) {
    .units <<- c(.units, row["name"])
    .names <<- c(.names, paste(row, collapse = "/"))
  }))
  .all.units <- .units
  names(.all.units) <- .names
  
  assign(
    "FORMATS", 
    reactiveValues(
      dates = c(
        "YYYY",
        "YYYY-MM", "YYYY-MM-DD", "YYYY-DD-MM",
        "MM-YYYY", "DD-MM-YYYY", "MM-DD-YYYY"
      ),
      units = .all.units,
      dataone.list = .DATAONE.LIST,
      taxa.authorities = .TAXA.AUTHORITIES
    ),
    envir = main.env
  )

  # Semantics ====

  assign(
    "SEMANTICS",
    reactiveValues(
      ontologies = character()
    ),
    envir = main.env
  )
  
  # Settings ====
  
  assign(
    "SETTINGS", 
    reactiveValues(
      logged = FALSE,
      user = "public",
      orcid.token = character(),
      cedar.token = character(),
      metacat.token = character(),
      metacat.test = FALSE
    ), 
    envir = main.env
  )
  
  # EAL rv ====
  assign(
    "EAL", 
    reactiveValues(
      page = 1, # page number
      history = character(), # all browsed pages names in steps
      current = character(), # last of history
      completed = FALSE,  # is current page completed?
      tag.list = tagList(), # side HTML tags to display
      help = character(),
      .next = 0,
      .prev = 0
    ), 
    envir = main.env
  )
  
  assign(
    "save.variable",
    initReactive(main.env = main.env),
    envir = main.env
  )
  
  # Patterns ====
  assign(
    "PATTERNS",
    reactiveValues(
      # match one expression for latitude or longitude
      coordinates = "[+-]?[[:digit:]]+[.,]*[[:digit:]]*",
      name = "^[[:alpha:] \\'\\.\\-]+$",
      email = "^[^@]+@[^@]+\\.[[:alpha:]]",
      ORCID = "\\d{4}-\\d{4}-\\d{4}-(\\d{4}|\\d{3}X)"
    ),
    envir = main.env
  )
  
  # output ====
  shinyOptions(main.env = main.env)
  shinyOptions(shiny.reactlog = main.env$reactlog)
  addResourcePath("media", system.file("media/", package = "MetaShARK"))
  # return(main.env)
  return(NULL)
}
