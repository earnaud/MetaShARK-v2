#' @import shiny
#' @importFrom EML get_unitList
#' @importFrom data.table fread
#' @importFrom dplyr %>%
#'
#' @noRd
.globalScript <- function(.args = list(dev = FALSE, wip = FALSE), envir) {

  # Options setup ====
  options(stringsAsFactors = FALSE)
  options(shiny.reactlog = .args$reactlog)
  
  # Environment setup ====
  main.env <- new.env(parent = envir)
  
  assign("dev", .args$dev, main.env)
  assign("wip", .args$wip, main.env)

  # Paths ====
  wwwPaths <- system.file("resources", package = "MetaShARK") %>%
    paste(., dir(.), sep = "/") %>%
    as.list()
  names(wwwPaths) <- basename(unlist(wwwPaths))
  PATHS <- reactiveValues(
    home = "~",
    eal.dp = paste0("~/dataPackagesOutput/emlAssemblyLine/"),
    eal.dp.index = paste0("~/dataPackagesOutput/emlAssemblyLine/index.txt"),
    eal.tmp = tempdir(),
    resources = wwwPaths
  )
  dir.create(isolate(PATHS$eal.dp), recursive = TRUE, showWarnings = FALSE)
  dir.create(isolate(PATHS$eal.tmp), recursive = TRUE, showWarnings = FALSE)

  assign("PATHS", PATHS, envir = main.env)

  # Sessionning ====
  
  if (isTRUE(file.exists(isolate(PATHS$eal.dp.index)))) {
    DP.LIST <- data.table::fread(isolate(PATHS$eal.dp.index), sep = "\t")
    DP.LIST$path <- DP.LIST$path %>%
      gsub("//+", "/", .)
  } else {
    DP.LIST <- data.frame(
      creator.orcid = character(),
      name = character(),
      title = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
    # Fill DP.LIST for first-time runs
    .files <- dir(isolate(PATHS$eal.dp), full.names = TRUE) %>%
      gsub(pattern = "//", replacement = "/")
    if(length(.files) > 0) {
      sapply(.files, function(.file) {
        .info <- jsonlite::read_json(
          sprintf(
            "%s/%s.json", 
            .file, 
            basename(.file) %>%
              gsub(pattern = "_emldp$", replacement = "")
          )
        )[[1]] %>%
          jsonlite::unserializeJSON()
        
        .row <- c(
          creator.orcid = "public",
          name = .info$SelectDP$dp.name,
          title = .info$SelectDP$dp.title,
          path = .file
        )
        DP.LIST <<- rbind(DP.LIST, .row)
      })
    }
  }
  colnames(DP.LIST) <- c("creator", "name", "title", "path")
  
  # Curate DP.LIST versus actual list
  .actual.index <- dir(
    isolate(main.env$PATHS$eal.dp),
    pattern = "_emldp$",
    full.names = TRUE
  ) %>% gsub("//+", "/", .)
  DP.LIST <- dplyr::filter(DP.LIST, path %in% .actual.index)

  data.table::fwrite(DP.LIST, isolate(PATHS$eal.dp.index), sep = "\t")

  assign("DP.LIST", DP.LIST, envir = main.env)
  makeReactiveBinding("DP.LIST", env = main.env)

  # Values ====
  assign(
    "VALUES",
    reactiveValues(
      thresholds = reactiveValues(
        files.size.max = 500000
      ),
      steps = ui.steps
    ),
    envir = main.env
  )
  # Formats ====
  # DataONE nodes
  .DATAONE.LIST <- data.table::fread(wwwPaths$dataoneCNodesList.txt)

  # Taxa authorities
  .TAXA.AUTHORITIES <- data.table::fread(wwwPaths$taxaAuthorities.txt)

  # Unit types
  .all.units <- EML::get_unitList()
  .some.units <- .all.units$units[-which(!is.na(.all.units$units$deprecatedInFavorOf)),]
  if(anyDuplicated(.some.units$id))
    .some.units <- .some.units[-anyDuplicated(.some.units$id),]
  .all.units$units <- .some.units
  .units <- "custom"
  .names <- "custom/custom"
  invisible(apply(.all.units$units[c("unitType", "name")], 1, function(row) {
    .units <<- c(.units, row["name"])
    if(row["unitType"] %in% c("", "NA") || is.na(row["unitType"])) 
      row["unitType"] <- "unsorted"
    .names <<- c(.names, paste(row, collapse = "/"))
  }))
  names(.units) <- .names

  assign(
    "FORMATS",
    reactiveValues(
      dates = c(
        "YYYY-MM-DD", "YYYY",
        "YYYY-MM",  "YYYY-DD-MM",
        "MM-YYYY", "DD-MM-YYYY", "MM-DD-YYYY",
        "hh:mm:ss", "hh:mm", "mm:ss", "hh"
      ),
      units = .units,
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
      old.page = 0,
      page = 1, # page number
      page.load = makeReactiveTrigger(),
      history = isolate(main.env$VALUES$steps[1]), # all browsed pages names in steps
      current = isolate(main.env$VALUES$steps[1]), # last of history
      completed = FALSE, # is current page completed?
      tag.list = tagList(), # side HTML tags to display
      help = character()
    ),
    envir = main.env
  )

  assign(
    "save.variable",
    initReactive(main.env = main.env),
    envir = main.env
  )

  # Local rv ====
  assign(
    "local.rv",
    reactiveValues(),
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
      ORCID = "\\d{4}-\\d{4}-\\d{4}-(\\d{4}|\\d{3}X)",
      dateRegex = reactiveValues(
        DD = "^0?[1-9]$|^[12][0-9]$|^3[01]$",
        MM = "^0?[1-9]$|^1[0-2]$",
        YY = "^[0-9]{2}$",
        YYYY = "^[12][0-9]{3}$"
      )
    ),
    envir = main.env
  )

  # output ====
  shinyOptions(shiny.reactlog = .args$reactlog)

  return(main.env)
}
