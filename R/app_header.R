#' Preload & set variables
#' 
#' This function is used to prepare variables at the application startup.
#' It takes up to a dozen of seconds.
#' 
#' CAPITAL written variables are constant variables.
#' common written variables are regular variables.
#' 
#' @import shiny
#' @importFrom EML get_unitList
#' @importFrom data.table fread
#'
#' @noRd
.globalScript <- function(
  .args = list(dev = FALSE, wip = FALSE, reactlog = TRUE), 
  envir,
  ...
) {
  # handle additional arguments in ...
  other.items <- c(...)
  
  # Options setup ====
  options(stringsAsFactors = FALSE)
  options(shiny.reactlog = .args$reactlog)
  
  # Environment setup ====
  main.env <- new.env(parent = envir)
  # assign specific values for quick access
  assign("dev", .args$dev, main.env)
  assign("wip", .args$wip, main.env)
  # assign other values in 'metashark.args'
  other.args <- .args[which(names(.args) != c("wip", "dev"))]
  assign("other.args", other.args, main.env)
  # assign ... in main.env
  assign("other.items", other.items, main.env)
  
  # Paths ====
  # Paths contained in inst/resources
  resourcePaths <- system.file("resources", package = "MetaShARK") |>
    dir(full.names = TRUE) |>
    as.list()
  names(resourcePaths) <- basename(unlist(resourcePaths))
  
  # Set Home -- different if testmode is on
  HOME <- if(isTRUE(getOption("shiny.testmode")))
    system.file("../tests/testthat/app/tests/shinytest/test_data_package/", package = "MetaShARK") else
      "~/dataPackagesOutput/emlAssemblyLine"
  # Index interesting paths
  PATHS <- reactiveValues(
    # home directory
    eal.dp = paste0(HOME, "/"),
    # path to index file
    # FIXME later : still unused, shall handle users sessions & profiles
    eal.dp.index =sprintf(
      "%s/index.txt",
      HOME
    ),
    # defined temp dir
    eal.tmp = tempdir(),
    # paths to resources
    resources = resourcePaths
  )
  
  # Test-specific path setup
  if(!isTRUE(getOption("shiny.testmode")))
    dir.create(isolate(PATHS$eal.dp), recursive = TRUE, showWarnings = FALSE)
  # dir.create(isolate(PATHS$eal.tmp), recursive = TRUE, showWarnings = FALSE)

  # save PATHS
  assign("PATHS", PATHS, envir = main.env)

  # Sessionning ====

  # In test, set DP list to empty
  if(isTRUE(getOption("shiny.testmode"))) {
    devmsg(tag = "test", "setting DP list")
    # FIXME : add dp index
    DP.LIST <- data.frame(
      creator = character(),
      name = character(),
      title = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
  } else if (isTRUE(file.exists(isolate(PATHS$eal.dp.index)))) { # if existing index: dp list
    DP.LIST <- readDataTable(isolate(PATHS$eal.dp.index), sep = "\t")
    DP.LIST$path <- DP.LIST$path |>
      gsub(pattern = "//+", replacement = "/")
  } else { # if no index: empty list
    DP.LIST <- data.frame(
      creator = character(),
      name = character(),
      title = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
    # Fill DP.LIST for first-time runs
    # Get list of DPs
    .files <- dir(isolate(PATHS$eal.dp), full.names = TRUE) |>
      gsub(pattern = "//", replacement = "/")
    # Set information per DP entry by reading json summary file
    if(length(.files) > 0) {
      sapply(.files, function(.file) {
        .info <- jsonlite::read_json(
          sprintf(
            "%s/%s.json",
            .file,
            basename(.file) |>
              gsub(pattern = "_emldp$", replacement = "")
          )
        )[[1]] |>
          jsonlite::unserializeJSON()
        # set row for the file
        .row <- c(
          creator.orcid = "public", # default to public access
          name = .info$SelectDP$dp.name,
          title = .info$SelectDP$dp.title,
          path = .file
        )
        # Add to dp list
        DP.LIST <<- rbind(DP.LIST, .row)
      })
    }
  }
  # check DP.LIST columns are well named
  colnames(DP.LIST) <- c("creator", "name", "title", "path")
  
  # Curate DP.LIST versus actual list
  .actual.index <- dir(
    isolate(main.env$PATHS$eal.dp),
    pattern = "_emldp$",
    full.names = TRUE
  ) |> 
    gsub(pattern = "//+", replacement = "/")
  # Only keep really existing DP 
  DP.LIST <- dplyr::filter(DP.LIST, path %in% .actual.index)
  
  # save actual index
  data.table::fwrite(DP.LIST, isolate(PATHS$eal.dp.index), sep = "\t")
  # save DP.LIST
  assign("DP.LIST", DP.LIST, envir = main.env)
  # Make it reactive 
  makeReactiveBinding("DP.LIST", env = main.env)

  # Values ====
  # Multiple purposes data
  
  # DataONE nodes
  .ENDPOINTS <- readDataTable(resourcePaths$registeredEndpoints.txt)
  
  # Save 
  assign(
    "VALUES",
    reactiveValues(
      dataone.endpoints = .ENDPOINTS,
      thresholds = reactiveValues(
        files.size.max = 500000 
      ),
      steps = c(
        "SelectDP",
        "Data_Files",
        "Attributes",
        "Categorical_Variables",
        "Geographic_Coverage",
        "Taxonomic_Coverage",
        "Personnel",
        "Miscellaneous",
        "Make_EML"
      ),
      last.timer = Sys.time()
    ),
    envir = main.env
  )
  
  # Formats ====

  # Taxa authorities
  .TAXA.AUTHORITIES <- data.table::fread(resourcePaths$taxaAuthorities.txt)

  # Unit types
  .all.units <- EML::get_unitList()$units
  .ind <- seq_along(.all.units$name)[-anyDuplicated(.all.units$name)]
  .units <- .all.units$name[.ind]
  .unitTypes <- .all.units$unitType[.ind] 
  .unitTypes <- replace(
    .unitTypes,
    which(.unitTypes %in% c("", "NA", NA)), 
    "unsorted"
  )
  .unitList <- sort(paste(.unitTypes, .units, sep = "/"))
  
  .units <- "custom"
  names(.units) <- "custom"
  
  # Structure unit list
  sapply(.unitList, function(unit) {
    .units <<- c(
      .units, 
      setNames(
        gsub("^.*/(.*)$", "\\1", unit),
        gsub("^(.*)/.*$", "\\1", unit)
      )
    )
  })
  .units <- split(.units, names(.units)) |>
    sapply(unname)
  # Turn 1-length items' names to items themselves
  sapply(
    which(sapply(.units, length) == 1),
    function(li) 
      names(.units)[li] <<- .units[[li]]
  )
  # Set custom as first unit
  .units <- c(.units["custom"], .units[names(.units) != "custom"])
  
  # Save
  assign(
    "FORMATS",
    reactiveValues(
      dates = c(
        "YYYY-MM-DD", "YYYY",
        "YYYY-MM",  "YYYY-DD-MM",
        "MM-YYYY", "DD-MM-YYYY", "MM-DD-YYYY",
        "hh:mm:ss", "hh:mm", "mm:ss", "hh"
      ),
      lubridate_formats = lubridate:::lubridate_formats,
      units = .units,
      taxa.authorities = .TAXA.AUTHORITIES
    ),
    envir = main.env
  )

  # Semantics ====
  # FIXME load ontologies
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
      side.tab = c()
      # , metacat.test = FALSE
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
      help = character(),
      ping = "" # change character to ping EAL and trigger an observer
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
