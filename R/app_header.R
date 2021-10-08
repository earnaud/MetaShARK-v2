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

  other.items <- c(...)
  if("ui.steps" %in% names(other.items)) {
    ui.steps <- other.items$ui.steps
    other.items <- other.items[-which(names(other.items) == "ui.steps")]
  }
  
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
  wwwPaths <- system.file("resources", package = "MetaShARK") |>
    dir(full.names = TRUE) |>
    as.list()
  names(wwwPaths) <- basename(unlist(wwwPaths))
  
  HOME <- if(isTRUE(getOption("shiny.testmode")))
    system.file("../tests/testthat/app/tests/shinytest/test_data_package/", package = "MetaShARK") else
      "~/dataPackagesOutput/emlAssemblyLine"
  PATHS <- reactiveValues(
    home = HOME,
    eal.dp = sprintf(
      "%s/",
      HOME
    ),
    eal.dp.index =sprintf(
      "%s/index.txt",
      HOME
    ),
    eal.tmp = tempdir(),
    resources = wwwPaths
  )
  if(!isTRUE(getOption("shiny.testmode")))
    dir.create(isolate(PATHS$eal.dp), recursive = TRUE, showWarnings = FALSE)
  dir.create(isolate(PATHS$eal.tmp), recursive = TRUE, showWarnings = FALSE)

  assign("PATHS", PATHS, envir = main.env)

  # Sessionning ====
  
  if(isTRUE(getOption("shiny.testmode"))) {
    devmsg(tag = "test", "setting DP list")
    DP.LIST <- data.frame(
      creator = character(),
      name = character(),
      title = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
  } else if (isTRUE(file.exists(isolate(PATHS$eal.dp.index)))) {
    DP.LIST <- readDataTable(isolate(PATHS$eal.dp.index), sep = "\t")
    DP.LIST$path <- DP.LIST$path |>
      gsub(pattern = "//+", replacement = "/")
  } else {
    DP.LIST <- data.frame(
      creator = character(),
      name = character(),
      title = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
    # Fill DP.LIST for first-time runs
    .files <- dir(isolate(PATHS$eal.dp), full.names = TRUE) |>
      gsub(pattern = "//", replacement = "/")
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
  ) |> 
    gsub(pattern = "//+", replacement = "/")
  DP.LIST <- dplyr::filter(DP.LIST, path %in% .actual.index)
  
  # save actual index
  data.table::fwrite(DP.LIST, isolate(PATHS$eal.dp.index), sep = "\t")

  assign("DP.LIST", DP.LIST, envir = main.env)
  makeReactiveBinding("DP.LIST", env = main.env)

  # Values ====
  # DataONE nodes
  .ENDPOINTS <- readDataTable(
    wwwPaths$registeredEndpoints.txt
  )
  
  assign(
    "VALUES",
    reactiveValues(
      dataone.endpoints = .ENDPOINTS,
      thresholds = reactiveValues(
        files.size.max = 500000
      ),
      steps = ui.steps
    ),
    envir = main.env
  )
  # Formats ====

  # Taxa authorities
  .TAXA.AUTHORITIES <- data.table::fread(wwwPaths$taxaAuthorities.txt)

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
  
  sapply(.unitList, function(unit) {
    .units <<- c(
      .units, 
      setNames(
        gsub("^.*/(.*)$", "\\1", unit),
        gsub("^(.*)/.*$", "\\1", unit)
      )
    )
  })

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
