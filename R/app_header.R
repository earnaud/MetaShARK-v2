#' Preload & set variables
#'
#' This function is used to prepare variables at the application startup.
#' It takes up to a dozen of seconds.
#'
#' @import shiny
#' @importFrom EML get_unitList
#' @importFrom data.table fread
#'
#' @noRd
.setGlobalVariables <- function(.args = list(dev = FALSE, wip = FALSE, reactlog = TRUE),
                                envir,
                                ...) {
  # handle additional arguments in ...
  other_items <- c(...)

  # Options setup ====
  options(stringsAsFactors = FALSE)
  options(shiny.reactlog = .args$reactlog)

  # Environment setup ====
  main_env <- new.env(parent = envir)
  # assign specific values for quick access
  assign("dev", .args$dev, main_env)
  assign("wip", .args$wip, main_env)
  # assign other values in 'metashark_args'
  other_args <- .args[which(names(.args) != c("wip", "dev"))]
  assign("other_args", other_args, main_env)
  # assign ... in main_env
  assign("other_items", other_items, main_env)

  # Paths ====
  # Paths contained in inst/resources
  resource_paths <- system.file("resources", package = "MetaShARK") |>
    dir(full.names = TRUE) |>
    as.list()
  names(resource_paths) <- basename(unlist(resource_paths))

  # Set Home -- different if testmode is on
  HOME <- if (isTRUE(getOption("shiny.testmode"))) {
    system.file(
      "../tests/testthat/app/tests/shinytest/test_data_package/",
      package = "MetaShARK"
    )
  } else {
    "~/dataPackagesOutput/emlAssemblyLine"
  }
  # Index interesting paths
  PATHS <- reactiveValues(
    # home directory
    eal_dp = paste0(HOME, "/"),
    # path to index file
    # FIXME later : still unused, shall handle users sessions & profiles
    eal_dp_index = sprintf("%s/index.txt", HOME),
    # defined temp dir
    eal_tmp = tempdir(),
    # paths to resources
    resources = resource_paths
  )

  # Test-specific path setup
  if (!isTRUE(getOption("shiny.testmode"))) {
    dir.create(isolate(PATHS$eal_dp), recursive = TRUE, showWarnings = FALSE)
  }

  # save PATHS
  assign("PATHS", PATHS, envir = main_env)

  # Sessionning ====

  # In test, set DP list to empty
  if (isTRUE(getOption("shiny.testmode"))) {
    devmsg(tag = "test", "setting DP list")
    # FIXME : add dp index
    DP_LIST <- data.frame(
      creator = character(),
      name = character(),
      title = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
  } else if (isTRUE(file.exists(isolate(PATHS$eal_dp_index)))) {
    DP_LIST <- readDataTable(isolate(PATHS$eal_dp_index), sep = "\t")
    DP_LIST$path <- DP_LIST$path |>
      gsub(pattern = "//+", replacement = "/")
  } else { # if no index: empty list
    DP_LIST <- data.frame(
      creator = character(),
      name = character(),
      title = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
    # Fill DP_LIST for first-time runs
    # Get list of DPs
    .files <- dir(isolate(PATHS$eal_dp), full.names = TRUE) |>
      gsub(pattern = "//", replacement = "/")
    # Set information per DP entry by reading json summary file
    if (length(.files) > 0) {
      sapply(.files, \ (.file) {
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
        if("dp.name" %in% names(.info$SelectDP)) browser()
        .row <- c(
          creator_orcid = "public", # default to public access
          name = .info$SelectDP$dp.name,
          title = .info$SelectDP$dp_title,
          path = .file
        )
        # Add to dp list
        DP_LIST <<- rbind(DP_LIST, .row)
      })
    }
  }
  # check DP_LIST columns are well named
  colnames(DP_LIST) <- c("creator", "name", "title", "path")

  # Curate DP_LIST versus actual list
  .actual_index <- dir(
    isolate(main_env$PATHS$eal_dp),
    pattern = "_emldp$",
    full.names = TRUE
  ) |>
    gsub(pattern = "//+", replacement = "/")
  # Only keep really existing DP
  DP_LIST <- dplyr::filter(DP_LIST, path %in% .actual_index)

  # save actual index
  data.table::fwrite(DP_LIST, isolate(PATHS$eal_dp_index), sep = "\t")
  # save DP_LIST
  assign("DP_LIST", DP_LIST, envir = main_env)
  # Make it reactive
  makeReactiveBinding("DP_LIST", env = main_env)

  # Values ====
  # Multiple purposes data

  # DataONE nodes
  .ENDPOINTS <- readDataTable(resource_paths$registeredEndpoints.txt)

  # Save
  assign(
    "VALUES",
    reactiveValues(
      dataone_endpoints = .ENDPOINTS,
      thresholds = reactiveValues(
        files_size_max = 2000000
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
      )
    ),
    envir = main_env
  )

  # Formats ====

  # Taxa authorities
  .TAXA_AUTHORITIES <- data.table::fread(resource_paths$taxaAuthorities.txt)

  # Unit types
  .all.units <- EML::get_unitList()$units
  .unit_list <- split(.all.units$name, .all.units$unitType)
  names(.unit_list)[[1]] <- "unsorted"
  
  # Save
  assign(
    "FORMATS",
    reactiveValues(
      dates = c(
        "YYYY-MM-DD", "YYYY",
        "YYYY-MM", "YYYY-DD-MM",
        "MM-YYYY", "DD-MM-YYYY", "MM-DD-YYYY",
        "hh:mm:ss", "hh:mm", "mm:ss", "hh",
        "YYYY-MM-DD hh:mm:ss", "YYYY-MM-DD hh:mm"
      ),
      lubridate_formats = lubridate:::lubridate_formats,
      units = .unit_list,
      taxa_authorities = .TAXA_AUTHORITIES
    ),
    envir = main_env
  )

  # Semantics ====
  # FIXME load ontologies
  assign(
    "SEMANTICS",
    reactiveValues(
      ontologies = character()
    ),
    envir = main_env
  )

  # Settings ====
  assign(
    "SETTINGS",
    reactiveValues(
      logged = FALSE,
      user = "public",
      orcid_token = character(),
      cedar_token = character(),
      metacat_token = character(),
      side_tab = c()
    ),
    envir = main_env
  )

  # EAL rv ====
  assign(
    "EAL",
    reactiveValues(
      old_page = 0,
      page = 1, # page number
      page_load = makeReactiveTrigger(),
      history = isolate(main_env$VALUES$steps[1]), # EAL pages names
      current = isolate(main_env$VALUES$steps[1]), # last of history
      completed = FALSE, # is current page completed?
      tag_list = tagList(), # side HTML tags to display
      help = character(),
      ping = "" # change character to ping EAL and trigger an observer
    ),
    envir = main_env
  )

  assign(
    "save_variable",
    initReactive(main_env = main_env),
    envir = main_env
  )

  # Local rv ====
  assign(
    "local_rv",
    reactiveValues(),
    envir = main_env
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
      date_regex = reactiveValues(
        DD = "^0?[1-9]$|^[12][0-9]$|^3[01]$",
        MM = "^0?[1-9]$|^1[0-2]$",
        YY = "^[0-9]{2}$",
        YYYY = "^[12][0-9]{3}$" # 1000 - 2999
      )
    ),
    envir = main_env
  )

  # output ====
  shinyOptions(shiny.reactlog = .args$reactlog)

  return(main_env)
}
