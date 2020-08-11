# Main save variable ====

#' @import shiny
#'
#' @noRd
initReactive <- function(sub.list = NULL, save.variable = NULL, main.env) {
  if (!is.null(sub.list) && is.null(save.variable)) {
    stop("Attempt to initialize save.variable's sub.list without save.variable.")
  }
  if (!(is.null(sub.list) || sub.list %in% c("emlal", "metafin"))) {
    stop("Attempt to initialize save.variable with inconsistent arguments")
  }
  
  # re-creates a whole save.variable
  if (is.null(sub.list)) {
    save.variable <- reactiveValues()
  }
  # emlal reactivelist management
  else if (sub.list == "emlal") {
    save.variable <- reactiveValues(
      step = 1,
      quick = FALSE,
      creator = character(),
      history = isolate(main.env$EAL$history),
      SelectDP = reactiveValues(
        dp.name = NULL,
        dp.path = NULL,
        dp.metadata.path = NULL,
        dp.title = NULL
      ),
      DataFiles = data.frame(stringsAsFactors = FALSE),
      CatVars = reactiveValues(),
      GeoCov = reactiveValues(),
      TaxCov = reactiveValues(
        taxa.table = NULL,
        taxa.col = NULL,
        taxa.name.type = NULL,
        taxa.authority = NULL
      ),
      Personnel = reactiveValues(
        personnel = NULL
      ),
      Misc = reactiveValues(
        abstract = reactiveValues(
          content = character(),
          file = character()
        ),
        methods = reactiveValues(
          content = character(),
          file = character()
        ),
        keywords = reactiveValues(
          keyword = character(),
          keyword.thesaurus = character()
        ),
        temporal_coverage = NULL,
        additional.information = reactiveValues(
          content = character(),
          file = character()
        )
      )
    )
  }
  # metafin reactivelist management
  else if (sub.list == "metafin") {
    save.variable <- reactiveValues()
  }
  
  # differential returns
  return(save.variable)
}

#' @import shiny
#' @importFrom jsonlite write_json serializeJSON
#'
#' @noRd
saveReactive <- function(main.env) {
  if(is.null(main.env$local.rv))
    stop("No content provided.")
  if(is.null(main.env$save.variable))
    stop("No save variable provided")
  
  withProgress({
    setProgress(1 / 3, "Module save")
    
    # Write provided content
    main.env$save.variable <- do.call(
      switch(main.env$EAL$current,
             "SelectDP" = .saveSelectDP,
             "Data Files" = .saveDataFiles,
             "Attributes" = .saveAttributes,
             "Categorical Variables" = .saveCatVars,
             "Geographic Coverage" = .saveGeoCov,
             "Taxonomic Coverage" = .saveTaxCov,
             "Personnel" = .savePersonnel,
             "Miscellaneous" = .saveMisc
      ),
      args = list(
        main.env
      )
    )
    
    setProgress(2 / 3, "Global save")
    
    # Save JSON
    path <- main.env$save.variable$SelectDP$dp.path
    filename <- main.env$save.variable$SelectDP$dp.name
    location <- paste0(path, "/", filename, ".json")
    if (file.exists(location)) {
      file.remove(location)
    }
    jsonlite::write_json(
      jsonlite::serializeJSON(
        listReactiveValues(main.env$save.variable)
      ),
      location
    )
    
    incProgress(1 / 3)
  }) %>% isolate()
  
  showNotification(
    paste("Saved:", main.env$EAL$current, "."),
    duration = 1.5,
    type = "message"
  )
}

#' @import shiny
setSaveVariable <- function(content, save.variable, lv = 1, root = "root") {
  lapply(
    names(content),
    function(label) {
      sub.content <- content[[label]]
      type.content <- typeof(sub.content)
      sub.save.variable <- save.variable[[label]]
      type.save.variable <- typeof(sub.save.variable)
      
      if (is.reactivevalues(sub.save.variable)) {
        if (!is.data.frame(sub.content) &&
            is.list(sub.content)) {
          x <- setSaveVariable(content[[label]], save.variable[[label]], lv = lv + 1, root = label)
        }
        else {
          x <- sub.content
        }
      }
      else {
        x <- sub.content
      }
      
      isolate(save.variable[[label]] <- x)
      return(NULL)
    }
  )
  
  return(save.variable)
}

# Local save variable ====
setLocalRV <- function(main.env){
  
  browser(expr = main.env$EAL$page == 3)
  
  main.env$local.rv <- switch(
    main.env$EAL$current,
    # SelectDP ----
    "SelectDP" = reactiveValues(
      dp.name = character(),
      dp.title = character(),
      dp.list = list.files(
        main.env$PATHS$eal.dp,
        pattern = "_emldp$",
        full.names = FALSE
      ),
      dp.license = NULL
    ),
    # DataFiles ----
    "Data Files" = reactiveValues(
      data.files = if (checkTruth(main.env$save.variable$DataFiles)) { # from create button in SelectDP
        .ind <- which(file.exists(main.env$save.variable$DataFiles$datapath))
        .col <- which(names(main.env$save.variable$DataFiles) != "metadatapath")
        main.env$save.variable$DataFiles[.ind, .col]
      }
      else
        data.frame(stringsAsFactors = FALSE)
    ),
    # Attributes ----
    "Attributes" = reactiveValues(
      current.file = 0,
      tables = NULL,
      current.table = NULL,
      current.preview = NULL,
      cu.table = data.frame(stringsAsFactors = FALSE),
      cu.values = rep(NA, 5),
      modal.on = FALSE,
      unit.id = character(),
      units.list = isolate(main.env$FORMATS$units),
      annotations = reactiveValues(
        values = data.frame(stringsAsFactors = FALSE),
        count = 0
      )
    ),
    # CatVars ----
    "Categorical Variables" = reactiveValues(
      current.index = 0
    ),
    # GeoCov ----
    "Geographic Coverage" = reactiveValues(
      columns = reactiveValues(
        choices = reactiveValues(
          files = "all",
          sites = NA_character_,
          coords = NA_character_
        ),
        site = reactiveValues(
          col = character(),
          file = character()
        ),
        lat = reactiveValues(
          col = character(),
          file = character()
        ),
        lon = reactiveValues(
          col = character(),
          file = character()
        ),
        complete = FALSE
      ),
      custom = reactiveValues(
        id = numeric(),
        coordinates = data.frame(
          geographicDescription = character(),
          northBoundingCoordinate = numeric(),
          southBoundingCoordinate = numeric(),
          eastBoundingCoordinate = numeric(),
          westBoundingCoordinate = numeric(),
          stringsAsFactors = FALSE
        ),
        complete = FALSE
      )
    ),
    # TaxCov ----
    "Taxonomic Coverage" = reactiveValues(
      taxa.table = character(),
      taxa.col = character(),
      taxa.name.type = character(),
      taxa.authority = character(),
      complete = FALSE
    ),
    # Personnel ----
    "Personnel" = reactiveValues(
      Personnel = data.frame(
        id = numeric(),
        # Basic Identity
        givenName = character(),
        middleInitial = character(),
        surName = character(),
        # Contact
        organizationName = character(),
        electronicMailAddress = character(),
        # Personnel information
        userId = character(),
        role = character(),
        # Project information
        projectTitle = character(),
        fundingAgency = character(),
        fundingNumber = character(),
        stringsAsFactors = FALSE
      )
    ),
    # Misc ----
    "Miscellaneous" = reactiveValues(
      # Abstract
      abstract = reactiveValues(
        content = character(),
        file = paste(
          isolate(main.env$save.variable$SelectDP$dp.metadata.path),
          "abstract.txt",
          sep = "/"
        )
      ),
      # Methods
      methods = reactiveValues(
        content = character(),
        file = paste(
          isolate(main.env$save.variable$SelectDP$dp.metadata.path),
          "methods.txt",
          sep = "/"
        )
      ),
      # Keywords
      keywords = reactiveValues(
        keyword = kw$keyword,
        keyword.thesaurus = kw$keyword.thesaurus
      ),
      # Temporal coverage
      temporal.coverage = c(Sys.Date() - 1, Sys.Date()),
      # Additional information
      additional.information = reactiveValues(
        content = character(),
        file = paste(
          isolate(main.env$save.variable$SelectDP$dp.metadata.path),
          "additional_info.txt",
          sep = "/"
        )
      )
    )
    # (End) ----
  )
  
  return(main.env)
}