# Manage savevar variable -----------------------------------------------------
#' @title initReactive
#'
#' @description EMLAL module specific function. Initialize `savevar` variable.
#'
#' @param sub.list either NULL, "emlal", "metafin" to precise which sub.list to initialize (NULL initializes the whole variable)
#'
#' @import shiny
initReactive <- function(sub.list = NULL, savevar = NULL, main.env) {
  if (!is.null(sub.list) && is.null(savevar)) {
    stop("Attempt to initialize savevar's sub.list without savevar.")
  }
  if (!(is.null(sub.list) || sub.list %in% c("emlal", "metafin"))) {
    stop("Attempt to initialize savevar with inconsistent arguments")
  }

  # re-creates a whole savevar
  if (is.null(sub.list)) {
    savevar <- reactiveValues()
  }

  # emlal reactivelist management
  if (is.null(sub.list) || sub.list == "emlal") {
    savevar$emlal <- reactiveValues(
      step = main.env$EAL$navigate,
      quick = FALSE,
      history = main.env$EAL$history,
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
  if (is.null(sub.list) || sub.list == "metafin") {
    savevar$metafin <- reactiveValues()
  }

  # differential returns
  return(if (is.null(sub.list)) {
    savevar
  } else {
    switch(sub.list,
      emlal = savevar$emlal,
      metafin = savevar$metafin
    )
  })
}

#' @describeIn initReactive
#'
#' @description save the `savevar` variable at wanted location
#'
#' @return
#' `savevar` modified.
#'
#' @import shiny
#' @importFrom jsonlite write_json serializeJSON
saveReactive <- function(
  savevar,
  rv = NULL,
  main.env = NULL
) {
  withProgress({
    setProgress(1 / 3, "Module save")

    # Write provided rv
    if (!is.null(rv)) {
      if (is.null(names(rv))) {
        message("No module name! Give it as a name for `rv`.")
      } else {
        rv <- rv[1]

        savevar <- do.call(
          switch(names(rv),
            DataFiles = .saveDataFiles,
            Attributes = .saveAttributes,
            CatVars = .saveCatVars,
            GeoCov = .saveGeoCov,
            TaxCov = .saveTaxCov,
            Personnel = .savePersonnel,
            Misc = .saveMisc,
          ),
          args = list(
            savevar = savevar,
            rv = rv[[1]]
          )
        )
        
        # if (names(rv) == "DataFiles") {
        #   savevar <- .saveDataFiles(
        #     savevar = savevar,
        #     rv = rv[[1]]
        #   )
        # }
        # if (names(rv) == "Attributes") {
        #   savevar <- .saveAttributes(
        #     savevar = savevar,
        #     rv = rv[[1]]
        #   )
        # }
        # if (names(rv) == "CatVars") {
        #   savevar <- .saveCatVars(
        #     savevar = savevar,
        #     rv = rv[[1]]
        #   )
        # }
        # if (names(rv) == "GeoCov") {
        #   savevar <- .saveGeoCov(
        #     savevar = savevar,
        #     rv = rv[[1]],
        #     main.env = main.env
        #   )
        # }
        # if (names(rv) == "TaxCov") {
        #   savevar <- .saveTaxCov(
        #     savevar = savevar,
        #     rv = rv[[1]]
        #   )
        # }
        # if (names(rv) == "Personnel") {
        #   savevar <- .savePersonnel(
        #     savevar = savevar,
        #     rv = rv[[1]]
        #   )
        # }
        # if (names(rv) == "Misc") {
        #   savevar <- .saveMisc(
        #     savevar = savevar,
        #     rv = rv[[1]]
        #   )
        # }
      }
    }

    setProgress(2 / 3, "Global save")

    # Save JSON
    path <- savevar$emlal$SelectDP$dp.path
    filename <- savevar$emlal$SelectDP$dp.name
    location <- paste0(path, "/", filename, ".json")
    if (file.exists(location)) {
      file.remove(location)
    }
    jsonlite::write_json(
      jsonlite::serializeJSON(listReactiveValues(savevar)),
      location
    )

    incProgress(1 / 3)
  }) %>% isolate

  showNotification(
    paste("Saved:", names(rv)[1], "!"), 
    duration = 1.5, 
    type = "message"
  )

  return(savevar)
}

#' @title readFilesText
#'
#' @param files files basename located in the same directory or,
#' if prefix = NULL, list of full filenames to read
#' @param prefix common file prefix for all file names
#' specified in 'files'. By default, sep = "/"
#'
#' @importFrom readtext readtext
readPlainText <- function(files, prefix = NULL, sep = "/", ...) {
  if (is.null(prefix)) sep <- ""

  readtext::readtext(
    paste(
      prefix,
      files,
      sep = sep
    )
  )$text
}

#' @title checkTruth
#'
#' @description check if `x` is truthy (as shiny::isTruthy) or not.
#' Returns the argument if truthy, or the `output` argument if not (default to NULL)
#'
#' @param x argument to check fo truthiness
#' @param output what to return if `x` is not truthy
#'
#' @import shiny
checkTruth <- function(x) {
  if (missing(x)) {
    stop("'x' is missing with no default.")
  }
  return(isTruthy(x) && isTruthy(unlist(x)))
}
