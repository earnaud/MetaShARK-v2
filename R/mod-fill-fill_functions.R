# fill_functions.R

# Manage savevar variable -----------------------------------------------------
#' @title initReactive
#'
#' @description EMLAL module specific function. Initialize `savevar` variable.
#'
#' @param sublist either NULL, "emlal", "metafin" to precise which sublist to initialize (NULL initializes the whole variable)
#'
#' @importFrom shiny reactiveValues
initReactive <- function(sublist = NULL, savevar = NULL) {
  if (!is.null(sublist) && is.null(savevar)) {
    stop("Attempt to initialize savevar's sublist without savevar.")
  }
  if (!(is.null(sublist) || sublist %in% c("emlal", "metafin"))) {
    stop("Attempt to initialize savevar with inconsistent arguments")
  }

  # re-creates a whole savevar
  if (is.null(sublist)) {
    savevar <- reactiveValues()
  }

  # emlal reactivelist management
  if (is.null(sublist) || sublist == "emlal") {
    savevar$emlal <- reactiveValues(
      step = 0,
      quick = FALSE,
      history = character(),
      SelectDP = reactiveValues(
        dp_name = NULL,
        dp_path = NULL,
        dp_title = NULL
      ),
      DataFiles = data.frame(),
      CatVars = reactiveValues(),
      GeoCov = data.frame(),
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
          keywords = character(),
          keywordsThesaurus = character()
        ),
        temporal_coverage = NULL,
        additional_information = reactiveValues(
          content = character(),
          file = character()
        )
      )
    )
  }

  # metafin reactivelist management
  if (is.null(sublist) || sublist == "metafin") {
    savevar$metafin <- reactiveValues()
  }

  # differential returns
  return(if (is.null(sublist)) {
    savevar
  } else {
    switch(sublist,
      emlal = savevar$emlal,
      metafin = savevar$metafin
    )
  })
}

#' @describeIn initReactive
#'
#' @description save the `savevar` variable at wanted location
#'
#' @importFrom shiny withProgress incProgress
saveReactive <- function(savevar) {
  path = savevar$emlal$SelectDP$dp_path
  filename = savevar$emlal$SelectDP$dp_name
  location <- paste0(path, "/", filename, ".rds")
  
  withProgress({
      if (file.exists(location)) 
        file.remove(location)
      incProgress(1 / 3)
      saveRDS(savevar, location)
      incProgress(2 / 3)
    },
    message = "Saving current metadata"
  )
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

  readtext(
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
#' @importFrom shiny isTruthy
checkTruth <- function(x, output = NULL) {
  if (missing(x)) {
    stop("'x' is missing with no default.")
  }
  if (isTruthy(x) && isTruthy(unlist(x))) {
    return(x)
  } else {
    return(output)
  }
}
