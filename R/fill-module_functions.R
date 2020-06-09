# fill_functions.R

# Manage savevar variable -----------------------------------------------------
#' @title initReactive
#'
#' @description EMLAL module specific function. Initialize `savevar` variable.
#'
#' @param sublist either NULL, "emlal", "metafin" to precise which sublist to initialize (NULL initializes the whole variable)
#'
#' @importFrom shiny reactiveValues
initReactive <- function(sublist = NULL, savevar = NULL, glob) {
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
      step = glob$NAVIGATE,
      quick = FALSE,
      history = glob$HISTORY,
      SelectDP = reactiveValues(
        dp_name = NULL,
        dp_path = NULL,
        dp_title = NULL
      ),
      DataFiles = data.frame(),
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
#' @return 
#' `savevar` modified.
#' 
#' @importFrom shiny withProgress incProgress isolate
#' @importFrom jsonlite write_json serializeJSON
saveReactive <- function(
  savevar, 
  rv = NULL, 
  .method = "",
  .values = c(),
  globals = NULL
){
  withProgress({
    setProgress(1/3, "Module save")
    
    # Write provided rv
    if(!is.null(rv)){
      if(is.null(names(rv)))
        message("No module name! Give it as a name for `rv`.")
      else {
        rv <- rv[1]
        
        if(names(rv) == "DataFiles") {
          savevar <- .saveDataFiles(
            savevar = savevar, 
            rv = rv[[1]]
          )
        } 
        if(names(rv) == "Attributes") {
          savevar <- .saveAttributes(
            savevar = savevar, 
            rv = rv[[1]]
          )
        } 
        if(names(rv) == "CatVars") {
          savevar <- .saveCatVars(
            savevar = savevar, 
            rv = rv[[1]]
          )
        } 
        if(names(rv) == "GeoCov") {
          savevar <- .saveGeoCov(
            savevar = savevar,
            rv = rv[[1]],
            .method = .method,
            .values = .values,
            globals = globals
          )
        } 
        if(names(rv) == "TaxCov") {
          savevar <- .saveTaxCov(
            savevar = savevar,
            rv = rv[[1]]
          )
        } 
        if(names(rv) == "Personnel") {
          savevar <- .savePersonnel(
            savevar = savevar,
            rv = rv[[1]]
          )
        } 
        if(names(rv) == "Misc") {
          savevar <- .saveMisc(
            savevar = savevar,
            rv = rv[[1]]
          )
        } 
      }
    }
    
    setProgress(2/3, "Global save")
    
    # Save JSON
    path <- savevar$emlal$SelectDP$dp_path
    filename <- savevar$emlal$SelectDP$dp_name
    location <- paste0(path, "/", filename, ".json")
    if (file.exists(location))
      file.remove(location)
    write_json(
      serializeJSON(listReactiveValues(savevar)),
      location
    )
    
    incProgress(1/3)
  }) %>% isolate
  
  showNotification("Saved !", duration = 1.5, type = "message")
  
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
checkTruth <- function(x) {
  if (missing(x)) {
    stop("'x' is missing with no default.")
  }
  return (isTruthy(x) && isTruthy(unlist(x)))
}
