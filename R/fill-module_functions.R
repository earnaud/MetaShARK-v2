# Manage save.variable variable -----------------------------------------------------
#' @import shiny
#' 
#' @noRd
initReactive <- function(sub.list = NULL, save.variable = NULL, main.env) {
  if (!is.null(sub.list) && is.null(save.variable)) {
    stop("Attempt to initialize save.variable's sub.list without savevar.")
  }
  if (!(is.null(sub.list) || sub.list %in% c("emlal", "metafin"))) {
    stop("Attempt to initialize save.variable with inconsistent arguments")
  }

  # re-creates a whole save.variable
  if (is.null(sub.list)) {
    save.variable <- reactiveValues()
  }

  # emlal reactivelist management
  if (is.null(sub.list) || sub.list == "emlal") {
    save.variable$emlal <- reactiveValues(
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
    save.variable$metafin <- reactiveValues()
  }

  # differential returns
  return(if (is.null(sub.list)) {
    save.variable
  } else {
    switch(sub.list,
      emlal = save.variable$emlal,
      metafin = save.variable$metafin
    )
  })
}

#' @describeIn initReactive
#'
#' @description save the `save.variable` variable at wanted location
#'
#' @return
#' `save.variable` modified.
#'
#' @import shiny
#' @importFrom jsonlite write_json serializeJSON
saveReactive <- function(
  save.variable,
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

        save.variable <- do.call(
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
            save.variable = savevar,
            rv = rv[[1]]
          )
        )
        
        # if (names(rv) == "DataFiles") {
        #   save.variable <- .saveDataFiles(
        #     save.variable = savevar,
        #     rv = rv[[1]]
        #   )
        # }
        # if (names(rv) == "Attributes") {
        #   save.variable <- .saveAttributes(
        #     save.variable = savevar,
        #     rv = rv[[1]]
        #   )
        # }
        # if (names(rv) == "CatVars") {
        #   save.variable <- .saveCatVars(
        #     save.variable = savevar,
        #     rv = rv[[1]]
        #   )
        # }
        # if (names(rv) == "GeoCov") {
        #   save.variable <- .saveGeoCov(
        #     save.variable = savevar,
        #     rv = rv[[1]],
        #     main.env = main.env
        #   )
        # }
        # if (names(rv) == "TaxCov") {
        #   save.variable <- .saveTaxCov(
        #     save.variable = savevar,
        #     rv = rv[[1]]
        #   )
        # }
        # if (names(rv) == "Personnel") {
        #   save.variable <- .savePersonnel(
        #     save.variable = savevar,
        #     rv = rv[[1]]
        #   )
        # }
        # if (names(rv) == "Misc") {
        #   save.variable <- .saveMisc(
        #     save.variable = savevar,
        #     rv = rv[[1]]
        #   )
        # }
      }
    }

    setProgress(2 / 3, "Global save")

    # Save JSON
    path <- save.variable$emlal$SelectDP$dp.path
    filename <- save.variable$emlal$SelectDP$dp.name
    location <- paste0(path, "/", filename, ".json")
    if (file.exists(location)) {
      file.remove(location)
    }
    jsonlite::write_json(
      jsonlite::serializeJSON(listReactiveValues(save.variable)),
      location
    )

    incProgress(1 / 3)
  }) %>% isolate

  showNotification(
    paste("Saved:", names(rv)[1], "!"), 
    duration = 1.5, 
    type = "message"
  )

  return(save.variable)
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