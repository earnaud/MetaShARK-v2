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
      step = isolate(main.env$EAL$page),
      quick = FALSE,
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

#' @import shiny
#' @importFrom jsonlite write_json serializeJSON
#' 
#' @noRd
saveReactive <- function(
  save.variable,
  rv = NULL,
  write = FALSE
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
            rv = rv[[1]],
            write = write
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

#' @import shiny
setSavevar <- function(content, save.variable, lv = 1, root = "root") {
  
  lapply(
    names(content),
    function(label) {
      sub.content <- content[[label]]
      type.content <- typeof(sub.content)
      sub.save.variable <- savevar[[label]]
      type.save.variable <- typeof(sub.savevar)
      
      if (is.reactivevalues(sub.save.variable)) {
        if (!is.data.frame(sub.content) &&
            is.list(sub.content)) {
          x <- setSavevar(content[[label]], save.variable[[label]], lv = lv + 1, root = label)
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