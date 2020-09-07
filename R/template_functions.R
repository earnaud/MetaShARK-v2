#' @noRd
template <- function(main.env, page){
  if(is.null(main.env$local.rv))
    stop("No content provided.")
  if(is.null(main.env$save.variable))
    stop("No save variable provided")
  
  if(page %in% c(1,2,3,6))
    x <- do.call(
      what = switch(page,
        `1` = ".templateInit",
        `2` = ".templateAttributes",
        `3` = ".templateCV_GeoCov",
        `6` = ".templateTaxCov"
      ),
      args = list(
        main.env
      )
    )
  else
    x <- TRUE
  
  return(x)
}

.templateInit <- function(main.env){
  x <- try({
    EMLassemblyline::template_directories(
      main.env$save.variable$SelectDP$dp.path,
      main.env$save.variable$SelectDP$dp.name
    )
    EMLassemblyline::template_core_metadata(
      main.env$save.variable$SelectDP$dp.metadata.path,
      main.env$local.rv$dp.license()
    )
  })
  
  # End set up variable
  if (class(x) != "try-error") {
    main.env$local.rv$dp.list <- c(
      main.env$local.rv$dp.list, 
      main.env$local.rv$dp.name()
    )
    main.env$EAL$old.page <- main.env$EAL$page
    main.env$EAL$page <- main.env$EAL$page + 1
  } else {
    unlink(
      main.env$save.variable$SelectDP$dp.path,
      recursive = TRUE
    )
    main.env$save.variable <- initReactive(main.env = main.env$EAL)
    showNotification(
      x,
      type = "error",
      closeButton = TRUE,
      duration = NULL
    )
  }
  
  return(x)
}

.templateAttributes <- function(main.env){
  x <- try({
    EMLassemblyline::template_table_attributes(
      path = isolate(main.env$save.variable$SelectDP$dp.metadata.path),
      data.path = isolate(main.env$save.variable$SelectDP$dp.data.path),
      data.table = isolate(main.env$save.variable$DataFiles$name)
    )
  })
  
  if(class(x) == "try-error") {
    isolate({main.env$EAL$page <- main.env$EAL$page - 1})
  }
  
  return(x)
}

.templateCV_GeoCov <- function(main.env){
  # for each attribute data frame
  .do.template.catvars <- sapply(
    seq_along(main.env$local.rv$md.filenames),
    function(cur_ind) {
      # check for direction: CustomUnits or CatVars
      return(isTRUE("categorical" %in% main.env$local.rv$md.tables[[cur_ind]][, "class"]))
    }
  ) %>%
    unlist() %>%
    any()
  
  # EMLAL: template new fields if needed
  x <- try({
    if (isTRUE(.do.template.catvars)) {
      EMLassemblyline::template_categorical_variables(
        path = main.env$save.variable$SelectDP$dp.metadata.path,
        data.path = main.env$save.variable$SelectDP$dp.data.path
      )
    }
    
    EMLassemblyline::template_geographic_coverage(
      path = main.env$save.variable$SelectDP$dp.metadata.path,
      data.path = main.env$save.variable$SelectDP$dp.data.path,
      empty = TRUE,
      write.file = TRUE
    )
  })
  
  if (isFALSE(.do.template.catvars)) {
    isolate({main.env$EAL$page <- main.env$EAL$page + 1})
  }
}

.templateTaxCov <- function(main.env){
  if (isTRUE(main.env$EAL$completed)) {
    x <- try(
      template_taxonomic_coverage(
        main.env$save.variable$emlal$SelectDP$dp_metadata_path,
        main.env$save.variable$emlal$SelectDP$dp_data_path,
        taxa.table = rv$taxa.table,
        taxa.col = rv$taxa.col,
        taxa.name.type = rv$taxa.name.type,
        taxa.authority = rv$taxa.authority
      )
    )
    showNotification(
      "Taxonomic Coverage has been written.",
      type = "message"
    )
  }
  else {
    x <- try(TRUE)
    showNotification(
      "Taxonomic Coverage has been skipped.",
      type = "message"
    )
  }
}