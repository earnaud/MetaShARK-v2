#' @noRd
templateModules <- function(main_env, page) {
  if (is.null(main_env$local_rv)) {
    stop("No content provided.")
  }
  if (is.null(main_env$save_variable)) {
    stop("No save_variable provided")
  }

  out <- if (page %in% c(1, 2, 3, 6)) {
    do.call(
      what = switch(as.character(page),
        "1" = ".templateInit",
        "2" = ".templateAttributes",
        "3" = ".templateCV_GeoCov",
        "5" = ".templateGeoCov_column",
        "6" = ".templateTaxCov"
      ),
      args = list(
        main_env
      )
    )
  } else {
    TRUE
  }

  return(out)
}

#' @import shiny
#' @importFrom EMLassemblyline template_directories template_core_metadata
#'
#' @noRd
.templateInit <- function(main_env) {
  dir.create(
    main_env$save_variable$SelectDP$dp_path,
    recursive = TRUE
  )

  if (exists("template_issues")) {
    rm("template_issues", envir = .GlobalEnv)
  }

  x <- try({
    EMLassemblyline::template_directories(
      main_env$save_variable$SelectDP$dp_path,
      main_env$save_variable$SelectDP$dp_name
    )

    EMLassemblyline::template_core_metadata(
      main_env$save_variable$SelectDP$dp_metadata_path,
      main_env$local_rv$dp_license()
    )

    # Check for EAL issues
    if (exists("template_issues")) {
      stop("EAL issues")
    }
  })

  # End set up variable
  if (class(x) != "try-error") {
    main_env$local_rv$dp_list <- c(
      main_env$local_rv$dp_list,
      main_env$local_rv$dp_name()
    )
    main_env$EAL$old_page <- main_env$EAL$page
    main_env$EAL$page <- main_env$EAL$page + 1
  } else { # Remove all that has been done
    # Remove DP folder
    unlink(
      main_env$save_variable$SelectDP$dp_path,
      recursive = TRUE
    )
    # Re-initialize save_variable
    main_env$save_variable <- initReactive(main_env = main_env$EAL)
    # Tell the user
    showNotification(
      x,
      type = "error",
      closeButton = TRUE,
      duration = NULL
    )
  }

  return(x)
}

#' @import shiny
#' @importFrom EMLassemblyline template_table_attributes
#'
#' @noRd
.templateAttributes <- function(main_env) {
  if (exists("template_issues")) {
    rm("template_issues", envir = .GlobalEnv)
  }

  # Clean non matching attributes files
  .att_filenames <- dir(main_env$save_variable$SelectDP$dp_metadata_path,
    full.names = TRUE, pattern = "^attrib"
  )
  .data_filenames <- dir(main_env$save_variable$SelectDP$dp_data_path,
    full.names = TRUE
  )
  .short_att_names <- basename(.att_filenames) |>
    gsub(pattern = "\\..*$", replacement = "") |>
    gsub(pattern = "^attributes_", replacement = "")
  .short_data_names <- basename(.data_filenames) |>
    gsub(pattern = "\\..*$", replacement = "")
  .to_remove <- which(!.short_att_names %in% .short_data_names)
  try(file.remove(.att_filenames[.to_remove])) # file may not actually exist

  # Template attributes
  x <- try({
    EMLassemblyline::template_table_attributes(
      path = isolate(main_env$save_variable$SelectDP$dp_metadata_path),
      data.path = isolate(main_env$save_variable$SelectDP$dp_data_path),
      data.table = isolate(main_env$save_variable$DataFiles$name)
    )

    # Check for EAL issues
    if (exists("template_issues")) {
      stop("EAL issues")
    }
  })

  if (class(x) == "try-error") {
    showNotification(
      x,
      type = "error",
      closeButton = TRUE,
      duration = NULL
    )
  }
}

#' @import shiny
#' @importFrom EMLassemblyline template_categorical_variables
#' template_geographic_coverage
#'
#' @noRd
.templateCV_GeoCov <- function(main_env) {
  devmsg(main_env$EAL$page, tag = "template")
  # for each attribute data frame
  md_tables <- if (main_env$EAL$page == 3) {
    main_env$local_rv$md_tables
  } else {
    main_env$save_variable$Attributes$content
  }

  # loop required to check each 'class' column
  # -- replaced in savevariable_functions.R by a reactive()
  .do_template_catvars <- if (main_env$EAL$page == 3) {
    main_env$local_rv$use_catvars()
  } else {
    any(sapply(
      listReactiveValues(main_env$save_variable$Attributes$content),
      function(table) any(table$class == "categorical")
    ))
  }

  if (exists("template_issues")) {
    rm("template_issues", envir = .GlobalEnv)
  }

  # EMLAL: template new fields if needed
  x <- try({
    if (isTRUE(.do_template_catvars)) {
      EMLassemblyline::template_categorical_variables(
        path = main_env$save_variable$SelectDP$dp_metadata_path,
        data.path = main_env$save_variable$SelectDP$dp_data_path
      )
    }

    # Check for EAL issues
    if (exists("template_issues")) {
      stop("EAL template issues - CatVar")
    }

    # Template if not existing
    if ("GeoCov" %in% names(main_env$save_variable)) {
      EMLassemblyline::template_geographic_coverage(
        path = main_env$save_variable$SelectDP$dp_metadata_path,
        data.path = main_env$save_variable$SelectDP$dp_data_path,
        empty = TRUE,
        write.file = TRUE
      )

      # TODO add templating for spatial coverage
      # .are_shp_files <- sapply(main_env$local_rv$data_filepath,
      #   EMLassemblyline:::is.shp.dir)
      # if (any(.are.shp.files)) {
      #   EMLassemblyline:::template_spatial_coverage(
      #     path = main_env$save_variable$SelectDP$dp_metadata_path,
      #     data.path = main_env$save_variable$SelectDP$dp.data.path
      #   )
      # }
    }

    # Check for EAL issues
    if (exists("template_issues")) {
      stop("EAL template issues - GeoCov")
    }

    return("done")
  })

  if (class(x) == "try-error") {
    devmsg(x[1], tag = "on template")
    showNotification(
      x,
      type = "error",
      closeButton = TRUE,
      duration = NULL
    )
  }
}

.templateGeoCov_column <- function(main_env) {
  if (exists("template_issues")) {
    rm("template_issues", envir = .GlobalEnv)
  }

  # expected to have common file between lat and lon
  # FIXME check this better
  data_file <- main_env$local_rv$columns$lat$file

  x <- try({
    EMLassemblyline::template_geographic_coverage(
      path = main_env$save_variable$SelectDP$dp_metadata_path,
      data.path = main_env$save_variable$SelectDP$dp_data_path,
      data.table = data_file,
      site.col = main_env$local_rv$columns$site$col,
      lat.col = main_env$local_rv$columns$lat$col,
      lon.col = main_env$local_rv$columns$lon$col,
      write.file = TRUE
    )
  })
}

#' @import shiny
#' @importFrom EMLassemblyline template_taxonomic_coverage
#'
#' @noRd
.templateTaxCov <- function(main_env) {
  if (exists("template_issues")) {
    rm("template_issues", envir = .GlobalEnv)
  }

  if (isTRUE(main_env$local_rv$complete)) {
    showModal(
      modalDialog(
        title = "Templating taxonomic coverage",
        tagList(
          tags$h3("Taxonomic coverage is being processed"),
          "Please wait until completion. This might take minutes.",
          "Selected authorities being queried:",
          main_env$FORMATS$taxa_authorities |>
            filter(id %in% main_env$local_rv$taxa_authority) |>
            select(authority) |>
            unlist() |>
            lapply(tags$li) |>
            tagList() |>
            tags$ul()
        ),
        footer = NULL
      )
    )

    x <- try({
      EMLassemblyline::template_taxonomic_coverage(
        main_env$save_variable$SelectDP$dp_metadata_path,
        main_env$save_variable$SelectDP$dp_data_path,
        taxa_table = main_env$local_rv$taxa_table,
        taxa_col = main_env$local_rv$taxa_col,
        taxa_name_type = main_env$local_rv$taxa_name_type,
        taxa_authority = main_env$local_rv$taxa_authority
      )

      # Check for EAL issues
      if (exists("template_issues")) {
        stop("EAL issues")
      }
    })

    removeModal()

    if (class(x) == "try-error") {
      showNotification(
        x,
        type = "error",
        closeButton = TRUE,
        duration = NULL
      )
    } else {
      showNotification(
        "Taxonomic Coverage has been written.",
        type = "message"
      )
    }
  } else {
    showNotification(
      "Taxonomic Coverage has been skipped.",
      type = "message"
    )
  }
}

checkTemplates <- function(main_env) {
  pat <- switch(as.character(main_env$EAL$page),
    `3` = "^attribute",
    `4` = "^catvar",
    `5` = "^geographic_coverage",
    `6` = "^taxonomic_coverage",
    ""
  )

  # Found at least one template file matching 'pat'
  check <- isContentTruthy(
    dir(
      main_env$save_variable$SelectDP$dp_metadata_path,
      pattern = pat
    )
  ) || # Or Clicked "Previous"
    main_env$EAL$page < main_env$EAL$old_page

  if (isFALSE(check)) { # clicked next and didn't find template
    templateModules(
      main_env,
      switch(as.character(main_env$EAL$page),
        `3` = 2,
        `4` = 3,
        `5` = 3,
        `6` = 6
      )
    )
  }
}
