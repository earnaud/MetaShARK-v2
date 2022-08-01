#' Executed after changing page
#'
#' @import shiny
#' @importFrom jsonlite write_json serializeJSON
#'
#' @noRd
saveReactive <- function(main_env, page, do_template = TRUE) {
  if (is.null(main_env$local_rv)) {
    stop("No content provided.")
  }
  if (is.null(main_env$save_variable)) {
    stop("No save_variable provided")
  }

  withProgress({
    isolate({
      # Save local variable content ----
      setProgress(1 / 3, "Save metadata")
      .tmp_save <- NULL
      if (page != 9) {
        .tmp_save <- do.call(
          what = switch(page,
            ".saveSelectDP",
            ".saveDataFiles",
            ".saveAttributes",
            ".saveCatVars",
            ".saveGeoCov",
            ".saveTaxCov",
            ".savePersonnel",
            ".saveMisc"
          ),
          args = list(main_env)
        )

        if (class(.tmp_save) != "try-error" || !is.null(.tmp_save)) {
          main_env$save_variable <- .tmp_save
        }
      }

      # Template ----
      .tmp_template <- TRUE
      if (isTRUE(do_template)) {
        .tmp_template <- templateModules(main_env, page)
      }

      # Error catching ----
      # If an error came out, do not go further
      if (class(.tmp_save) == "try-error" ||
          class(.tmp_template) == "try-error") {
        # not page-1 but oldpage: comes back to previously visited
        isolate({
          main_env$EAL$page <- main_env$EAL$old_page
        })
        setProgress(1, "Exit save")
        stop("Error arose while saving.")
      }

      # Save JSON ----
      setProgress(2 / 3, "Write JSON")

      # set files + path
      path <- main_env$save_variable$SelectDP$dp_path
      filename <- main_env$save_variable$SelectDP$dp_name
      location <- paste0(path, "/", filename, ".json")

      # overwrite files
      if (dir.exists(path)) {
        if (file.exists(location)) {
          file.remove(location)
        }
        jsonlite::write_json(
          jsonlite::serializeJSON(
            listReactiveValues(main_env$save_variable)
          ),
          location
        )
      } else {
        devmsg("%s not found.", path, tag = "fill_module_saves.R")
      }

      setProgress(1)
    }) # end of isolate
  })

  showNotification(
    paste("Saved:", main_env$EAL$current, "."),
    duration = 2.5,
    type = "message"
  )

  return(NULL)
}

#' @noRd
.saveSelectDP <- function(main_env) {
  # Shortcuts
  .sv <- main_env$save_variable
  content <- main_env$local_rv
  # Save content in sv
  .sv$SelectDP$dp_name <- content$dp_name()
  .sv$SelectDP$dp_path <- paste0(
    main_env$PATHS$eal_dp,
    content$dp_name(),
    "_emldp"
  )
  .sv$SelectDP$dp_metadata_path <- paste(
    .sv$SelectDP$dp_path,
    content$dp_name(),
    "metadata_templates",
    sep = "/"
  )
  .sv$SelectDP$dp_data_path <- paste(
    .sv$SelectDP$dp_path,
    content$dp_name(),
    "data_objects",
    sep = "/"
  )
  .sv$SelectDP$dp_eml_path <- paste(
    .sv$SelectDP$dp_path,
    content$dp_name(),
    "eml",
    sep = "/"
  )
  .sv$SelectDP$dp_title <- content$dp_title()
  .sv$creator <- if (isTRUE(main_env$SETTINGS$logged)) {
    main_env$SETTINGS$user
  } else {
    "public"
  } # prefered to set it properly than only with variable

  return(.sv)
}

#' @importFrom dplyr select
#'
#' @noRd
.saveDataFiles <- function(main_env) {
  .sv <- main_env$save_variable
  .tmp <- main_env$local_rv$data_files
  .tmp <- try(dplyr::select(.tmp, -id), silent = TRUE)
  # Format content
  if (!isContentTruthy(.tmp) || class(.tmp) == "try-error") {
    devmsg("Invalid content.", tag = "fill_saves.R")
    # don't change .sv
  } else {
    # -- Get files data
    .from <- .tmp$datapath
    .to <- paste0(
      .sv$SelectDP$dp_data_path,
      "/", .tmp$name
    )
    file.copy(
      from = .from,
      to = .to
    )
    .tmp$datapath <- .to

    # -- set metadatapath
    .tmp$metadatapath <- paste(
      .sv$SelectDP$dp_metadata_path,
      sub(
        "(.*)\\.[a-zA-Z0-9]*$",
        "attributes_\\1.txt",
        .tmp$name
      ),
      sep = "/"
    )
    .tmp[] <- lapply(.tmp, as.character)

    # Save
    .sv$DataFiles <- .tmp
    try(main_env$local_rv$data_files$datapath <- .tmp$datapath)
  }

  return(.sv)
}

#' @importFrom dplyr filter select
#' @importFrom data.table fwrite
#'
#' @noRd
.saveAttributes <- function(main_env) {
  .sv <- main_env$save_variable
  content <- main_env$local_rv
  # Save
  .sv$Attributes$content <- content$md_tables
  devmsg(names(content$md_tables), tag = "save attributes")

  # Write attribute tables
  sapply(
    names(content$md_tables),
    function(tablename) {
      # write filled tables
      path <- .sv$DataFiles |>
        dplyr::filter(grepl(tablename, metadatapath)) |>
        dplyr::select(metadatapath) |>
        unlist()
      table <- content$md_tables[[tablename]]
      data.table::fwrite(table, path, sep = "\t")
    }
  )

  # Write Custom units
  if (isContentTruthy(content$CU_Table)) {
    data.table::fwrite(
      content$CU_Table,
      paste0(.sv$SelectDP$dp_metadata_path, "/custom_units.txt")
    )
  }

  return(.sv)
}

#' @importFrom data.table fwrite
#'
#' @noRd
.saveCatVars <- function(main_env) {
  content <- main_env$local_rv

  sapply(content$cv.files, function(.file_path) {
    .file_name <- basename(.file_path)

    # Fix content
    content$cv_tables[[.file_name]]$
      code <- gsub("\"\"", "", content$cv_tables[[.file_name]]$code)
    
    # Save
    main_env$save_variable$
      CatVars[[.file_name]] <- content$cv_tables[[.file_name]]

    # Overwrite
    file.remove(.file_path)
    data.table::fwrite(
      main_env$save_variable$CatVars[[.file_name]],
      .file_path,
      sep = "\t",
      na = "NA",
      quote = FALSE
    )
  })

  return(main_env$save_variable)
}

#' @noRd
#'
#' @importFrom data.table fwrite
#' @importFrom sf st_polygon st_as_text
#' @importFrom dplyr bind_rows
#' @import shiny
.saveGeoCov <- function(main_env) {
  .sv <- main_env$save_variable
browser()
  # Initialize variables
  .method <- main_env$local_rv$method

  .data_files <- .sv$DataFiles$datapath
  .data_content <- lapply(.data_files, readDataTable)
  names(.data_content) <- basename(.data_files)

  # format extracted content - keep latlon-valid columns
  .data_content_coordinates <- lapply(
    names(.data_content),
    function(.data_filename) {
      df <- .data_content[[.data_filename]]
      .df_num <- unlist(
        lapply(df, function(.df_col) {
          .df_col <- .df_col[!is.na(.df_col)]
          if (is.character(.df_col)) {
            .df_col <- enc2utf8(.df_col)
          }
          all(grepl(main_env$PATTERNS$coordinates, .df_col))
        })
      )
      df[, .df_num]
    }
  )
  names(.data_content_coordinates) <- basename(.data_files)

  .values <- list(
    .data_content = .data_content,
    .data_content_coordinates = .data_content_coordinates
  )

  geocov <- NULL
  .sv$GeoCov <- reactiveValues() # reset
  .sv$GeoCov$method <- .method

  # Columns ----
  if (.method == "columns" &&
    isContentTruthy(main_env$local_rv$columns$site) &&
    isContentTruthy(main_env$local_rv$columns)) {
    devmsg("Geographic Coverage saved with columns",
           tag = "fill_module_saves.R")
    .sv$GeoCov$columns <- main_env$local_rv$columns


    # Replaced with a EAL templating functions
    geocov <- NULL
  }

  # Custom ----
  if (.method == "custom") {
    devmsg("Geographic Coverage saved with custom", tag = "fill_module_saves.R")
    browser()
    # shortcuts
    .local_rv <- main_env$local_rv$custom
    .features_ids <- names(.local_rv)[
      !names(.local_rv) %in% c("count", "complete")
    ]

    # build coverage table from local_rv
    .custom_coordinates <- lapply(.features_ids, function(feat_id) {
      .points <- .local_rv[[feat_id]]$points
      if (.local_rv[[feat_id]]$type == "marker") {
        .points <- .points[1, ]
      }
      if (.local_rv[[feat_id]]$type == "rectangle") {
        .points <- .points[1:2, ]
      }

      data.frame(
        geographicDescription = .local_rv[[feat_id]]$description,
        northBoundingCoordinate = max(.points$lat),
        southBoundingCoordinate = min(.points$lat),
        eastBoundingCoordinate = max(.points$lon),
        westBoundingCoordinate = min(.points$lon),
        wkt = if (.local_rv[[feat_id]]$type == "polygon") {
          .points[c(seq_row(.points), 1), 2:3] |>
            as.matrix() |>
            list() |>
            st_polygon() |>
            st_as_text()
        } else {
          ""
        }
      ) # end of data.frame
    }) |>
      bind_rows()

    # save
    browser()
    .sv$GeoCov$custom <- main_env$local_rv$custom

    # fill
    geocov <- .custom_coordinates
  }

  # Write data
  if (isContentTruthy(geocov)) {
    data.table::fwrite(
      geocov[, 1:5], # do not write wkt column in geocov
      paste(
        .sv$SelectDP$dp_metadata_path,
        "geographic_coverage.txt",
        sep = "/"
      ),
      sep = "\t"
    )

    if ("wkt" %in% names(geocov)) {
      data.table::fwrite(
        geocov["wkt"], # do not write wkt column in geocov
        paste(
          .sv$SelectDP$dp_metadata_path,
          ".spatial_coverage.txt",
          sep = "/"
        ),
        sep = "\t"
      )
    }
  }

  # Output
  return(.sv)
}

#' @noRd
.saveTaxCov <- function(main_env) {
  .sv <- main_env$save_variable
  content <- main_env$local_rv

  # Save
  .sv$TaxCov$taxa_table <- content$taxa_table
  .sv$TaxCov$taxa_col <- content$taxa_col
  .sv$TaxCov$taxa_name_type <- content$taxa_name_type
  .sv$TaxCov$taxa_authority <- content$taxa_authority

  main_env$save_variable <- .sv
}

#' @importFrom data.table fwrite
#' @importFrom dplyr mutate
#'
#' @noRd
.savePersonnel <- function(main_env) {
  .sv <- main_env$save_variable
  content <- main_env$local_rv

  if (isContentTruthy(content$Personnel)) {
    # Save variable
    .sv$Personnel <- content$Personnel

    # Write file
    # prettify
    cols <- c(
      "givenName", "middleInitial", "surName",
      "organizationName", "electronicMailAddress",
      "userId", "role",
      "projectTitle", "fundingAgency", "fundingNumber"
    )
    personnel <- content$Personnel[names(content$Personnel) %in% cols]
    .personnel <- personnel[NULL, ]
    sapply(seq_row(personnel), function(ind) {
      row <- personnel[ind, ]

      roles <- unlist(strsplit(row$role, ","))
      lapply(seq(roles), function(ind) {
        .personnel <<- rbind(
          .personnel,
          row |> dplyr::mutate(role = roles[ind])
        )
      })
    })

    # File template for personnel
    data.table::fwrite(
      .personnel,
      paste0(
        .sv$SelectDP$dp_metadata_path,
        "/personnel.txt"
      ),
      sep = "\t"
    )
  }

  # Output
  return(.sv)
}

#' @import shiny
#' @importFrom data.table fwrite
#' @importFrom htmltools save_html HTML
#' @importFrom rmarkdown pandoc_convert
#' @importFrom dplyr bind_rows
#'
#' @noRd
.saveMisc <- function(main_env) {
  .sv <- main_env$save_variable
  content <- main_env$local_rv

  # save local variable ----
  content$abstract$file <- paste0(.sv$SelectDP$dp_metadata_path, "/abstract.md")
  content$methods$file <- paste0(.sv$SelectDP$dp_metadata_path, "/methods.md")
  content$additional_information$file <- paste0(
    .sv$SelectDP$dp_metadata_path,
    "/additional_info.md"
  )
  .sv$Misc <- content

  # abstract ----
  saveHTMLasMD(content$abstract)
  removeDuplicateFiles(
    content$abstract$file,
    main_env$save_variable$SelectDP$dp_metadata_path)

  # methods ----
  saveHTMLasMD(content$methods)
  removeDuplicateFiles(
    content$methods$file,
    main_env$save_variable$SelectDP$dp_metadata_path)

  # keywords ----
  # Set NA thesaurus as "" thesaurus
  content$keywords[["keywordThesaurus"]] <- replace(
    content$keywords[["keywordThesaurus"]],
    which(is.na(content$keywords[["keywordThesaurus"]])),
    ""
  )
  # build keywords data.frame
  .keywords <- lapply(unique(content$keywords$keywordThesaurus), function(kwt) {
    row.ind <- which(content$keywords$keywordThesaurus == kwt)

    data.frame(
      keyword = strsplit(content$keywords$keyword[row.ind], ",") |>
        unlist(),
      keywordThesaurus = kwt # repeated as many times as necessary
    )
  }) |>
    dplyr::bind_rows()
  # write files
  data.table::fwrite(
    .keywords,
    paste0(
      .sv$SelectDP$dp_metadata_path,
      "/keywords.txt"
    ),
    sep = "\t"
  )

  # additional information ----
  saveHTMLasMD(content$additional_information)
  removeDuplicateFiles(
    content$additional_information$file,
    main_env$save_variable$SelectDP$dp_metadata_path)

  # Output
  return(.sv)
}
