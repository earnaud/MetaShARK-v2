#' @import shiny
#' @importFrom dplyr filter select mutate
#' @importFrom data.table fread
#' @importFrom rmarkdown pandoc_convert
#' @importFrom xml2 read_html
#' @importFrom textutils HTMLdecode
#'
#' @noRd
setLocalRV <- function(main_env) {
  # Check if templates are present if DP already exists
  if (isContentTruthy(main_env$save_variable$SelectDP$dp_metadata_path)) {
    checkTemplates(main_env)
  }

  # Set variable ====
  devmsg(tag = "setLocalRV", "set variable %s", main_env$EAL$page)
  main_env$local_rv <- switch(main_env$EAL$page,
    setupPageEAL1(main_env),
    setupPageEAL2(main_env),
    setupPageEAL3(main_env),
    setupPageEAL4(main_env),
    setupPageEAL5(main_env),
    setupPageEAL6(main_env),
    setupPageEAL7(main_env),
    setupPageEAL8(main_env),
    setupPageEAL9(main_env)
  )

  return(main_env)
}


setupPageEAL1 <- function(main_env) {
  # Build rv ====
  local_rv <- reactiveValues(
    dp_name = character(),
    dp_title = character(),
    dp_list = listDP(main_env),
    dp_license = NULL
  )

  return(local_rv)
}

setupPageEAL2 <- function(main_env) {
  .sv <- main_env$save_variable$DataFiles

  # Build rv ====
  local_rv <- reactiveValues(
    data_files = if (isContentTruthy(.sv) && any(file.exists(.sv$datapath))) {
      .ind <- which(file.exists(.sv$datapath))
      .col <- which(names(.sv) != "metadatapath")
      cbind(
        id = paste0("_", seq(.ind)),
        main_env$save_variable$DataFiles[.ind, .col]
      )
    } else {
      data.frame(stringsAsFactors = FALSE)
    },
    counter = 1
  )

  return(local_rv)
}

setupPageEAL3 <- function(main_env) {
  # Validity checks ----

  # Are data files correctly set
  if (!isContentTruthy(main_env$save_variable$DataFiles$metadatapath)) {
    stop("[savevariable_functions.R]
      Incorrect value for variable:
      main_env$save_variable$DataFiles$metadatapath")
  }

  # Preview ----
  .preview <- lapply(
    main_env$save_variable$DataFiles$datapath,
    function(.file_path) {
      table <- readDataTable(.file_path)
      out <- lapply(colnames(table), function(col) {
        .out <- table[, col][which(sapply(table[, col], isContentTruthy))]
        if (length(.out) < 5) {
          .out <- c(.out, rep("", 5 - length(.out)))
        } else {
          .out <- .out[1:5]
        }
        return(.out)
      }) |>
        setNames(nm = colnames(table))
      return(out)
    }
  ) |>
    setNames(nm = gsub(
      "(.*)\\..*$",
      "\\1.txt",
      basename(main_env$save_variable$DataFiles$datapath)
    ))

  # Custom units ----
  .custom_units <- reactiveValues(
    table = readDataTable(
      dir(
        isolate(main_env$save_variable$SelectDP$dp_metadata_path),
        pattern = "^custom_units",
        full.names = TRUE
      )
    )
  )

  # Build rv ----
  local_rv <- reactiveValues(
    md.tables = reactiveValues(),
    # checked = FALSE,
    completed = reactiveValues(),
    data.filepath = main_env$save_variable$DataFiles$datapath,
    md.filenames = basename(main_env$save_variable$DataFiles$metadatapath),
    tree_content = c(),
    preview = .preview,
    custom_units = .custom_units
  )

  # Add content ----
  lapply(
    seq(length(main_env$save_variable$DataFiles$datapath)),
    function(.ind) {

      # Shortcuts ----
      # metadata path
      .md_path <- main_env$save_variable$DataFiles$metadatapath[.ind]
      # data file names (with .txt extension)
      .rv_name <- gsub("^attributes_", "", basename(.md_path))
      # metadata table -- without misread content
      .md_table <- readDataTable(.md_path)
      .md_table[is.na(.md_table)] <- ""

      ## Attribute Definition ----
      .to.fill <- which(!sapply(
        .md_table[["attributeDefinition"]],
        isContentTruthy))
      .md_table[["attributeDefinition"]][.to.fill] <- paste(
        "Description for:", .md_table[["attributeName"]][.to.fill]
      )

      ## Curate date ----
      .date_row <- which(.md_table[["class"]] == "Date")
      # Ensure removing "!Add.*here!"
      .to_replace <- which(grepl(
        "!Add.*here!", .md_table[["dateTimeFormatString"]]))
      if (any(.to_replace)) {
        .md_table[["dateTimeFormatString"]][.to_replace] <- ""
      }

      # If any date, fill rows
      if (isTruthy(.date_row)) {
        # do not work on filled date row (except by !Add.*here!)
        .filled_date <- (
          sapply(.md_table[["dateTimeFormatString"]], isContentTruthy) &
            !sapply(.md_table[["dateTimeFormatString"]], grepl, "!Add.*here!")
        )[.date_row]
        if (any(!.filled_date)) { # if any date is not filled
          # Read 100 first rows
          .data_table <- readDataTable(
            main_env$save_variable$DataFiles$datapath[.ind],
            nrows = 100
          )
          # Guess date for date rows not filled
          .md_table[["dateTimeFormatString"]][.date_row] <- sapply(
            .date_row[!.filled_date],
            function(.row) {
              # !!! .row is attributes row and data column !!!
              .dates <- .data_table[[.row]] |> as.character()
              .date_formats <- guessDateTimeFormat(
                .dates, main_env$FORMATS$lubridate_formats
              ) |>
                convertLubridateFormat() |>
                unique()

              # If no result, default format string
              if (length(.date_formats) == 0) {
                .date_formats <- main_env$FORMATS$dates[1]
              }

              return(.date_formats[1])
            }
          )
        }
      }

      ## Curate unit ----
      .nonunit_rows <- which(.md_table[["class"]] != "numeric")
      .unit_rows <- which(.md_table[["class"]] == "numeric")
      if (isTruthy(.nonunit_rows)) {
        .md_table$unit[.nonunit_rows] <- rep("", length(.nonunit_rows))
      }
      if (isTruthy(.unit_rows)) {
        .val <- .md_table$unit[.unit_rows]
        .val[sapply(.val, function(v) {
          v == main_env$FORMATS$units$dimensionless[1] ||
            !isTruthy(v) ||
            grepl("!Ad.*ere!", v)
        })] <- main_env$FORMATS$units$dimensionless[1]
        .md_table$unit[.unit_rows] <- .val
      }
      # Add units for 'latitude' and 'longitude'
      .degree_attributes <- .md_table[["attributeName"]] %in%
        c("latitude", "longitude")
      if (any(.degree_attributes)) {
        .md_table$unit[.degree_attributes] <- "degree"
      }

      # Commit changes
      local_rv$md.tables[[.rv_name]] <<- .md_table

      # Add reactivity to each table
      makeReactiveBinding(sprintf("local_rv$md.tables$%s", .rv_name))

      # Add completed status for each attribute of each table
      local_rv$completed[[.rv_name]] <- reactiveValues()
      lapply(seq_row(.md_table), function(row.index) {
        # Set completed per row by class
        .attribute <- .md_table[["attributeName"]][row.index]
        # Just use the attribute since checking the attribute makes checking 
        # all fields
        local_rv$completed[[.rv_name]][[.attribute]] <- TRUE
      }) # end lapply:row

      # Setup tree
      local_rv$tree_content <- build_attributes_tree(main_env)
    }
  )

  # Add reactive bindings ----
  makeReactiveBinding("local_rv$custom.units$table")

  return(local_rv)
}

setupPageEAL4 <- function(main_env) {
  .cv_path <- isolate(main_env$save_variable$SelectDP$dp_metadata_path)
  .cv_files <- optional(list.files(
    .cv_path,
    pattern = "catvar",
    full.names = TRUE
  ), character())
  .current_index <- optional(as.numeric(isContentTruthy(.cv_files)), numeric())
  .current_file <- optional(basename(.cv_files[.current_index]), character())

  # Build rv ====
  local_rv <- reactiveValues(
    trigger = makeReactiveTrigger(),
    cv_tables = reactiveValues(),
    completed = reactiveValues()
  )

  sapply(.cv_files, function(.file_path) {
    # Prepare variables
    .file_name <- basename(.file_path)

    # Set each table ----
    # Read and store catvars values
    local_rv$cv.tables[[.file_name]] <- readDataTable(.file_path)

    # Curate values
    .catvar_attributes_names <- c()
    sapply(main_env$save_variable$Attributes$content, function(.md_table) {
      .catvar_attributes_names <<- c(
        .catvar_attributes_names,
        .md_table$attributeName[.md_table$class == "categorical"]
      )
    })
    local_rv$cv.tables[[.file_name]] <- local_rv$cv.tables[[.file_name]] |>
      # keep attributes actually found as categorical
      dplyr::filter(attributeName %in% .catvar_attributes_names) |>
      # Fill in automated definitions
      dplyr::mutate(
        definition = if (definition == "NA" || !isTruthy(definition)) {
          paste("Value:", code, "for attribute:", attributeName)
        } else {
          definition
        }
      ) |>
      # avoid "" in code
      dplyr::mutate(
        code = gsub("^$", "\"\"", code)
      )

    # Add reactive binding ----
    makeReactiveBinding(
      sprintf("local_rv$cv.tables$%s", .file_name)
    )

    # Set completed ----
    # Create RV
    local_rv$completed[[.file_name]] <- reactiveValues()
    # Initialize each attribute as FALSE
    .attributes <- local_rv$cv.tables[[.file_name]]$attributeName |>
      unique()
    lapply(attributes, function(attribute) {
      local_rv$completed[[.file_name]][[attribute]] <- FALSE
    })
  }) # end sapply:.cv_file

  return(local_rv)
}

setupPageEAL5 <- function(main_env) {
  # Set variables
  .att <- main_env$save_variable$Attributes$content
  .site <- list()
  .col <- list()

  sapply(names(.att), function(.md_file) {
    .data_file <- main_env$save_variable$DataFiles |>
      filter(grepl(.md_file, metadatapath)) |> # full path of attributes
      select("datapath") |> # full matching data path
      unlist() |>
      basename()
    # Set potential sites choices from attributes
    .site[[.data_file]] <<- .att[[.md_file]] |>
      dplyr::filter("class" %in% c("character", "categorical")) |>
      dplyr::select("attributeName") |>
      unlist()
    .site[[.data_file]] <<- paste(.data_file, .site[[.data_file]], sep = "/") |>
      setNames(nm = .site[[.data_file]])
    # Set columns choices
    .col[[.data_file]] <<- .att[[.md_file]] |>
      dplyr::filter(class == "numeric") |>
      dplyr::select(attributeName) |>
      unlist()
    .col[[.data_file]] <<- paste(.data_file, .col[[.data_file]], sep = "/") |>
      setNames(nm = .col[[.data_file]])
  })

  # Build rv ----
  local_rv <- reactiveValues(
    method = "columns",
    # Columns
    columns = reactiveValues(
      choices = reactiveValues(
        # file names based on Attributes templates, not Data Files
        files = names(main_env$save_variable$Attributes$content),
        sites = .sites,
        coords = .col
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
      )
    ),
    # Custom
    custom = reactiveValues(
      # units will be inserted as reactiveValues() named with numbers
      count = 0
    )
  )

  # Set saved values ----
  if (isContentTruthy(listReactiveValues(main_env$save_variable$GeoCov))) {
    if (is.null(main_env$save_variable$GeoCov$method)) {
      local_rv$method <- names(main_env$save_variable$GeoCov)
    } else {
      local_rv$method <- main_env$save_variable$GeoCov$method
    }

    ## Columns ----
    if (local_rv$method == "columns" &&
      isContentTruthy(main_env$save_variable$GeoCov$columns)) {
      .site_name <- main_env$save_variable$GeoCov$columns$site$col
      .lat_col <- main_env$save_variable$GeoCov$columns$lat$col
      .lon_col <- main_env$save_variable$GeoCov$columns$lon$col

      # Each time, only set previous values if they are matched in the data
      if (.site_name %grep% local_rv$columns$choices$sites) {
        local_rv$columns$site <- main_env$save_variable$GeoCov$columns$site
      }
      if (.lat_col %grep% local_rv$columns$choices$coords) {
        local_rv$columns$lat$col <- main_env$save_variable$GeoCov$columns$lat$col
        local_rv$columns$lat$file <- main_env$save_variable$GeoCov$columns$lat$file
      }
      if (.lon_col %grep% local_rv$columns$choices$coords) {
        local_rv$columns$lon$col <- main_env$save_variable$GeoCov$columns$lon$col
        local_rv$columns$lon$file <- main_env$save_variable$GeoCov$columns$lon$file
      }
    }

    # Custom ----
    if (local_rv$method == "custom" &&
      isContentTruthy(main_env$save_variable$GeoCov$custom)) {
      # shortcut save_variable
      saved_tables <- main_env$save_variable$GeoCov$custom

      if (isContentTruthy(saved_tables)) {
        count <- 0

        try(sapply(
          names(saved_tables)[
            !(names(saved_tables) %in% c("count", "complete"))],
          function(.row_ind) {
            row <- saved_tables[[.row_ind]]
            # Recount number of items from 1:n
            count <<- count + 1
            local_rv$custom[[as.character(count)]] <<- reactiveValues(
              # number of locationInputs
              count = nrow(row$points),
              # Values
              type = row$type,
              description = row$description,
              points = row$points,
              color = row$color
            )
          }
        )) # end try
        local_rv$custom$count <- count
      }
    }
  }

  # Add reactive bindings ----

  makeReactiveBinding("local_rv$columns")
  makeReactiveBinding("local_rv$custom")

  # Set completeness ----
  local_rv$columns$complete <- reactive(
    isTruthy(local_rv$columns$site$col) &&
      isTruthy(local_rv$columns$lat$col) &&
      isTruthy(local_rv$columns$lon$col)
  )
  local_rv$custom$complete <- reactive({
    .nm <- names(local_rv$custom)[
      !names(local_rv$custom) %in% c("complete", "count")]
    if (length(.nm) == 0) {
      FALSE
    } else {
      all(sapply(.nm, function(name) {
        isContentTruthy(local_rv$custom[[name]])
      }))
    }
  })

  return(local_rv)
}

setupPageEAL6 <- function(main_env) {
  # Set variables
  .sv <- main_env$save_variable$TaxCov

  # Build rv ----
  local_rv <- reactiveValues(
    taxa_table = character(),
    taxa.col = character(),
    taxa_name_type = character(),
    taxa_authority = character(),
    complete = FALSE
  )

  if (isTruthy(.sv$taxa_table) &&
    gsub("\\..*", "", .sv$taxa_table) %grep%
      names(main_env$save_variable$Attributes$content)) {
    local_rv$taxa_table <- unlist(.sv$taxa_table)
  }

  # Column
  # Get equivalent name for attributes
  .att_table_name <- gsub("\\..*", ".txt", local_rv$taxa_table)
  if (isTruthy(.sv$taxa_col) &&
    .sv$taxa_col %in% main_env$save_variable$Attributes$content[[
    .att_table_name]][["attributeName"]]) {
    local_rv$taxa_col <- .sv$taxa_col
  }
  # Name type
  if (isTruthy(.sv$taxa_name_type) &&
    .sv$taxa_name_type %in% c("both", "scientific", "common")) {
    local_rv$taxa_name_type <- .sv$taxa_name_type
  }
  # Authority
  if (isTruthy(.sv$taxa_authority)) {
    local_rv$taxa_authority <- .sv$taxa_authority
  }

  return(local_rv)
}

setupPageEAL7 <- function(main_env) {
  # Build rv ----
  local_rv <- reactiveValues(
    role.choices = list(
      Base = list("creator", "contact", "PI"),
      Other = list("Other")
    ),
    last.modified = 0,
    Personnel = data.frame(
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
  )

  # Set saved values ----
  saved_table <- optional(main_env$save_variable$Personnel)
  if (!is.null(saved.table)) {
    # Remove NA
    saved_table[is.na(saved_table)] <- ""
    # Add id column - with specific values for retrieved data
    saved_table$id <- paste0("_", seq_row(saved_table))
    # Save
    local_rv$Personnel <- saved_table
  }

  # Add trigger inter-roleInputs
  local_rv$trigger <- reactive({
    req(main_env$EAL$page == 7)
    main_env$local_rv$last.modified
  })

  return(local_rv)
}

setupPageEAL8 <- function(main_env) {
  # Set variables ----
  # Get keywords
  kw <- data.frame()
  .md_path <- isolate(main_env$save_variable$SelectDP$dp_metadata_path)
  if (isContentTruthy(.md_path)) {
    kw <- readDataTable(
      paste0(.md_path, "/keywords.txt")
    )
    if ("keywordThesaurus" %in% names(kw)) {
      colnames(kw)[2] <- "keywordThesaurus"
    }

    # Collapse --get by same thesaurus -- set the keyword set
    if (isContentTruthy(kw)) {
      kw <- data.frame(
        keyword = sapply(unique(kw[["keywordThesaurus"]]), function(kwt) {
          paste(kw |>
            dplyr::filter(identical(keywordThesaurus, kwt)) |>
            dplyr::select(keyword) |>
            unlist(),
          collapse = ","
          )
        }),
        keywordThesaurus = unique(kw[["keywordThesaurus"]]),
        keyword_set = paste0("_", seq(unique(kw[["keywordThesaurus"]]))),
        stringsAsFactors = FALSE,
        row.names = c()
      )
    }
  }
  if (!isContentTruthy(kw)) {
    kw <- data.frame(
      keyword = character(),
      keywordThesaurus = character(),
      keyword_set = character(),
      stringsAsFactors = FALSE
    )
  }

  # Build rv ----
  local_rv <- reactiveValues(
    # Abstract
    abstract = reactiveValues(
      content = character(),
      file = paste(
        isolate(main_env$save_variable$SelectDP$dp_metadata_path),
        "abstract.md",
        sep = "/"
      )
    ),
    # Methods
    methods = reactiveValues(
      content = character(),
      file = paste(
        isolate(main_env$save_variable$SelectDP$dp_metadata_path),
        "methods.md",
        sep = "/"
      )
    ),
    # Keywords
    keywords = kw,
    # Temporal coverage
    temporal_coverage = c(Sys.Date() - 1, Sys.Date()),
    # Additional information
    additional.information = reactiveValues(
      content = character(),
      file = paste(
        isolate(main_env$save_variable$SelectDP$dp_metadata_path),
        "additional_info.md",
        sep = "/"
      )
    )
  )

  # Set saved values ----
  # markdown files
  sapply(c("abstract", "methods", "additional.information"), function(x) {
    isolate({
      local_rv[[x]]$content <- readHTMLfromMD(local_rv[[x]]$file)
    })
  })
  # temporal coverage
  if (isContentTruthy(main_env$save_variable$Misc$temporal_coverage)) {
    local_rv$temporal_coverage <- main_env$save_variable$Misc$temporal_coverage
  }

  return(local_rv)
}

setupPageEAL9 <- function(main_env) {
  local_rv <- reactiveValues(
    eml_written = length(dir(main_env$save_variable$SelectDP$dp_eml_path)) > 0
  )

  return(local_rv)
}
