#' @noRd
.saveSelectDP <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  
  # Save
  .sv$SelectDP$dp.name <- content$dp.name()
  .sv$SelectDP$dp.path <- paste0(
    main.env$PATHS$eal.dp,
    content$dp.name(),
    "_emldp"
  )
  .sv$SelectDP$dp.metadata.path <- paste(
    .sv$SelectDP$dp.path,
    content$dp.name(),
    "metadata_templates",
    sep = "/"
  )
  .sv$SelectDP$dp.data.path <- paste(
    .sv$SelectDP$dp.path,
    content$dp.name(),
    "data_objects",
    sep = "/"
  )
  .sv$SelectDP$dp.eml.path <- paste(
    .sv$SelectDP$dp.path,
    content$dp.name(),
    "eml",
    sep = "/"
  )
  .sv$SelectDP$dp.title <- content$dp.title()
  .sv$quick <- content$quick
  .sv$creator <- if(isTRUE(main.env$SETTINGS$logged))
    main.env$SETTINGS$user
  else
    "public" # prefered to set it properly than only with variable
  
  return(.sv)
}

#' @noRd
.saveDataFiles <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv

  # Format content
  .tmp <- isolate(content$data.files)
  if (!checkTruth(.tmp)) {
    .tmp <- data.frame(
      name = character(),
      size = character(),
      type = character(),
      datapath = character(),
      stringsAsFactors = FALSE
    )
  }
  else {
    # -- Get files data
    .from <- .tmp$datapath
    .to <- paste0(
      .sv$SelectDP$dp.data.path,
      "/", .tmp$name
    )
    file.copy(
      from = .from,
      to = .to
    )
    .tmp$datapath <- .to

    # -- set metadatapath
    .tmp$metadatapath <- paste(
      .sv$SelectDP$dp.metadata.path,
      sub(
        "(.*)\\.[a-zA-Z0-9]*$",
        "attributes_\\1.txt",
        .tmp$name
      ),
      sep = "/"
    )
  }
  .tmp[] <- lapply(.tmp, as.character)
  
  # Save
  .sv$DataFiles <- .tmp
  content$data.files$datapath <- .tmp$datapath

  return(.sv)
}

#' @noRd
#'
#' @import shiny
#' @importFrom data.table fwrite
.saveAttributes <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv

  # Save
  .sv$Attributes <- content$tables
  names(.sv$Attributes) <- .sv$DataFiles$name
  
  # Write attribute tables
  sapply(
    seq_along(content$filenames),
    function(cur_ind) {
      # write filled tables
      path <- .sv$DataFiles$metadatapath[cur_ind]
      table <- content$tables[[cur_ind]]
      data.table::fwrite(table, path, sep = "\t")
    }
  )
  
  # Write Custom units
  if (checkTruth(content$CU_Table)) {
    data.table::fwrite(
      content$CU_Table,
      paste0(.sv$SelectDP$dp.metadata.path, "/custom_units.txt")
    )
  }

  return(.sv)
}

#' @noRd
#'
#' @importFrom data.table fwrite
.saveCatVars <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  
  sapply(rv$catvarFiles, function(file_path) {
    file_name <- basename(file_path)
    
    # Save
    .sv$CatVars[[file_name]] <- content[[file_name]]$CatVars
    .tmp <- .sv$CatVars[[file_name]]$code == ""
    .sv$CatVars[[file_name]]$code[.tmp] <- "NA"

    # Overwrite
    file.remove(file_path)
    data.table::fwrite(
      .sv$CatVars[[file_name]],
      file_path,
      sep = "\t"
    )
  })

  return(.sv)
}

#' @noRd
#'
#' @importFrom data.table fwrite
#' @import shiny
.saveGeoCov <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  
  # Initialize variables
  .method <- if (isTRUE(content$columns$complete)) {
    "columns"
  } else if (isTRUE(content$custom$complete)) {
    "custom"
  } else {
    ""
  }

  data.files <- .sv$DataFiles$datapath
  data.content <- lapply(data.files, readDataTable, stringsAsFactors = FALSE)
  names(data.content) <- basename(data.files)

  # format extracted content - keep latlon-valid columns
  data.content.coordinates <- lapply(
    names(data.content),
    function(data.filename) {
      df <- data.content[[data.filename]]
      df.num <- unlist(
        lapply(df, function(df.col) {
          all(grepl(main.env$PATTERNS$coordinates, df.col))
        })
      )
      df[, df.num]
    }
  )
  names(data.content.coordinates) <- basename(data.files)

  .values <- list(
    data.content = data.content,
    data.content.coordinates = data.content.coordinates
  )

  geocov <- NULL
  .sv$GeoCov <- reactiveValues() # reset

  # GeoCov written if .method filled
  if (.method == "columns") {
    .sv$GeoCov$columns <- content$columns

    # Site
    site <- printReactiveValues(content$columns$site)
    .geographicDescription <- .values$data.content[[site["file"]]][[site["col"]]]

    # extract queried
    tmp <- extractCoordinates(
      content,
      "lat",
      main.env$PATTERNS$coordinates,
      .values$data.content
    )
    .northBoundingCoordinate <- tmp$coordinates$N
    .southBoundingCoordinate <- tmp$coordinates$S
    .latIndex <- tmp$coordIndex

    tmp <- extractCoordinates(
      content,
      "lon",
      main.env$PATTERNS$coordinates,
      .values$data.content
    )
    .eastBoundingCoordinate <- tmp$coordinates$E
    .westBoundingCoordinate <- tmp$coordinates$W
    .lonIndex <- tmp$coordIndex

    .geographicDescription <- .geographicDescription[
      .latIndex[which(.latIndex %in% .lonIndex)]
    ]

    # Final
    geocov <- data.frame(
      geographicDescription = .geographicDescription,
      northBoundingCoordinate = .northBoundingCoordinate,
      southBoundingCoordinate = .southBoundingCoordinate,
      eastBoundingCoordinate = .eastBoundingCoordinate,
      westBoundingCoordinate = .westBoundingCoordinate,
      stringsAsFactors = FALSE
    )
  } else if (.method == "custom") {
    # save
    .sv$GeoCov <- reactiveValues()
    .sv$GeoCov$custom <- content$custom

    # fill
    geocov <- content$custom$coordinates
  }
  # Write data
  if (!is.null(geocov) &&
    .method != "" &&
    all(dim(geocov) > 0)
  ) {
    data.table::fwrite(
      geocov,
      paste(
        .sv$SelectDP$dp.metadata.path,
        "geographic_coverage.txt",
        sep = "/"
      ),
      sep = "\t"
    )
  }
  
  # Output
  return(.sv)
}

#' @noRd
#'
#' @import shiny
.saveTaxCov <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  
  # Save
  .sv$TaxCov$taxa.table <- content$taxa.table
  .sv$TaxCov$taxa.col <- content$taxa.col
  .sv$TaxCov$taxa.name.type <- content$taxa.name.type
  .sv$TaxCov$taxa.authority <- content$taxa.authority

  # Output
  return(.sv)
}

#' @noRd
#'
#' @importFrom data.table fwrite
.savePersonnel <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  
  # Save
  .sv$Personnel <- content$Personnel

  # prettify
  cols <- c(
    "givenName", "middleInitial", "surName",
    "organizationName", "electronicMailAddress",
    "userId", "role",
    "projectTitle", "fundingAgency", "fundingNumber"
  )
  personnel <- content$Personnel[names(content$Personnel) %in% cols]

  # File template for personnel
  data.table::fwrite(
    personnel,
    paste0(
      .sv$SelectDP$dp.metadata.path,
      "/personnel.txt"
    ),
    sep = "\t"
  )

  return(.sv)
}

#' @noRd
#'
#' @importFrom data.table fwrite
#' @import shiny
.saveMisc <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  
  # save
  .sv$Misc <- content
  
  # Fill template for abstract
  write.text(
    content$abstract$content(),
    content$abstract$file
  )
  # Fill template for methods
  write.text(
    content$methods$content(),
    content$methods$file
  )
  # Fill template for keywords
  data.table::fwrite(
    data.frame(
      keyword = content$keywords$keyword,
      keywordThesaurus = content$keywords$keywordThesaurus
    ),
    paste0(
      .sv$SelectDP$dp.metadata.path,
      "/keywords.txt"
    ),
    sep = "\t"
  )
  # Fill template for additional information
  write.text(
    content$additional_information$content(),
    content$additional_information$file
  )
  
  return(.sv)
}
