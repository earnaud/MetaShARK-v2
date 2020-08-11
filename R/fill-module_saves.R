#' @noRd
.saveSelectDP <- function(main.env){
  content <- main.env$local.rv
  
  # Save
  main.env$save.variable$SelectDP$dp.name <- content$dp.name()
  main.env$save.variable$SelectDP$dp.path <- paste0(
    main.env$PATHS$eal.dp,
    content$dp.name(),
    "_emldp"
  )
  main.env$save.variable$SelectDP$dp.metadata.path <- paste(
    save.variable$SelectDP$dp.path,
    content$dp.name(),
    "metadata_templates",
    sep = "/"
  )
  main.env$save.variable$SelectDP$dp.data.path <- paste(
    save.variable$SelectDP$dp.path,
    content$dp.name(),
    "data_objects",
    sep = "/"
  )
  main.env$save.variable$SelectDP$dp.eml.path <- paste(
    save.variable$SelectDP$dp.path,
    content$dp.name(),
    "eml",
    sep = "/"
  )
  main.env$save.variable$SelectDP$dp.title <- content$dp.title()
  main.env$save.variable$quick <- content$quick
  main.env$save.variable$creator <- if(isTRUE(main.env$SETTINGS$logged))
    main.env$SETTINGS$user
  else
    "public" # prefered to set it properly than only with variable
}

#' @noRd
.saveDataFiles <- function(main.env){
  save.variable <- main.env$save.variable
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
      save.variable$SelectDP$dp.data.path,
      "/", .tmp$name
    )
    file.copy(
      from = .from,
      to = .to
    )
    .tmp$datapath <- .to

    # -- set metadatapath
    .tmp$metadatapath <- paste(
      save.variable$SelectDP$dp.metadata.path,
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
  save.variable$DataFiles <- .tmp
  content$data.files$datapath <- .tmp$datapath

  return(save.variable)
}

#' @noRd
#'
#' @import shiny
#' @importFrom data.table fwrite
.saveAttributes <- function(main.env){
  save.variable <- main.env$save.variable
  content <- main.env$local.rv

  # Save
  save.variable$Attributes <- content$tables
  names(save.variable$Attributes) <- save.variable$DataFiles$name
  
  # Write attribute tables
  sapply(
    seq_along(content$filenames),
    function(cur_ind) {
      # write filled tables
      path <- save.variable$DataFiles$metadatapath[cur_ind]
      table <- content$tables[[cur_ind]]
      data.table::fwrite(table, path, sep = "\t")
    }
  )
  
  # Write Custom units
  if (checkTruth(content$CU_Table)) {
    data.table::fwrite(
      content$CU_Table,
      paste0(save.variable$SelectDP$dp.metadata.path, "/custom_units.txt")
    )
  }

  return(save.variable)
}

#' @noRd
#'
#' @importFrom data.table fwrite
.saveCatVars <- function(main.env){
  save.variable <- main.env$save.variable
  content <- main.env$local.rv
  
  sapply(rv$catvarFiles, function(file_path) {
    file_name <- basename(file_path)
    
    # Save
    save.variable$CatVars[[file_name]] <- content[[file_name]]$CatVars
    .tmp <- save.variable$CatVars[[file_name]]$code == ""
    save.variable$CatVars[[file_name]]$code[.tmp] <- "NA"

    # Overwrite
    file.remove(file_path)
    data.table::fwrite(
      save.variable$CatVars[[file_name]],
      file_path,
      sep = "\t"
    )
  })

  return(save.variable)
}

#' @noRd
#'
#' @importFrom data.table fwrite
#' @import shiny
.saveGeoCov <- function(main.env){
  save.variable <- main.env$save.variable
  content <- main.env$local.rv
  
  # Initialize variables
  .method <- if (isTRUE(content$columns$complete)) {
    "columns"
  } else if (isTRUE(content$custom$complete)) {
    "custom"
  } else {
    ""
  }

  data.files <- save.variable$DataFiles$datapath
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
  save.variable$GeoCov <- reactiveValues() # reset

  # GeoCov written if .method filled
  if (.method == "columns") {
    save.variable$GeoCov$columns <- content$columns

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
    save.variable$GeoCov <- reactiveValues()
    save.variable$GeoCov$custom <- content$custom

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
        save.variable$SelectDP$dp.metadata.path,
        "geographic_coverage.txt",
        sep = "/"
      ),
      sep = "\t"
    )
  }
  
  # Output
  return(save.variable)
}

#' @noRd
#'
#' @import shiny
.saveTaxCov <- function(main.env){
  save.variable <- main.env$save.variable
  content <- main.env$local.rv
  
  # Save
  save.variable$TaxCov$taxa.table <- content$taxa.table
  save.variable$TaxCov$taxa.col <- content$taxa.col
  save.variable$TaxCov$taxa.name.type <- content$taxa.name.type
  save.variable$TaxCov$taxa.authority <- content$taxa.authority

  # Output
  return(save.variable)
}

#' @noRd
#'
#' @importFrom data.table fwrite
.savePersonnel <- function(main.env){
  save.variable <- main.env$save.variable
  content <- main.env$local.rv
  
  # Save
  save.variable$Personnel <- content$Personnel

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
      save.variable$SelectDP$dp.metadata.path,
      "/personnel.txt"
    ),
    sep = "\t"
  )

  return(save.variable)
}

#' @noRd
#'
#' @importFrom data.table fwrite
#' @import shiny
.saveMisc <- function(main.env){
  save.variable <- main.env$save.variable
  content <- main.env$local.rv
  
  # save
  save.variable$Misc <- content
  
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
      save.variable$SelectDP$dp.metadata.path,
      "/keywords.txt"
    ),
    sep = "\t"
  )
  # Fill template for additional information
  write.text(
    content$additional_information$content(),
    content$additional_information$file
  )
  
  return(save.variable)
}
