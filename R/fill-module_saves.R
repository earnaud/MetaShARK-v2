#' @noRd
.saveDataFiles <- function(savevar, rv) {
  .tmp <- isolate(rv$data.files)
  
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
      savevar$emlal$SelectDP$dp.data.path,
      "/", .tmp$name
    )
    file.copy(
      from = .from,
      to = .to
    )
    .tmp$datapath <- .to
  
    # -- set metadatapath
    .tmp$metadatapath <- paste(
      savevar$emlal$SelectDP$dp.metadata.path,
      sub(
        "(.*)\\.[a-zA-Z0-9]*$",
        "attributes_\\1.txt",
        .tmp$name
      ),
      sep = "/"
    )
  }
  
  .tmp[] <- lapply(.tmp, as.character)
  savevar$emlal$DataFiles <- .tmp
  rv$data.files$datapath <- .tmp$datapath
  
  return(savevar)
}

#' @noRd
#'
#' @import shiny
#' @importFrom data.table fwrite
.saveAttributes <- function(savevar, rv) {
  # Write attribute tables
  sapply(
    seq_along(rv$filenames),
    function(cur_ind) {
      # write filled tables
      path <- savevar$emlal$DataFiles$metadatapath[cur_ind]
      table <- rv$tables[[cur_ind]]
      fwrite(table, path, sep = "\t")
    }
  )

  # Write Custom units
  if (checkTruth(rv$CU_Table)) {
    fwrite(
      rv$CU_Table,
      paste0(savevar$emlal$SelectDP$dp.metadata.path, "/custom_units.txt")
    )
  }

  # Save
  savevar$emlal$Attributes <- rv$tables
  names(savevar$emlal$Attributes) <- savevar$emlal$DataFiles$name
  # if (rv$annotations$count > 0)
  #   savevar$emlal$Attributes$annotations <- rv$annotations$values

  return(savevar)
}

#' @noRd
#' 
#' @importFrom data.table fwrite
.saveCatVars <- function(savevar, rv) {
  sapply(rv$catvarFiles, function(file_path) {
    file_name <- basename(file_path)
    savevar$emlal$CatVars[[file_name]] <- rv[[file_name]]$CatVars

    .tmp <- savevar$emlal$CatVars[[file_name]]$code == ""
    savevar$emlal$CatVars[[file_name]]$code[.tmp] <- "NA"

    file.remove(file_path)

    fwrite(
      savevar$emlal$CatVars[[file_name]],
      file_path,
      sep = "\t"
    )
  })

  return(savevar)
}

#' @noRd
#' 
#' @importFrom data.table fwrite
#' @import shiny
.saveGeoCov <- function(savevar, rv, main.env) {
  # Initialize variables
  .method <- if (isTRUE(rv$columns$complete)) {
    "columns"
  } else if (isTRUE(rv$custom$complete)) {
    "custom"
  } else {
    ""
  }

  data.files <- savevar$emlal$DataFiles$datapath
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
  
  # GeoCov written if .method filled
  if (.method == "columns") {
    savevar$emlal$GeoCov <- reactiveValues() # reset
    savevar$emlal$GeoCov$columns <- rv$columns

    # Site
    site <- printReactiveValues(rv$columns$site)
    .geographicDescription <- .values$data.content[[site["file"]]][[site["col"]]]

    # extract queried
    tmp <- extractCoordinates(
      rv,
      "lat",
      main.env$PATTERNS$coordinates,
      .values$data.content
    )
    .northBoundingCoordinate <- tmp$coordinates$N
    .southBoundingCoordinate <- tmp$coordinates$S
    .latIndex <- tmp$coordIndex

    tmp <- extractCoordinates(
      rv,
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
    savevar$emlal$GeoCov <- reactiveValues()
    savevar$emlal$GeoCov$custom <- rv$custom

    # fill
    geocov <- rv$custom$coordinates
  }
  # Write data
  if (!is.null(geocov) &&
    .method != "" &&
    all(dim(geocov) > 0)
  ) {
    fwrite(
      geocov,
      paste(
        savevar$emlal$SelectDP$dp.metadata.path,
        "geographic_coverage.txt",
        sep = "/"
      ),
      sep = "\t"
    )
  }

  return(savevar)
}

#' @noRd
#' 
#' @import shiny
.saveTaxCov <- function(savevar, rv) {
  savevar$emlal$TaxCov <- reactiveValues(
    taxa.table = rv$taxa.table,
    taxa.col = rv$taxa.col,
    taxa.name.type = rv$taxa.name.type,
    taxa.authority = rv$taxa.authority
  )

  return(savevar)
}

#' @noRd
#' 
#' @importFrom data.table fwrite
.savePersonnel <- function(savevar, rv) {
  # save
  savevar$emlal$Personnel <- rv$Personnel

  # prettify
  cols <- c(
    "givenName", "middleInitial", "surName",
    "organizationName", "electronicMailAddress",
    "userId", "role",
    "projectTitle", "fundingAgency", "fundingNumber"
  )
  personnel <- rv$Personnel[names(rv$Personnel) %in% cols]

  # write file
  fwrite(
    personnel,
    paste0(
      savevar$emlal$SelectDP$dp.metadata.path,
      "/personnel.txt"
    ),
    sep = "\t"
  )

  return(savevar)
}

#' @noRd
#' 
#' @importFrom data.table fwrite
#' @import shiny
.saveMisc <- function(savevar, rv) {
  withProgress(
    {
      savevar$emlal$Misc <- rv

      setProgress(
        value = 0.25,
        message = "Writing 'abstract.txt'."
      )
      write.text(
        rv$abstract$content(),
        rv$abstract$file
      )

      setProgress(
        value = 0.5,
        "Writing 'methods.txt'."
      )
      write.text(
        rv$methods$content(),
        rv$methods$file
      )

      setProgress(
        value = 0.75,
        "Writing 'keywords.txt'."
      )
      fwrite(
        data.frame(
          keyword = rv$keywords$keyword,
          keywordThesaurus = rv$keywords$keywordThesaurus
        ),
        paste0(
          savevar$emlal$SelectDP$dp.metadata.path,
          "/keywords.txt"
        ),
        sep = "\t"
      )

      setProgress(
        value = 0.99,
        "Writing 'additional_info.txt'."
      )
      write.text(
        rv$additional_information$content(),
        rv$additional_information$file
      )

      incProgress(0.01)
    },
    message = "Processing Miscellaneous."
  )

  return(savevar)
}
