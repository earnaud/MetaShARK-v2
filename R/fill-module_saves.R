#' @noRd
.saveDataFiles <- function(save.variable, rv) {
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
      save.variable$emlal$SelectDP$dp.data.path,
      "/", .tmp$name
    )
    file.copy(
      from = .from,
      to = .to
    )
    .tmp$datapath <- .to
  
    # -- set metadatapath
    .tmp$metadatapath <- paste(
      save.variable$emlal$SelectDP$dp.metadata.path,
      sub(
        "(.*)\\.[a-zA-Z0-9]*$",
        "attributes_\\1.txt",
        .tmp$name
      ),
      sep = "/"
    )
  }
  
  .tmp[] <- lapply(.tmp, as.character)
  save.variable$emlal$DataFiles <- .tmp
  rv$data.files$datapath <- .tmp$datapath
  
  return(save.variable)
}

#' @noRd
#'
#' @import shiny
#' @importFrom data.table fwrite
.saveAttributes <- function(save.variable, rv) {
  # Write attribute tables
  sapply(
    seq_along(rv$filenames),
    function(cur_ind) {
      # write filled tables
      path <- save.variable$emlal$DataFiles$metadatapath[cur_ind]
      table <- rv$tables[[cur_ind]]
      data.table::fwrite(table, path, sep = "\t")
    }
  )

  # Write Custom units
  if (checkTruth(rv$CU_Table)) {
    data.table::fwrite(
      rv$CU_Table,
      paste0(save.variable$emlal$SelectDP$dp.metadata.path, "/custom_units.txt")
    )
  }

  # Save
  save.variable$emlal$Attributes <- rv$tables
  names(save.variable$emlal$Attributes) <- savevar$emlal$DataFiles$name

  return(save.variable)
}

#' @noRd
#' 
#' @importFrom data.table fwrite
.saveCatVars <- function(save.variable, rv) {
  sapply(rv$catvarFiles, function(file_path) {
    file_name <- basename(file_path)
    save.variable$emlal$CatVars[[file_name]] <- rv[[file_name]]$CatVars

    .tmp <- save.variable$emlal$CatVars[[file_name]]$code == ""
    save.variable$emlal$CatVars[[file_name]]$code[.tmp] <- "NA"

    file.remove(file_path)

    data.table::fwrite(
      save.variable$emlal$CatVars[[file_name]],
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
.saveGeoCov <- function(save.variable, rv, main.env) {
  # Initialize variables
  .method <- if (isTRUE(rv$columns$complete)) {
    "columns"
  } else if (isTRUE(rv$custom$complete)) {
    "custom"
  } else {
    ""
  }

  data.files <- save.variable$emlal$DataFiles$datapath
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
    save.variable$emlal$GeoCov <- reactiveValues() # reset
    save.variable$emlal$GeoCov$columns <- rv$columns

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
    save.variable$emlal$GeoCov <- reactiveValues()
    save.variable$emlal$GeoCov$custom <- rv$custom

    # fill
    geocov <- rv$custom$coordinates
  }
  # Write data
  if (!is.null(geocov) &&
    .method != "" &&
    all(dim(geocov) > 0)
  ) {
    data.table::fwrite(
      geocov,
      paste(
        save.variable$emlal$SelectDP$dp.metadata.path,
        "geographic_coverage.txt",
        sep = "/"
      ),
      sep = "\t"
    )
  }

  return(save.variable)
}

#' @noRd
#' 
#' @import shiny
.saveTaxCov <- function(save.variable, rv) {
  save.variable$emlal$TaxCov <- reactiveValues(
    taxa.table = rv$taxa.table,
    taxa.col = rv$taxa.col,
    taxa.name.type = rv$taxa.name.type,
    taxa.authority = rv$taxa.authority
  )

  return(save.variable)
}

#' @noRd
#' 
#' @importFrom data.table fwrite
.savePersonnel <- function(save.variable, rv) {
  # save
  save.variable$emlal$Personnel <- rv$Personnel

  # prettify
  cols <- c(
    "givenName", "middleInitial", "surName",
    "organizationName", "electronicMailAddress",
    "userId", "role",
    "projectTitle", "fundingAgency", "fundingNumber"
  )
  personnel <- rv$Personnel[names(rv$Personnel) %in% cols]

  # write file
  data.table::fwrite(
    personnel,
    paste0(
      save.variable$emlal$SelectDP$dp.metadata.path,
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
.saveMisc <- function(save.variable, rv) {
  withProgress(
    {
      save.variable$emlal$Misc <- rv

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
      data.table::fwrite(
        data.frame(
          keyword = rv$keywords$keyword,
          keywordThesaurus = rv$keywords$keywordThesaurus
        ),
        paste0(
          save.variable$emlal$SelectDP$dp.metadata.path,
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

  return(save.variable)
}
