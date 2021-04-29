#' Executed after changing page 
#' 
#' @import shiny
#' @importFrom jsonlite write_json serializeJSON 
#'
#' @noRd
saveReactive <- function(main.env, page, do.template = TRUE) {
  if(is.null(main.env$local.rv))
    stop("No content provided.")
  if(is.null(main.env$save.variable))
    stop("No save variable provided")
  
  withProgress({
    isolate({
      # Save local variable content ----
      setProgress(1 / 3, "Save metadata")
      if(page != 9) {
        main.env$save.variable <- do.call(
          what = switch(
            page,
            ".saveSelectDP",
            ".saveDataFiles",
            ".saveAttributes",
            ".saveCatVars",
            ".saveGeoCov",
            ".saveTaxCov",
            ".savePersonnel",
            ".saveMisc"
          ),
          args = list(main.env)
        )
      }
      
      # Template ----
      if(isTRUE(do.template))
        templateModules(main.env, page)
      
      # Save JSON ----
      setProgress(2 / 3, "Write JSON")
      # set files + path
      path <- main.env$save.variable$SelectDP$dp.path
      filename <- main.env$save.variable$SelectDP$dp.name
      location <- paste0(path, "/", filename, ".json")
      # write files
      if (file.exists(location))
        file.remove(location)
      jsonlite::write_json(
        jsonlite::serializeJSON(
          listReactiveValues(main.env$save.variable)
        ),
        location
      )
      
      incProgress(1 / 3)
    })
  })
  
  showNotification(
    paste("Saved:", main.env$EAL$current, "."),
    duration = 2.5,
    type = "message"
  )
}

#' @noRd
.saveSelectDP <- function(main.env){
  # Shortcuts
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  
  # Save content in sv
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

#' @importFrom dplyr select
#' 
#' @noRd
.saveDataFiles <- function(main.env){
  .sv <- main.env$save.variable
  .tmp <- main.env$local.rv$data.files
  .tmp <- try(dplyr::select(.tmp, -id))
  
  # Format content
  if (!isContentTruthy(.tmp)) {
    message("Invalid content")
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
  try(main.env$local.rv$data.files$datapath <- .tmp$datapath)

  return(.sv)
}

#' @importFrom dplyr filter select %>%
#' @importFrom data.table fwrite
#' 
#' @noRd
.saveAttributes <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  # Save
  .sv$Attributes$content <- content$md.tables
  devmsg(names(content$md.tables))
    
  # Write attribute tables
  sapply(
    names(content$md.tables),
    function(tablename) {
      # write filled tables
      path <- .sv$DataFiles %>%
        dplyr::filter(grepl(tablename, metadatapath)) %>%
        dplyr::select(metadatapath) %>%
        unlist()
      table <- content$md.tables[[tablename]]
      data.table::fwrite(table, path, sep = "\t")
    }
  )
  
  # Write Custom units
  if (isContentTruthy(content$CU_Table)) {
    data.table::fwrite(
      content$CU_Table, 
      paste0(.sv$SelectDP$dp.metadata.path, "/custom_units.txt")
    )
  }

  # Add use of categorical variables (or not)
  .sv$Attributes$use.catvars <- isTRUE(content$use.catvars())
  
  return(.sv)
}

#' @importFrom data.table fwrite
#' 
#' @noRd
.saveCatVars <- function(main.env){
  content <- main.env$local.rv
  
  sapply(content$cv.files, function(file.path) {
    devmsg(file.path)
    file.name <- basename(file.path)
    
    # Save
    main.env$save.variable$CatVars[[file.name]] <- content$cv.tables[[file.name]]

    # Overwrite
    file.remove(file.path)
    data.table::fwrite(
      main.env$save.variable$CatVars[[file.name]],
      file.path,
      sep = "\t",
      na = "NA",
      quote = FALSE
    )
  })

  return(main.env$save.variable)
}

#' @noRd
#'
#' @importFrom data.table fwrite
#' @import shiny
.saveGeoCov <- function(main.env){
  .sv <- main.env$save.variable
  
  # Initialize variables
  .method <- main.env$local.rv$method

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
          df.col <- df.col[!is.na(df.col)]
          if(is.character(df.col))
            df.col <- enc2utf8(df.col)
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
  .sv$GeoCov$method <- .method

  # GeoCov written if .method filled
  if (.method == "columns" &&
      isContentTruthy(main.env$local.rv$columns$site) &&
      isContentTruthy(main.env$local.rv$columns)) {
    .sv$GeoCov$columns <- main.env$local.rv$columns

    # Site
    site <- listReactiveValues(main.env$local.rv$columns$site)
    .geographicDescription <- .values$data.content[[site$file]][[site$col]]

    # extract queried
    .tmp <- extractCoordinates(
      main.env,
      "lat",
      main.env$PATTERNS$coordinates,
      .values$data.content
    )
    .northBoundingCoordinate <- .tmp$coordinates$N
    .southBoundingCoordinate <- .tmp$coordinates$S
    .lat.index <- .tmp$coord.index

    .tmp <- extractCoordinates(
      main.env,
      "lon",
      main.env$PATTERNS$coordinates,
      .values$data.content
    )
    .eastBoundingCoordinate <- .tmp$coordinates$E
    .westBoundingCoordinate <- .tmp$coordinates$W
    .lon.index <- .tmp$coord.index

    # Get only lines fully completed
    .geographicDescription <- .geographicDescription[
      .lat.index[which(.lat.index %in% .lon.index)]
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
    .sv$GeoCov$custom <- main.env$local.rv$custom

    # fill
    geocov <- main.env$local.rv$custom$coordinates
  }
  # Write data
  if (isContentTruthy(geocov))
    data.table::fwrite(
      geocov,
      paste(
        .sv$SelectDP$dp.metadata.path,
        "geographic_coverage.txt",
        sep = "/"
      ),
      sep = "\t"
    )
  
  # Output
  return(.sv)
}

#' @noRd
.saveTaxCov <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  
  # Save
  .sv$TaxCov$taxa.table <- content$taxa.table
  .sv$TaxCov$taxa.col <- content$taxa.col
  .sv$TaxCov$taxa.name.type <- content$taxa.name.type
  .sv$TaxCov$taxa.authority <- content$taxa.authority

  main.env$save.variable <- .sv
}

#' @importFrom data.table fwrite
#' @importFrom dplyr %>% mutate
#'
#' @noRd
.savePersonnel <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  
  if(isContentTruthy(content$Personnel)) {
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
    .personnel <- personnel[NULL,]
    sapply(seq(nrow(personnel)), function(ind) {
      row <- personnel[ind,]
      
      roles <- unlist(strsplit(row$role, ","))
      lapply(seq(roles), function(ind) 
        .personnel <<- rbind(
          .personnel, 
          row %>% dplyr::mutate(role = roles[ind])
        )
      )
    })
    
    # File template for personnel
    data.table::fwrite(
      .personnel,
      paste0(
        .sv$SelectDP$dp.metadata.path,
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
#' @importFrom dplyr %>% bind_rows
#' 
#' @noRd
.saveMisc <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  
  saveHTMLasMD <- function(.content) {
    .tmp.file <- tempfile(fileext = ".html")
    htmltools::save_html(html = htmltools::HTML(.content$content), .tmp.file)
    rmarkdown::pandoc_convert(
      .tmp.file,
      from = "html",
      to = "markdown_strict",
      output = .content$file
    )
  }
  
  removeDuplicateFiles <- function(filename) {
    if(isContentTruthy(filename)){
      onlyname <- sub("\\..+", "", basename(filename))
      synonyms <- dir(
        main.env$save.variable$SelectDP$dp.metadata.path,
        pattern = onlyname,
        full.names = TRUE
      )
      file.remove(synonyms[basename(synonyms) != basename(filename)])
    }
  }
  
  # save
  .sv$Misc <- content
  
  # Fill template for abstract
  saveHTMLasMD(content$abstract)
  if(!is.character(content$abstract$file))
    content$abstract$file <- paste0(.sv$SelectDP$dp.metadata.path, "/abstract.txt")
  removeDuplicateFiles(content$abstract$file)
  
  # Fill template for methods
  saveHTMLasMD(content$methods)
  if(!is.character(content$methods$file))
    content$methods$file <- paste0(.sv$SelectDP$dp.metadata.path, "/abstract.txt")
  removeDuplicateFiles(content$methods$file)
  
  # Fill template for keywords
  .tmp <- lapply(unique(content$keywords$keyword.thesaurus), function(kwt) {
    row.ind <- which(content$keywords$keyword.thesaurus == kwt)
    
    data.frame(
      keyword = strsplit(content$keywords$keyword[row.ind], ",") %>% 
        unlist(),
      keyword.thesaurus = kwt
    )
  })
  .keywords <- dplyr::bind_rows(.tmp)
  data.table::fwrite(
    .keywords,
    paste0(
      .sv$SelectDP$dp.metadata.path,
      "/keywords.txt"
    ),
    sep = "\t"
  )
  # Fill template for additional information
  saveHTMLasMD(content$additional.information)
  if(!is.character(content$additional.information$file))
    content$additional.information$file <- paste0(.sv$SelectDP$dp.metadata.path, "/abstract.txt")
  removeDuplicateFiles(content$additional.information$file)
  
  # Remove non-md files
  sapply(c("abstract", "methods", "additional.information"), function(x) {
    file.remove(
      dir(
        main.env$save.variable$SelectDP$dp.metadata.path,
        full.names = TRUE,
        pattern = paste0("^.*", x, ".*\\.[^md]$")
      )
    )
  })
  
  # Output
  return(.sv)
}
