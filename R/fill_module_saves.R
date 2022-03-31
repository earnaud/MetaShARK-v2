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
      .tmp.save = NULL
      if(page != 9) {
        .tmp.save <- do.call(
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
      
        if(class(.tmp.save) != "try-error" || !is.null(.tmp.save)) {
          main.env$save.variable <- .tmp.save
        }
      }
      
      # Template ----
      .tmp.template <- TRUE
      if(isTRUE(do.template)) {
        .tmp.template <- templateModules(main.env, page)
      }
      
      # Error catching ----
      # If an error came out, do not go further
      if(class(.tmp.save) == "try-error" || class(.tmp.template) == "try-error") {
        browser() # not page-1 but page <- oldpage: comes back to previously visited
        isolate({main.env$EAL$page <- main.env$EAL$old.page})
        setProgress(1, "Exit save")
        stop("Error arose while saving.")
      }
      
      # Save JSON ----
      setProgress(2 / 3, "Write JSON")
      
      # set files + path
      path <- main.env$save.variable$SelectDP$dp.path
      filename <- main.env$save.variable$SelectDP$dp.name
      location <- paste0(path, "/", filename, ".json")
      
      # overwrite files
      if(dir.exists(path)) {
        if (file.exists(location))
          file.remove(location)
        jsonlite::write_json(
          jsonlite::serializeJSON(
            listReactiveValues(main.env$save.variable)
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
    paste("Saved:", main.env$EAL$current, "."),
    duration = 2.5,
    type = "message"
  )
  
  return(NULL)
}

#' @noRd
.saveSelectDP <- function(main.env){
  # Shortcuts
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  # browser()
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
  # .sv$quick <- content$quick
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
  .tmp <- try(dplyr::select(.tmp, -id), silent = TRUE)
  # Format content
  if (!isContentTruthy(.tmp) || class(.tmp) == "try-error") {
    devmsg("Invalid content.", tag="fill_module_saves.R")
    # don't change .sv
  } else {
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
    .tmp[] <- lapply(.tmp, as.character)
    
    # Save
    .sv$DataFiles <- .tmp
    try(main.env$local.rv$data.files$datapath <- .tmp$datapath)
  }

  return(.sv)
}

#' @importFrom dplyr filter select
#' @importFrom data.table fwrite
#' 
#' @noRd
.saveAttributes <- function(main.env){
  .sv <- main.env$save.variable
  content <- main.env$local.rv
  # Save
  .sv$Attributes$content <- content$md.tables
  devmsg(names(content$md.tables), tag = "save attributes")
  
  # Write attribute tables
  sapply(
    names(content$md.tables),
    function(tablename) {
      # write filled tables
      path <- .sv$DataFiles |>
        dplyr::filter(grepl(tablename, metadatapath)) |>
        dplyr::select(metadatapath) |>
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
  # .sv$Attributes$use.catvars <- isTRUE(content$use.catvars())
  
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
#' @importFrom sf st_polygon st_as_text
#' @importFrom dplyr bind_rows
#' @import shiny
.saveGeoCov <- function(main.env){
  .sv <- main.env$save.variable
  # browser()
  # Initialize variables
  .method <- main.env$local.rv$method

  data.files <- .sv$DataFiles$datapath
  data.content <- lapply(data.files, readDataTable)
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

  # Columns ----
  if (.method == "columns" &&
      isContentTruthy(main.env$local.rv$columns$site) &&
      isContentTruthy(main.env$local.rv$columns)) {
    devmsg("Geographic Coverage saved with columns", tag = "fill_module_saves.R")
    .sv$GeoCov$columns <- main.env$local.rv$columns

    # Site
    # site <- listReactiveValues(main.env$local.rv$columns$site)
    # .geographicDescription <- .values$data.content[[site$file]][[site$col]]
    # 
    # # extract queried
    # .tmp <- extractCoordinates(
    #   main.env,
    #   "lat",
    #   main.env$PATTERNS$coordinates,
    #   .values$data.content
    # )
    # .northBoundingCoordinate <- .tmp$coordinates$N
    # .southBoundingCoordinate <- .tmp$coordinates$S
    # .lat.index <- .tmp$coord.index
    # 
    # .tmp <- extractCoordinates(
    #   main.env,
    #   "lon",
    #   main.env$PATTERNS$coordinates,
    #   .values$data.content
    # )
    # .eastBoundingCoordinate <- .tmp$coordinates$E
    # .westBoundingCoordinate <- .tmp$coordinates$W
    # .lon.index <- .tmp$coord.index
    # 
    # # Get only lines fully completed
    # .index <- intersect(.lat.index, .lon.index)
    # # .northBoundingCoordinate <- .northBoundingCoordinate[.lat.index[.lat.index %in% .index]]
    # # .southBoundingCoordinate <- .southBoundingCoordinate[.lat.index[.lat.index %in% .index]]
    # # .eastBoundingCoordinate <- .eastBoundingCoordinate[.lon.index[.lon.index %in% .index]]
    # # .westBoundingCoordinate <- .westBoundingCoordinate[.lon.index[.lon.index %in% .index]]
    # .geographicDescription <- .geographicDescription[.index]
    # 
    # # Final
    # geocov <- data.frame(
    #   geographicDescription = .geographicDescription,
    #   northBoundingCoordinate = .northBoundingCoordinate,
    #   southBoundingCoordinate = .southBoundingCoordinate,
    #   eastBoundingCoordinate = .eastBoundingCoordinate,
    #   westBoundingCoordinate = .westBoundingCoordinate,
    #   stringsAsFactors = FALSE
    # )
    
    # Templated
    geocov <- NULL
  }
  
  # Custom ----
  if (.method == "custom") {
    devmsg("Geographic Coverage saved with custom", tag = "fill_module_saves.R")
    # shortcuts
    .local.rv <- main.env$local.rv$custom
    .features.ids <- names(.local.rv)[
      !names(.local.rv) %in% c("count", "complete")
    ]
    
    # build coverage table from local.rv
    .custom.coordinates <- lapply(.features.ids, function(feat.id) {
      .points <- .local.rv[[feat.id]]$points
      if(.local.rv[[feat.id]]$type == "marker")
        .points <- .points[1,]
      if(.local.rv[[feat.id]]$type == "rectangle")
        .points <- .points[1:2,]
      
      data.frame(
        geographicDescription = .local.rv[[feat.id]]$description, 
        northBoundingCoordinate = max(.points$lat),
        southBoundingCoordinate = min(.points$lat),
        eastBoundingCoordinate = max(.points$lon),
        westBoundingCoordinate = min(.points$lon),
        wkt = if(.local.rv[[feat.id]]$type == "polygon") {
          .points[c(1:nrow(.points), 1), 2:3] |>
            as.matrix() |>
            list() |>
            st_polygon() |>
            st_as_text()
          
        } else ""
      ) # end of data.frame
    }) |>
      bind_rows()
    
    # save
    .sv$GeoCov$custom <- main.env$local.rv$custom

    # fill
    geocov <- .custom.coordinates
  }
  
  # Write data
  if (isContentTruthy(geocov)){
    data.table::fwrite(
      geocov[,1:5], # do not write wkt column in geocov
      paste(
        .sv$SelectDP$dp.metadata.path,
        "geographic_coverage.txt",
        sep = "/"
      ),
      sep = "\t"
    )
    
    if("wkt" %in% names(geocov))
      data.table::fwrite(
        geocov["wkt"], # do not write wkt column in geocov
        paste(
          .sv$SelectDP$dp.metadata.path,
          ".spatial_coverage.txt",
          sep = "/"
        ),
        sep = "\t"
      )
  }
  
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
#' @importFrom dplyr mutate
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
          row |> dplyr::mutate(role = roles[ind])
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
#' @importFrom dplyr bind_rows
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
  
  # save local variable ----
  content$abstract$file <- paste0(.sv$SelectDP$dp.metadata.path, "/abstract.md")
  content$methods$file <- paste0(.sv$SelectDP$dp.metadata.path, "/methods.md")
  content$additional_information$file <- paste0(
    .sv$SelectDP$dp.metadata.path, 
    "/additional_info.md"
  )
  .sv$Misc <- content
  
  # abstract ----
  saveHTMLasMD(content$abstract)
  removeDuplicateFiles(content$abstract$file)
  
  # methods ----
  saveHTMLasMD(content$methods)
  removeDuplicateFiles(content$methods$file)
  
  # keywords ----
  # Set NA thesaurus as "" thesaurus
  content$keywords$keywordThesaurus <- replace(
    content$keywords$keywordThesaurus, 
    which(is.na(content$keywords$keywordThesaurus)),
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
      .sv$SelectDP$dp.metadata.path,
      "/keywords.txt"
    ),
    sep = "\t"
  )
  
  # additional information ----
  saveHTMLasMD(content$additional_information)
  removeDuplicateFiles(content$additional_information$file)
  
  # Remove non-md files
  # sapply(c("abstract", "methods", "additional_information"), function(x) {
  #   file.remove(
  #     dir(
  #       main.env$save.variable$SelectDP$dp.metadata.path,
  #       full.names = TRUE,
  #       pattern = paste0("^.*", x, ".*\\.[^md]$")
  #     )
  #   )
  # })
  
  # Output
  return(.sv)
}