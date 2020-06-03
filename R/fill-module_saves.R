#'
.saveDataFiles <- function(savevar, rv){
  tmp <- savevar$emlal$DataFiles
  
  if(!checkTruth(tmp))
    tmp <- data.frame(
      name = character(),
      size = character(),
      type = character(),
      datapath = character()
    )
  
  # -- Get files data
  tmp$datapath <- paste0(
    savevar$emlal$SelectDP$dp_data_path,
    "/", rv$data_files$name
  )
  
  # -- set metadatapath
  tmp$metadatapath <- paste(
    savevar$emlal$SelectDP$dp_metadata_path,
    sub(
      "(.*)\\.[a-zA-Z0-9]*$",
      "attributes_\\1.txt",
      rv$data_files$name
    ),
    sep = "/"
  )
  
  # Set table name
  tmp$table_name <- rv$data_files$table_name
  # Set description
  tmp$description <- rv$data_files$description
  # Set URL
  tmp$url <- rv$data_files$url
  
  savevar$emlal$DataFiles <- tmp
  
  # copy files
  file.copy(
    from = rv$data_files$datapath,
    to = savevar$emlal$SelectDP$dp_data_path
  )
  
  rv$data_files$datapath <-paste0(
    savevar$emlal$SelectDP$dp_data_path,
    "/", rv$data_files$name
  )
  return(savevar)
}

#' @importFrom shiny withProgress incProgress reactiveValues
#' @importFrom data.table fwrite
.saveAttributes <- function(savevar, rv){
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
  if(checkTruth(rv$CU_Table))
    fwrite(
      rv$CU_Table,
      paste0(savevar$emlal$SelectDP$dp_metadata_path, "/custom_units.txt")
    )
  
  return(savevar)
}

#' @importFrom data.table fwrite
.saveCatVars <- function(savevar, rv){
  sapply(rv$catvarFiles, function(file_path){
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

#' @importFrom data.table fwrite
.saveGeoCov <- function(
  savevar, rv, .method = "columns", .values, globals){
  
  geocov <- data.frame(
    geographicDescription = character(),
    northBoundingCoordinate = numeric(),
    southBoundingCoordinate = numeric(),
    eastBoundingCoordinate = numeric(),
    westBoundingCoordinate = numeric()
  )
  
  # GeoCov written if .method filled
  if(.method == "columns"){
    # reset
    savevar$emlal$GeoCov <- reactiveValues()
    savevar$emlal$GeoCov$columns <- rv$columns
    
    # Site
    site <- rv$columns$site.name
    .geographicDescription <- .values$data.content[[site["file"]]][[site["col"]]]
    
    # extract queried
    tmp <- extractCoordinates(
      rv,
      "lat",
      globals$PATTERNS$LATLON, 
      .values$data.content
    )
    .northBoundingCoordinate <- tmp$coordinates$N
    .southBoundingCoordinate <- tmp$coordinates$S
    rv$warnings$latWarnings <- tmp$warnings
    
    tmp <- extractCoordinates(
      rv,
      "lon",
      globals$PATTERNS$LATLON, 
      .values$data.content
    )
    .eastBoundingCoordinate <- tmp$coordinates$E
    .westBoundingCoordinate <- tmp$coordinates$W
    rv$warnings$lonWarnings <- tmp$warnings
    
    # Final
    geocov <- data.frame(
      geographicDescription = .geographicDescription,
      northBoundingCoordinate = .northBoundingCoordinate,
      southBoundingCoordinate = .southBoundingCoordinate,
      eastBoundingCoordinate = .eastBoundingCoordinate,
      westBoundingCoordinate = .westBoundingCoordinate,
      stringsAsFactors = FALSE
    )
  }
  else if(.method == "custom"){
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
        savevar$emlal$SelectDP$dp_metadata_path,
        "geographic_coverage.txt",
        sep = "/"
      ),
      sep = "\t"
    )
  }
  
  return(savevar)
}

#' @importFrom shiny reactiveValues
.saveTaxCov <- function(savevar, rv){
  savevar$emlal$TaxCov <- reactiveValues(
    taxa.table = rv$taxa.table,
    taxa.col = rv$taxa.col,
    taxa.name.type = rv$taxa.name.type,
    taxa.authority = rv$taxa.authority
  )
  
  return(savevar)
}

#' @importFrom data.table fwrite
.savePersonnel <- function(savevar, rv){
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
      savevar$emlal$SelectDP$dp_metadata_path,
      "/personnel.txt"
    ), 
    sep = "\t"
  )
  
  return(savevar)
}

#' @importFrom data.table fwrite
#' @importFrom shiny withProgress setProgress incProgress
.saveMisc <- function(savevar, rv){
  withProgress({
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
        savevar$emlal$SelectDP$dp_metadata_path,
        "/keywords.txt"
      ),
      sep = "\t"
    )
    
    setProgress(
      value = 0.99,
      "Writing 'additional_info.txt'."
    )
    write.text(
      rv$additional_info$content(),
      rv$additional_info$file
    )
    
    incProgress(0.01)
  },
    message = "Processing Miscellaneous.")
  
  return(savevar)
}
