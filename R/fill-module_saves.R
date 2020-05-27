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
    
    fwrite(
      savevar$emlal$CatVars[[file_name]],
      file_path,
      sep = "\t"
    )
  })
  
  return(savevar)
}

#' @importFrom shiny reactiveValues
.saveGeoCov <- function(savevar, rv, .write = TRUE){
  savevar$emlal$GeoCov <- if(rv$columns$complete)
      data.frame(
        geographicDescription = rv$columns$geographicDescription,
        northBoundingCoordinate = rv$columns$northBoundingCoordinate,
        southBoundingCoordinate = rv$columns$southBoundingCoordinate,
        eastBoundingCoordinate = rv$columns$eastBoundingCoordinate,
        westBoundingCoordinate = rv$columns$westBoundingCoordinate
      )
  else if(rv$custom$complete)
      data.frame(
        geographicDescription = rv$custom$geographicDescription,
        northBoundingCoordinate = rv$custom$northBoundingCoordinate,
        southBoundingCoordinate = rv$custom$southBoundingCoordinate,
        eastBoundingCoordinate = rv$custom$eastBoundingCoordinate,
        westBoundingCoordinate = rv$custom$westBoundingCoordinate
      )
  else NULL
  
  # Write data
  if (!is.null(savevar$emlal$GeoCov) && isTRUE(.write)) {
    fwrite(
      savevar$emlal$GeoCov,
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
