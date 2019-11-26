#' @param mn Member Node URL.
#' @param cn Contributing Node URL.
#' @param eml Metadata file (XML validating NCEAS' EML Schema)
#' @param data Data files
#' @param scripts Scripts files
#' 
#' @importFrom dataone MNode CNode D1Client
uploadDP <- function(mn, cn, eml, data, scripts = c()){
  # Set variables ----
  message("Set variables")
  # get arguments
  mn <- MNode(mn)
  cn <- CNode(cn)
  doc <- read_eml(eml)
  
  # set objects
  d1c <- new("D1Client", cn, mn)
  dp <- new("DataPackage")
  
  # set ids
  id = list(
    metadata = generateIdentifier(mn, scheme = "uuid"),
    data = c(),
    scripts = c()
  )
  
  # Edit metadata ----
  message("Edit metadata ")
  doc$packageId <- id$metadata
  doc$system <- mn@identifier
  
  # Data edit loop
  sapply(seq_along(data), function(ind){
    id$data <<- c(id$data, generateIdentifier(mn, scheme = "uuid"))
    doc$dataset$dataTable[[ind]]$physical$distribution$online$url <- paste0(mn@endpoint,
                                                                          "object/",
                                                                          id$data[ind])
  })

  # Scripts edit loop -- if at least one script
  if(isTruthy(scripts)){
    sapply(seq_along(scripts), function(ind){
      message(ind)
      id$scripts <<- c(id$scripts, generateIdentifier(mn, scheme = "uuid"))
      doc$dataset$otherEntity[[ind]]$physical$distribution$online$url <- paste0(mn@endpoint,
                                                                                "object/",
                                                                                id$scripts[ind])
    })
  }
  
  # Commit metadata
  write_eml(doc, eml)
  
  # Write datapackage ----
  message("Write data package")
  # Metadata object
  metadataObj <- new("DataObject",
                     id = id$metadata,
                     format ="eml://ecoinformatics.org/eml-2.1.1",
                     filename = eml)
  dp <- addMember(dp, metadataObj)
  
  # Data objects
  browser()
  sapply(seq_along(data), function(ind){  
    dataObj <- new("DataObject",
                   id = id$data[ind],
                   format = "text/csv",
                   filename = data[ind])
    dp <- addMember(dp, dataObj, mo = metadataObj)
  })
  
  # Scripts objects 
  message("1")
  if(isTruthy(scripts)){
    sapply(seq_along(scripts), function(ind){  
      scriptObj <- new("DataObject",
                       id = id$scripts[ind],
                       format = "application/R")
      dp <- addMember(dp, scriptObj, mo = metadataObj)
    })
  }
  message("1")
  
  # Constraints ----
  
  # Access rules ----
  
  # dev ----
  browser()
  
  # Upload ! ----
  eml_validate(doc)
  packageId <- uploadDataPackage(d1c, dp, public = TRUE)
  
}