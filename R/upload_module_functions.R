#' @importFrom dataone generateIdentifier CNode MNode D1Client uploadDataPackage
#' @importFrom datapack addMember
#' @importFrom EML read_eml write_eml eml_validate
#' @importFrom mime guess_type
#' 
#' @noRd
# FIXME rework this module
uploadDP <- function(
  # connection
  endpoint,
  token,
  # content
  eml,
  data,
  scripts = NULL,
  # options
  use.doi = FALSE
) {
  options(dataone_test_token = token)
  options(dataone_token = token)
  
  # Set variables ----
  devmsg(tag = "upload", "* Sending data package to %s", endpoint$name)
  
  d1c <- dataone::D1Client(
    endpoint |>
      dplyr::select(cn) |>
      as.character(),
    endpoint |>
      dplyr::select(mn) |>
      as.character()
  )
  if (use.doi) {
    doi <- dataone::generateIdentifier(d1c@mn, "DOI")
  } # TODO check this feature
  
  # Write DP ----
  # set data package
  dp <- methods::new("DataPackage")
  
  # * metadata ----
  # Add metadata to the data package
  devmsg(tag = "upload", "* Set metadata")
  
  metadata_id <- generateIdentifier(d1c@mn, scheme = "uuid")
  doc <- EML::read_eml(eml)
  eml.format <- doc$schemaLocation |>
    gsub(
      pattern = "(eml-[0-9]+\\.[0-9]+\\.[0-9]+).+$", 
      replacement = "\\1"
    )
  metadataObj <- methods::new(
    "DataObject",
    id = metadata_id,
    format = eml.format,
    filename = eml
  )
  dp <- datapack::addMember(dp, metadataObj)
  
  # * data ----
  # Add data to the data package
  devmsg(tag = "upload", "* Set data")
  
  data.formats <- mime::guess_type(data)
  
  for(d in seq(data)) {
    dataObj <- methods::new(
      "DataObject",
      format = data.formats[d],
      filename = data[d]
    )
    dp <- datapack::addMember(dp, do = dataObj, mo = metadataObj)
  }
  
  # * scripts ----
  # Add scripts to the data package
  if (length(scripts) != 0) {
    devmsg(tag = "upload", "* Set scripts")
    
    scripts.formats <- mime::guess_type(scripts)
    
    for(d in seq(scripts)) {
      scriptObj <- methods::new(
        "DataObject",
        format = scripts.formats[d],
        filename = scripts[d]
      )
      dp <- datapack::addMember(dp, do = scriptObj, mo =metadataObj)
    }
  }
  
  # Access rules ----
  # TODO allow customized access rules
  
  # Upload ----
  devmsg(tag = "upload", "* Upload")
  
  packageId <- try(
    dataone::uploadDataPackage(
      d1c,
      dp,
      public = TRUE,
      quiet = FALSE
    )
  )
  
  if (class(packageId) == "try-error"){
    adress <- ""
    # browser()
  } else {
    adress <- paste0(
      sub("metacat.+$", "", d1c@mn@baseURL),
      "view/",
      sub("resource_map_", "", packageId)
    )
    devmsg(tag = "upload", "* Success (resource map ID: %s)", packageId)
  }
  options(dataone_test_token = NULL)
  options(dataone_token = NULL)
  
  
  return(list(
    adress = adress,
    id = metadata_id
  ))
  # return(dp)
}

#' @import shiny
#'
#' @noRd
describeWorkflowUI <- function(id, sources, targets) {
  ns <- NS(id)
  
  span(
    id = NS(id, "span"),
    div(selectInput(NS(id, "script"), "Source script", sources),
        style = "display: inline-block; vertical-align: middle;"
    ),
    "describes",
    div(selectInput(NS(id, "data"), "Target data file", targets),
        style = "display: inline-block; vertical-align: middle;"
    ),
    actionButton(NS(id, "remove"), "", icon("minus"), class = "redButton"),
    style = "display: inline-block;"
  )
}

#' @import shiny
#'
#' @noRd
describeWorkflow <- function(input, output, session) {
  ns <- session$ns
  rv <- reactiveValues()
  
  # Get
  rv$source <- reactive(input$script)
  rv$target <- reactive(input$data)
  
  # Remove
  observeEvent(input$remove, {
    removeUI(
      selector = paste0("#", NS(id, "span"))
    )
    rv <- NULL
  })
  
  # Output
  return(
    rv
  )
}
