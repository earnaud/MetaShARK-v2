#' @importFrom dataone generateIdentifier CNode MNode D1Client uploadDataPackage
#' @importFrom datapack addMember
#' @importFrom EML read_eml write_eml eml_validate
#' @importFrom mime guess_type
#' 
#' @noRd
# FIXME rework this module
uploadDP <- function(
  # essential
  mn,
  cn,
  token,
  eml,
  data,
  # facultative
  scripts = c(),
  # formats,
  use.doi = FALSE
) {
  # Set variables ----
  cn <- dataone::CNode(cn)
  mn <- dataone::getMNode(cn, mn)
  if (use.doi) {
    doi <- dataone::generateIdentifier(mn, "DOI")
  } # TODO check this feature
  
  # # Write DP ----
  
  # set data package
  dp <- methods::new("DataPackage")
  
  # Add metadata to the data package
  devmsg(tag = "upload", "* Set metadata")
  
  metadataObj <- methods::new(
    "DataObject",
    # id = if(use.doi) doi else NULL,
    format = eml$format,
    filename = eml$file
  )
  dp <- datapack::addMember(dp, metadataObj)
  
  # Add data to the data package
  devmsg(tag = "upload", "* Set data")
  
  dataObjs <- sapply(
    seq(data$file),
    function(d, metadataObj = metadataObj) {
      # add data object
      dataObj <- methods::new(
        "DataObject",
        format = data$format[d],
        filename = data$file[d]
      )
      dp <<- datapack::addMember(dp, dataObj, metadataObj)
      return(dataObj)
    }
  )
  
  # Add scripts to the data package
  devmsg(tag = "upload", "* Set scripts")
  
  if (length(scripts) != 0) {
    progObjs <- sapply(
      seq(scripts$file),
      function(s, metadataObj = metadataObj) {
        progObj <- methods::new(
          "DataObject",
          format = scripts$format[s],
          filename = scripts$file[s]
        )
        dp <<- datapack::addMember(dp, progObj, metadataObj)
        
        return(progObj)
      }
    )
  }
  
  # # Access rules ----
  # devmsg(tag = "upload", "* Set access")
  
  # TODO allow customized access rules
  accessRules <- NA 
  
  # # Upload ----
  
  d1c <- dataone::D1Client(cn, mn)
  
  devmsg(tag = "upload", "* Upload")
  
  options(dataone_test_token = token)
  options(dataone_token = token)
  
  packageId <- try(
    dataone::uploadDataPackage(
      d1c,
      dp,
      public = TRUE,
      accessRules = accessRules,
      quiet = FALSE,
      packageId = paste0("urn:uuid:", uuid::UUIDgenerate())
    )
  )
  
  if (class(packageId) == "try-error")
    browser()
  
  options(dataone_test_token = NULL)
  options(dataone_token = NULL)
  
  if (class(packageId) != "try-error")
    devmsg(tag = "upload", "* Success: %s", packageId)
  
  return(packageId)
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
