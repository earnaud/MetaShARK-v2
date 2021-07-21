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
  
  message("Init")
  
  cn <- dataone::CNode(cn)
  mn <- dataone::MNode(mn)
  if (use.doi) {
    doi <- dataone::generateIdentifier(mn, "DOI")
  } # TODO check this feature
  
  # # Write DP ----
  
  # set data package
  dp <- methods::new("DataPackage")
  
  message("Metadata")
  
  # Add metadata to the data package
  metadataObj <- methods::new(
    "DataObject",
    # id = if(use.doi) doi else NULL,
    format = eml$format,
    filename = eml$file
  )
  dp <- datapack::addMember(dp, metadataObj)
  
  message("Data")
  
  # Add data to the data package
  dataObjs <- sapply(
    seq(data$file),
    function(d, metadataObj = metadataObj) {
      # add data object
      dataObj <- methods::new(
        "DataObject",
        format = data$format[d],
        filename = data$file[d]
      )
      dp <- datapack::addMember(dp, dataObj, metadataObj)
      return(dataObj)
    }
  )
  
  message("Scripts")
  
  # Add scripts to the data package
  if (length(scripts) != 0) {
    progObjs <- sapply(
      seq(scripts$file),
      function(s, metadataObj = metadataObj) {
        progObj <- methods::new(
          "DataObject",
          format = scripts$format[s],
          filename = scripts$file[s]
        )
        dp <- datapack::addMember(dp, progObj, metadataObj)
        
        return(progObj)
      }
    )
  }
  
  # # Access rules ----
  
  message("Access")
  
  accessRules <- NA # TODO allow customized access rules
  
  # # Upload ----
  
  d1c <- dataone::D1Client(cn, mn)
  
  message("Upload")
  
  options(dataone_test_token = token$test)
  options(dataone_token = token$prod)
  
  packageId <- try(
    dataone::uploadDataPackage(
      d1c,
      dp,
      public = TRUE,
      accessRules = accessRules,
      quiet = FALSE
    )
  )
  
  if (class(packageId) == "try-error") browser()
  
  options(dataone_test_token = NULL)
  options(dataone_token = NULL)
  
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
