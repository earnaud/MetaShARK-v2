#' @title uploadDP
#'
#' @description automation script to upload data packages
#' produced with {dataone} and {datapack}. This function is
#' freely inspired by [this tutorial script](http://training.arcticdata.io/materials/arctic-data-center-training/programming-metadata-and-data-publishing.html#publish-data-to-the-arctic-data-center-test-site).
#'
#' @param mn Member Node URL.
#' @param cn Contributing Node URL.
#' @param token Dataone (test) token.
#' @param eml Metadata file path (XML validating NCEAS' EML Schema)
#' @param data Data files path
#' @param scripts Scripts files path
#' @param formats List of DataONE CN supported [formats](https://cn.dataone.org/cn/v2/formats)
#'
#' @importFrom dataone generateIdentifier CNode MNode D1Client uploadDataPackage
#' @importFrom datapack addMember
#' @importFrom EML read_eml write_eml eml_validate
#' @importFrom mime guess_type
uploadDP <- function(
  # essential
  mn,
  cn,
  token,
  eml,
  data,
  # facultative
  scripts = c(),
  formats,
  use.doi = FALSE
) {
  # Set variables -----------------------------------------------------

  message("Init")

  cn <- CNode(cn)
  mn <- MNode(mn)
  if (use.doi) {
    doi <- generateIdentifier(mn, "DOI")
  } # TODO check this feature

  # # Write DP -----------------------------------------------------

  # set data package
  dp <- new("DataPackage")

  message("Metadata")

  # Add metadata to the data package
  metadataObj <- new(
    "DataObject",
    # id = if(use.doi) doi else NULL,
    format = eml$format,
    filename = eml$file
  )
  dp <- addMember(dp, metadataObj)

  message("Data")

  # Add data to the data package
  dataObjs <- sapply(
    seq(data$file),
    function(d, metadataObj = metadataObj) {
      # add data object
      dataObj <- new(
        "DataObject",
        format = data$format[d],
        filename = data$file[d]
      )
      dp <- addMember(dp, dataObj, metadataObj)
      return(dataObj)
    }
  )

  message("Scripts")

  # Add scripts to the data package
  if (length(scripts) != 0) {
    progObjs <- sapply(
      seq(scripts$file),
      function(s, metadataObj = metadataObj) {
        progObj <- new(
          "DataObject",
          format = scripts$format[s],
          filename = scripts$file[s]
        )
        dp <- addMember(dp, progObj, metadataObj)

        return(progObj)
      }
    )
  }

  # # Access rules -----------------------------------------------------

  message("Access")

  accessRules <- NA # TODO allow customized access rules

  # # Upload -----------------------------------------------------

  d1c <- D1Client(cn, mn)

  message("Upload")

  options(dataone_test_token = token$test)
  options(dataone_token = token$prod)

  packageId <- try(
    uploadDataPackage(
      d1c,
      dp,
      public = TRUE,
      accessRules = accessRules,
      quiet = FALSE
    )
  )

  if(class(packageId) == "try-error") browser()
  
  options(dataone_test_token = NULL)
  options(dataone_token = NULL)

  return(packageId)
}

#' @title describeWorkflowUI
#'
#' @description UI part of the script-data workflow-describing linking module (in upload).
#'
#' @importFrom shiny NS span div selectInput actionButton icon
describeWorkflowUI <- function(id, sources, targets) {
  ns <- NS(id)

  span(
    id = ns("span"),
    div(selectInput(ns("script"), "Source script", sources),
      style = "display: inline-block; vertical-align: middle;"
    ),
    "describes",
    div(selectInput(ns("data"), "Target data file", targets),
      style = "display: inline-block; vertical-align: middle;"
    ),
    actionButton(ns("remove"), "", icon("minus"), class = "redButton"),
    style = "display: inline-block;"
  )
}

#' @describeIn  describeWorkflowUI
#'
#' @description server part of the script-data workflow-describing linking module (in upload).
#'
#' @importFrom shiny reactiveValues reactive observeEvent removeUI
describeWorkflow <- function(input, output, session) {
  ns <- session$ns
  rv <- reactiveValues()

  # Get
  rv$source <- reactive(input$script)
  rv$target <- reactive(input$data)

  # Remove
  observeEvent(input$remove, {
    removeUI(
      selector = paste0("#", ns("span"))
    )
    rv <- NULL
  })

  # Output
  return(
    rv
  )
}
