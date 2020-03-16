#' @title uploadDP
#'
#' @description automation script to upload data packages
#' produced with {dataone} and {datapack}. This function is
#' freely inspired by [this tutorial script](http://training.arcticdata.io/materials/arctic-data-center-training/programming-metadata-and-data-publishing.html#publish-data-to-the-arctic-data-center-test-site).
#'
#' @param mn Member Node URL.
#' @param cn Contributing Node URL.
#' @param eml Metadata file path (XML validating NCEAS' EML Schema)
#' @param data Data files path
#' @param scripts Scripts files path
#' @param formats List of DataONE CN supported [formats](https://cn.dataone.org/cn/v2/formats)
#'
#' @importFrom dataone MNode CNode D1Client generateIdentifier uploadDataPackage
#' @importFrom datapack addMember
#' @importFrom EML read_eml write_eml eml_validate
#' @importFrom mime guess_type
#'
#' @export
uploadDP <- function(mn, cn, eml, data, scripts = c(), formats) {
  # Set variables -----------------------------------------------------
  message("Set variables")
  # set arguments
  mn <- MNode(mn)
  cn <- CNode(cn)
  doc <- read_eml(eml)

  # set objects
  d1c <- new("D1Client", cn, mn)
  dp <- new("DataPackage")

  # set ids
  id <- list(
    metadata = generateIdentifier(mn, scheme = "uuid"),
    data = c(),
    scripts = c()
  )

  # Edit metadata -----------------------------------------------------
  message("Editing metadata ...")
  doc$packageId <- id$metadata
  doc$dataset$maintenance$description <- "completed"
  doc$system <- mn@identifier

  # Data edit loop
  sapply(seq_along(data), function(ind) {
    id$data <<- c(id$data, generateIdentifier(mn, scheme = "uuid"))
    doc$dataset$dataTable[[ind]]$physical$distribution$online$url <- paste0(
      mn@endpoint,
      "object/",
      id$data[ind]
    )
  })

  # Scripts edit loop -- if at least one script
  if (isTruthy(scripts)) {
    sapply(seq_along(scripts), function(ind) {
      id$scripts <<- c(id$scripts, generateIdentifier(mn, scheme = "uuid"))
      doc$dataset$otherEntity[[ind]]$physical$distribution$online$url <- paste0(
        mn@endpoint,
        "object/",
        id$scripts[ind]
      )
    })
  }

  # Commit metadata
  write_eml(doc, eml)

  # Write DP -----------------------------------------------------
  message("Writing data package ...")
  # Metadata object
  metadataObj <- new("DataObject",
    id = id$metadata,
    format = "eml://ecoinformatics.org/eml-2.1.1",
    filename = eml
  )
  dp <- addMember(dp, metadataObj)

  # Data objects
  sapply(seq_along(data), function(ind) {
    this.format <- guess_type(data[ind])
    if (!this.format %in% formats) {
      message(this.format, " is not recognized")
    }
    dataObj <- new("DataObject",
      id = id$data[ind],
      format = this.format,
      filename = data[ind]
    )
    dp <- addMember(dp, dataObj, mo = metadataObj)
  })

  # Scripts objects
  if (isTruthy(scripts)) {
    sapply(seq_along(scripts), function(ind) {
      this.format <- guess_type(data[ind])
      if (!this.format %in% formats) {
        message(this.format, "is not recognized")
      }
      scriptObj <- new("DataObject",
        id = id$scripts[ind],
        format = "application/R"
      )
      dp <- addMember(dp, scriptObj, mo = metadataObj)
    })
  }

  # Constraints -----------------------------------------------------


  # Access rules -----------------------------------------------------

  # Upload -----------------------------------------------------
  eml_validate(doc)
  packageId <- uploadDataPackage(d1c, dp, public = TRUE)
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
