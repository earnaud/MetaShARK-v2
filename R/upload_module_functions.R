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
  use_doi = FALSE) {
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
  # Brute fix the URL
  if (!grepl("^https", d1c@mn@endpoint)) {
    d1c@mn@endpoint <- gsub("http", "https", d1c@mn@endpoint)
  }
  devmsg(d1c@mn@endpoint)

  if (use_doi) {
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
  eml_format <- doc$schemaLocation |>
    gsub(
      pattern = "(eml-[0-9]+\\.[0-9]+\\.[0-9]+).+$",
      replacement = "\\1"
    )
  metadata_obj <- methods::new(
    "data_object",
    id = metadata_id,
    format = eml_format,
    filename = eml
  )
  dp <- datapack::addMember(dp, metadata_obj)

  # * data ----
  # Add data to the data package
  devmsg(tag = "upload", "* Set data")

  data_formats <- mime::guess_type(data)

  for (d in seq(data)) {
    data_obj <- methods::new(
      "data_object",
      format = data_formats[d],
      filename = data[d]
    )
    dp <- datapack::addMember(dp, do = data_obj, mo = metadata_obj)
  }

  # * scripts ----
  # Add scripts to the data package
  if (length(scripts) != 0) {
    devmsg(tag = "upload", "* Set scripts")

    scripts_formats <- mime::guess_type(scripts)

    for (d in seq(scripts)) {
      script_obj <- methods::new(
        "data_object",
        format = scripts_formats[d],
        filename = scripts[d]
      )
      dp <- datapack::addMember(dp, do = script_obj, mo = metadata_obj)
    }
  }

  # Access rules ----
  # TODO allow customized access rules

  # Upload ----
  devmsg(tag = "upload", "* Upload")

  package_id <- try(
    dataone::uploadDataPackage(
      d1c,
      dp,
      public = TRUE,
      quiet = FALSE
    )
  )

  if (class(package_id) == "try-error") {
    adress <- ""
  } else {
    adress <- paste0(
      sub("metacat.+$", "", d1c@mn@baseURL),
      "view/",
      sub("resource_map_", "", package_id)
    )
    devmsg(tag = "upload", "* Success (resource map ID: %s)", package_id)
  }
  options(dataone_test_token = NULL)
  options(dataone_token = NULL)

  # Fix for PNDB
  adress <- ifelse(
    grepl("^https://test.pndb", adress),
    gsub("^https://test", "https://data.test", adress),
    adress
  )

  message(adress)

  return(list(
    adress = adress,
    id = metadata_id
  ))
}

#' @import shiny
#'
#' @noRd
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

#' @import shiny
#'
#' @noRd
describeWorkflow <- function(input, output, session) {
  rv <- reactiveValues()

  # Get
  rv$source <- reactive(input$script)
  rv$target <- reactive(input$data)

  # Remove
  observeEvent(input$remove, {
    removeUI(
      selector = paste0("#", session$ns("span"))
    )
    rv <- NULL
  })

  # Output
  return(
    rv
  )
}
