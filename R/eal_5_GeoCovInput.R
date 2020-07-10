#' @import shiny
#' @importFrom shinyjs hidden
#' 
#' @noRd
GeoCovInputUI <- function(id, site.id, rmv.id, default = NULL) {
  ns <- NS(id)

  div.id <- id

  if (!is.null(default)) {
    default <- as.vector(default)
    def.site <- default["geographicDescription"]
    def.lat <- c(
      default["northBoundingCoordinate"],
      default["southBoundingCoordinate"]
    )
    def.lon <- c(
      default["eastBoundingCoordinate"],
      default["westBoundingCoordinate"]
    )
  }
  else {
    def.site <- ""
    def.lat <- c(-45, 45)
    def.lon <- c(-90, 90)
  }

  tags$div(
    id = site.id,
    fluidRow(
      column(2, "Description", style = "text-align: right"),
      column(9, textInput(ns("site_description"), def.site, site.id)),
      column(
        1,
        actionButton(
          ns(rmv.id),
          "",
          icon("trash"),
          class = "danger"
        )
      )
    ),
    fluidRow(
      column(
        6,
        sliderInput(
          ns(paste0("lat_", site.id)),
          tags$h4("Latitude"),
          min = -90, max = 90,
          value = def.lat,
          step = 0.01
        ),
        fluidRow(
          column(2, "South", style = "text-align: left"),
          column(2, "North", style = "text-align: right", offset = 8)
        )
      ),
      column(
        6,
        sliderInput(
          ns(paste0("lon_", site.id)),
          tags$h4("Longitude"),
          min = -180, max = 180,
          value = def.lon,
          step = 0.01
        ),
        fluidRow(
          column(2, "West", style = "text-align: left"),
          column(2, "East", style = "text-align: right", offset = 8)
        )
      )
    ),
    class = "inputBox"
  )
}

#' @import shiny
#' @importFrom dplyr slice %>%
#' @importFrom shinyjs onclick
#' 
#' @noRd
GeoCovInput <- function(input, output, session,
                        rv, rmv.id, site.id, ref) {

  # Metadata acquisition -----------------------------------------------------
  local.rv <- reactiveValues(
    id = ref,
    geographicDescription = "",
    northBoundingCoordinate = 0,
    southBoundingCoordinate = 0,
    eastBoundingCoordinate = 0,
    westBoundingCoordinate = 0
  )

  # site description
  observeEvent(input$site_description,
    {
      req(input$site_description)
      local.rv$geographicDescription <- input$site_description
    },
    priority = 1
  )
  
  # latitude
  observeEvent(input[[paste0("lat_", site.id)]],
    {
      req(input[[paste0("lat_", site.id)]])
      local.rv$northBoundingCoordinate <- max(input[[paste0("lat_", site.id)]])
      local.rv$southBoundingCoordinate <- min(input[[paste0("lat_", site.id)]])
    },
    priority = 1
  )
  
  # longitude
  observeEvent(input[[paste0("lon_", site.id)]],
    {
      req(input[[paste0("lon_", site.id)]])
      local.rv$eastBoundingCoordinate <- max(input[[paste0("lon_", site.id)]])
      local.rv$westBoundingCoordinate <- min(input[[paste0("lon_", site.id)]])
    },
    priority = 1
  )

  # Metadata save -----------------------------------------------------
  observeEvent(
    {
      input$site_description
      input[[paste0("lat_", site.id)]]
      input[[paste0("lon_", site.id)]]
    },
    {
      # Fetch correct index
      ind <- match(ref, rv$id)
      if (is.na(ind)) {
        ind <- length(rv$id) + 1
      }

      # print values into rv at selected index
      .actual.values <- printReactiveValues(local.rv)
      rv$id <- c(rv$id, .actual.values["id"])
      .actual.values <- .actual.values[names(.actual.values) != "id"]
      rv$coordinates[ind, ] <- .actual.values[colnames(rv$coordinates)]
    },
    ignoreInit = TRUE,
    priority = 0
  )

  # Remove UI -----------------------------------------------------
  shinyjs::onclick(rmv.id, {
    # remove the UI
    removeUI(selector = paste0("#", site.id), immediate = TRUE)

    # unload the UI
    ind <- match(ref, rv$id)
    rv$coordinates <- rv$coordinates[-ind, ]
    rv$id <- rv$id[-ind]
  })

  # Output -----------------------------------------------------
  return(rv)
}

#' @importFrom dplyr bind_rows
#' @import shiny
#' 
#' @noRd
insertGeoCovInput <- function(id, rv, ns, default = NULL) {
  # !!! warning: rv = rv$custom here !!!

  # initialize IDs -----------------------------------------------------
  div.id <- id
  site.id <- paste0("site_", div.id)
  rmv.id <- paste0("rmv_", div.id)

  # Proper module server -----------------------------------------------------
  # create the UI
  new.ui <- GeoCovInputUI(ns(div.id), site.id, rmv.id, default = default)

  # insert the UI
  insertUI(selector = "#inserthere", ui = new.ui)

  # create the server
  rv <- callModule(
    GeoCovInput, div.id,
    rv, rmv.id, site.id, id
  )

  # Output -----------------------------------------------------
  return(rv)
}

#' @importFrom stringr str_extract_all
#' 
#' @noRd
extractCoordinates <- function(
  rv,
  coord.cols,
  .pattern,
  files.data
) {
  # initialize variables
  if (coord.cols == "lat") {
    coord.cols <- printReactiveValues(rv$columns$lat)
    coord.tags <- c("N", "S")
  }
  else if (coord.cols == "lon") {
    coord.cols <- printReactiveValues(rv$columns$lon)
    coord.tags <- c("E", "W")
  }

  # Extract proper coordinates
  coordinates <- files.data[[coord.cols["file"]]][[coord.cols["col"]]] %>% # uniformize decimal separators
    sapply(.,
      gsub,
      pattern = ",",
      replacement = "."
    )
  coord.index <- which(grepl(.pattern, coordinates))
  coordinates <- coordinates[coord.index] %>% 
    unname %>% 
    as.numeric

  if(!is.data.frame(coordinates))
    coordinates <- data.frame(
      coordinates, stringsAsFactors = FALSE
    )
  
  # Check for having only two columns
  if (dim(coordinates)[2] > 2) 
    coordinates <- coordinates[, 1:2]
  if (dim(coordinates)[2] == 1)
    coordinates <- cbind(coordinates, coordinates)

  if (dim(coordinates)[2] <= 2 && dim(coordinates)[2] > 0) {
    # assign West and East / North and South to columns
    coordinates[
      coordinates[, 1] <= coordinates[, 2],
    ] <- rev(coordinates[
      coordinates[, 1] <= coordinates[, 2],
    ])

    colnames(coordinates) <- coord.tags
    if (all(coordinates[, 1] <= coordinates[, 2])) {
      colnames(coordinates) <- rev(coord.tags)
    }
  }

  return(
    list(
      coordinates = coordinates,
      coord.index = coord.index
    )
  )
}
