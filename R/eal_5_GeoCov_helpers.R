# Columns ====

#' @noRd
extractCoordinates <- function(main.env, coord.cols, .pattern, files.data) {
  # initialize variables -- replace by column names
  if (coord.cols == "lat") {
    coord.cols <- main.env$local.rv$columns$lat
    coord.tags <- c("N", "S")
  } else if (coord.cols == "lon") {
    coord.cols <- main.env$local.rv$columns$lon
    coord.tags <- c("E", "W")
  }
  
  # Extract proper coordinates ----
  # Get coordinates written in data file
  coordinates <- files.data[[coord.cols$file]][,coord.cols$col] |> 
    sapply(gsub, pattern = ",", replacement = ".") |> # uniformize decimal separators
    as.data.frame() |>
    setNames(coord.cols$col)
  # coord.index <- which(grepl(.pattern, coordinates))
  # Get indexes at which valid coordinates are
  coord.index <- which(
    sapply(
      1:nrow(coordinates),
      function(i) all(sapply(coordinates[i,], grepl, pattern = .pattern))
    )
  )
  # Work only with valid coordinates
  coordinates <- coordinates[coord.index,] |>
    sapply(as.numeric) |>
    as.data.frame()
  # NA removed: number of lines does not reflect the total number
  
  if (!is.data.frame(coordinates)) {
    coordinates <- data.frame(
      # id = coord.index, # different from simple line number
      coord = coordinates,
      stringsAsFactors = FALSE
    )
  }

  # Check for having only two columns
  if (dim(coordinates)[2] > 2) {
    coordinates <- coordinates[, 1:2]
  }
  # Work on two columns
  if (dim(coordinates)[2] == 1) {
    coordinates <- cbind(coordinates, coordinates)
  }
  # order from east to east (leave rows order unchanged)
  if (dim(coordinates)[2] <= 2 && dim(coordinates)[2] > 0) {
    # assign West and East / North and South to columns
    coordinates[
      coordinates[, 1] <= coordinates[, 2],
    ] <- rev(coordinates[
      coordinates[, 1] <= coordinates[, 2],
    ])
    # Set tags to columns
    colnames(coordinates) <- coord.tags # WE / SN
    if (all(coordinates[, 1] <= coordinates[, 2])) {
      colnames(coordinates) <- rev(coord.tags) # EW / NS
    }
  }
  
  return(
    cbind(
      coordinates,
      coord.index = coord.index
    )
  )
}

# Custom ====

#' @import shiny
#' 
#' @noRd
insertGeoCovInput <- function(id, main.env, default = NULL) {
  # create the UI
  new.ui <- GeoCovInputUI(id, default = default)
  # insert the UI
  insertUI(selector = "#inserthere_eal5", ui = new.ui)
  # create the server
  GeoCovInput(unns(id), main.env)
}

#' @import shiny
#' @importFrom shinyWidgets textInputAddon
#'
#' @noRd
GeoCovInputUI <- function(id, site.id, default = NULL) {
  # Variable initialization
  site.id <- NS(id, "site")

  # Get default ====
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
    # Default coordinates are a square around Europe
    def.lat <- c(35, 65)
    def.lon <- c(-25, 45)
  }

  # UI ====
  tags$div(
    id = sprintf("%s-container", id),
    fluidRow(
      column(
        11,
        shinyWidgets::textInputAddon(
          NS(id, "site_description"), 
          label = NULL,
          addon = "Description",
          value = def.site
        )
      ),
      column(
        1,
        actionButton(
          NS(id, "remove"),
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
          NS(id, "latitude"),
          tags$h4("Latitude"),
          min = -90, max = 90,
          value = unlist(def.lat),
          step = 0.01
        )
      ),
      column(
        6,
        sliderInput(
          NS(id, "longitude"),
          tags$h4("Longitude"),
          min = -180, max = 180,
          value = unlist(def.lon),
          step = 0.01
        )
      )
    ),
    fluidRow(
      column(1, "South", style = "text-align: left"),
      column(1, "North", style = "text-align: right", offset = 4),
      column(1, "West", style = "text-align: left"),
      column(1, "East", style = "text-align: right", offset = 4)
    ),
    class = "inputBox"
  )
}

#' @import shiny
#'
#' @noRd
GeoCovInput <- function(id, main.env) {
  # main.env$local.rv$custom$, rmv.id, site.id to get from main.env & id
  moduleServer(id, function(input, output, session) {
    ref <- id
    
    # Metadata acquisition ====
    local.rv <- reactiveValues(
      id = ref,
      geographicDescription = "",
      northBoundingCoordinate = 0,
      southBoundingCoordinate = 0,
      eastBoundingCoordinate = 0,
      westBoundingCoordinate = 0
    )

    # site description
    observeEvent(input$site_description, {
      req(input$site_description)
      local.rv$geographicDescription <- input$site_description
    },
    priority = 1)

    # latitude
    observeEvent(input$latitude, {
      req(input$latitude)
      local.rv$northBoundingCoordinate <- max(input$latitude)
      local.rv$southBoundingCoordinate <- min(input$latitude)
    },
    priority = 1)

    # longitude
    observeEvent(input$longitude,
      {
        req(input$longitude)
        local.rv$eastBoundingCoordinate <- max(input$longitude)
        local.rv$westBoundingCoordinate <- min(input$longitude)
      },
      priority = 1
    )

    # Metadata save ----
    observeEvent(
      {
        input$site_description
        input$latitude
        input$longitude
      },
      {
        # Fetch correct index
        ind <- match(ref, main.env$local.rv$custom$id)
        if (is.na(ind)) {
          ind <- length(main.env$local.rv$custom$id) + 1
        }

        # print values into main.env$local.rv$custom$ at selected index
        .actual.values <- listReactiveValues(local.rv)
        main.env$local.rv$custom$id <- c(main.env$local.rv$custom$id, .actual.values["id"])
        .actual.values <- .actual.values[names(.actual.values) != "id"]
        main.env$local.rv$custom$coordinates[ind, ] <- .actual.values[colnames(main.env$local.rv$custom$coordinates)]
      },
      ignoreInit = TRUE,
      priority = 0
    )

    # Remove UI ----
    observeEvent(input$remove, {
      # remove the UI
      removeUI(selector = paste0("#", NS(id, "site")), immediate = TRUE)

      # unload the UI
      ind <- match(ref, main.env$local.rv$custom$id)
      main.env$local.rv$custom$coordinates <- main.env$local.rv$custom$coordinates[-ind, ]
      main.env$local.rv$custom$id <- main.env$local.rv$custom$id[-ind]
    },
    ignoreInit = TRUE,
    label = paste("EAL5", NS(id, "remove"))
    )
  })
}