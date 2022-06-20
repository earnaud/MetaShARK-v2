# Columns ====

#' @noRd
extractCoordinates <- function(main_env, coord_cols, .pattern, files_data) {
  # initialize variables -- replace by column names
  if (coord_cols == "lat") {
    coord_cols <- main_env$local_rv$columns$lat
    coord_tags <- c("N", "S")
  } else if (coord_cols == "lon") {
    coord_cols <- main_env$local_rv$columns$lon
    coord_tags <- c("E", "W")
  }

  # Extract proper coordinates ----
  # Get coordinates written in data file
  coordinates <- files_data[[coord_cols$file]][, coord_cols$col] |>
    sapply(gsub, pattern = ",", replacement = ".") |>
    as.data.frame() |>
    setNames(coord_cols$col)
  # Get indexes at which valid coordinates are
  coord_index <- which(
    sapply(seq_row(coordinates), function(i) {
      all(sapply(coordinates[i, ], grepl, pattern = .pattern))
    })
  )
  # Work only with valid coordinates
  coordinates <- coordinates[coord_index, ] |>
    sapply(as.numeric) |>
    as.data.frame()
  # NA removed: number of lines does not reflect the total number

  if (!is.data.frame(coordinates)) {
    coordinates <- data.frame(
      # id = coord_index, # different from simple line number
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
    colnames(coordinates) <- coord_tags # WE or SN
    if (all(coordinates[, 1] <= coordinates[, 2])) {
      colnames(coordinates) <- rev(coord_tags) # EW or NS
    }
  }

  return(
    cbind(
      coordinates,
      coord_index = coord_index
    )
  )
}

# Custom ====

#' @import shiny
#'
#' @noRd
insertGeoCovInput <- function(id, main_env, default = NULL) {
  # create the UI
  new_ui <- GeoCovInputUI(id, default = default)
  # insert the UI
  insertUI(selector = "#inserthere_eal5", ui = new_ui)
  # create the server
  GeoCovInput(unns(id), main_env)
}

#' @import shiny
#' @importFrom shinyWidgets textInputAddon
#'
#' @noRd
GeoCovInputUI <- function(id, site_id, default = NULL) {
  # Variable initialization
  site_id <- NS(id, "site")

  # Get default ====
  if (!is.null(default)) {
    default <- as.vector(default)
    def_site <- default["geographicDescription"]
    def_lat <- c(
      default["northBoundingCoordinate"],
      default["southBoundingCoordinate"]
    )
    def_lon <- c(
      default["eastBoundingCoordinate"],
      default["westBoundingCoordinate"]
    )
  } else {
    def_site <- ""
    # Default coordinates are a square around Europe
    def_lat <- c(35, 65)
    def_lon <- c(-25, 45)
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
          value = def_site
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
          value = unlist(def_lat),
          step = 0.01
        )
      ),
      column(
        6,
        sliderInput(
          NS(id, "longitude"),
          tags$h4("Longitude"),
          min = -180, max = 180,
          value = unlist(def_lon),
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
GeoCovInput <- function(id, main_env) {
  # main_env$local_rv$custom$, rmv.id, site_id to get from main_env & id
  moduleServer(id, function(input, output, session) {
    ref <- id

    # Metadata acquisition ====
    # Naming exceptions: shall match EAL templates
    local_rv <- reactiveValues(
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
        local_rv$geographicDescription <- input$site_description
      },
      priority = 1
    )

    # latitude
    observeEvent(input$latitude, {
        req(input$latitude)
        local_rv$northBoundingCoordinate <- max(input$latitude)
        local_rv$southBoundingCoordinate <- min(input$latitude)
      },
      priority = 1
    )

    # longitude
    observeEvent(input$longitude, {
        req(input$longitude)

        local_rv$eastBoundingCoordinate <- max(input$longitude)
        local_rv$westBoundingCoordinate <- min(input$longitude)
      },
      priority = 1
    )

    # Metadata save ----
    observeEvent({
        input$site_description
        input$latitude
        input$longitude
      }, {
        # Fetch correct index
        ind <- match(ref, main_env$local_rv$custom$id)
        if (is.na(ind)) {
          ind <- length(main_env$local_rv$custom$id) + 1
        }

        # print values into main_env$local_rv$custom$ at selected index
        .actual_values <- listReactiveValues(local_rv)
        main_env$local_rv$custom$id <- c(
          main_env$local_rv$custom$id,
          .actual_values["id"]
        )
        .actual_values <- .actual_values[names(.actual_values) != "id"]
        main_env$local_rv$custom$coordinates[ind, ] <- .actual_values[
          colnames(main_env$local_rv$custom$coordinates)
        ]
      },
      ignoreInit = TRUE,
      priority = 0
    )

    # Remove UI ----
    observeEvent(input$remove, {
        # remove the UI
        removeUI(selector = paste0("#", NS(id, "site")), immediate = TRUE)

        # unload the UI
        ind <- match(ref, main_env$local_rv$custom$id)
        main_env$local_rv$custom$coordinates <- main_env$local_rv$custom$
          coordinates[-ind, ]
        main_env$local_rv$custom$id <- main_env$local_rv$custom$id[-ind]
      },
      ignoreInit = TRUE,
      label = paste("EAL5", NS(id, "remove"))
    )
  })
}
