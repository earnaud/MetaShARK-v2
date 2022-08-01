# Insert Custom GeoCov Form
insertCustomGeoCov <- function(id, main_env) {
  # create the UI
  new_ui <- customGeoCov_UI(
    id,
    value = optional(listReactiveValues(main_env$local_rv$custom[[unns(id)]]))
  )
  # insert the UI
  insertUI(selector = "#inserthere_eal5", ui = new_ui, immediate = TRUE)

  # create the server
  customGeoCov(unns(id), main_env)
}

# Custom GeoCov Form UI
#' @noRd
#' @importFrom colourpicker colourInput
customGeoCov_UI <- function(id, value = NULL) {
  ns <- NS(id)

  value <- optional(
    value,
    list(
      type = "rectangle",
      description = sprintf("site %s", unns(id)),
      points = data.frame(
        id = 1:3,
        lat = c(30, 60, 45),
        lon = c(-15, 35, -15),
        stringsAsFactors = FALSE
      ),
      color = "#0000FF"
    )
  )

  tags$div(
    id = ns("box"),
    class = "inputBox",
    style = "
    max-height: 10vw !important;
    background: #ffffff57;",
    fluidRow(
      # Form column ----
      column(
        10,
        fluidRow(
          column(
            6,
            ## Site description ----
            textInput(
              ns("site_description"),
              "Site description",
              optional(value$description, "")
            ),
            ## Site type ----
            radioButtons(
              inputId = ns("type"),
              label = "Type",
              choices = c("marker", "rectangle", "polygon"),
              selected = optional(value$type, "rectangle"),
              inline = TRUE
            )
          ),
          ## Points input ----
          column(
            6,
            tags$div(
              style = "overflow-y: scroll; height: 6vw;",
              tags$div(id = sprintf("inserthere_eal5_custom_%s", unns(id))),
              actionButton(ns("new_point"), "Add point", icon("plus"))
            )
          )
        )
      ),
      # Accessory column ----
      column(
        2,
        style = "top: 2em",
        fluidRow(
          actionButton(
            ns("rmv"), "",
            icon("trash"),
            class = "danger"
          )
        ),
        fluidRow(
          colourpicker::colourInput(
            ns("color"),
            label = "Color",
            showColour = "background",
            value = value$color,
            palette = "limited"
          ) |>
            tagAppendAttributes(style = "width:40px;")
        )
      )
    )
  )
}

# Custom GeoCov Server
customGeoCov <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    # Dev ----
    observeEvent(input$devinside, {
      browser()
    })

    # Set reactive value ----
    # Do not set if already set
    main_env$local_rv$custom[[id]] <- optional(
      main_env$local_rv$custom[[id]],
      reactiveValues(
        count = 0, # number of locationInputs
        # Values
        type = "rectangle",
        description = "",
        points = data.frame(
          id = 1:3,
          lat = c(30, 60, 45),
          lon = c(-15, 35, -15),
          stringsAsFactors = FALSE
        ),
        color = "#03f"
      )
    )
    # Correct some values
    .npoints <- nrow(main_env$local_rv$custom[[id]]$points)
    # Ensure points ids match the numbers of rows
    main_env$local_rv$custom[[id]]$points$id <- 1:.npoints
    # Fix the count
    # main_env$local_rv$custom[[id]]$count <- .npoints

    # Load ====
    # !!! Initialized on click, not with main_env$EAL$page

    ## Adds as many locations input as necessary ----
    sapply(
      1:.npoints,
      function(rowid) { # enforced previously
        main_env$local_rv$custom[[id]]$
          count <- main_env$local_rv$custom[[id]]$count + 1
        
        insertCustomLocationInput(
          session$ns(as.character(rowid)), # 1:nrow
          outer_id = id,
          default = main_env$local_rv$custom[[id]]$points |>
            filter(id == rowid) |>
            select(c("lat", "lon")),
          selector = sprintf("#inserthere_eal5_custom_%s", id),
          local_rv = main_env$local_rv,
          removable = main_env$local_rv$custom[[id]]$count > 3
        )
      }
    ) # end of sapply

    ## Init visibility ----
    # Hide 2nd box if marker
    shinyjs::toggle("2-box", condition = main_env$local_rv$custom[[id]]$
      type != "marker")
    # Hide 3rd box if not polygon
    shinyjs::toggle("3-box", condition = main_env$local_rv$custom[[id]]$
      type == "polygon")
    # Hide New point box unless type is polygon
    shinyjs::toggle("new_point", condition = main_env$local_rv$custom[[id]]$
      type == "polygon")

    # Get description ====
    observeEvent(input$site_description, {
        req(main_env$EAL$page == 5)
        req(isTruthy(input$site_description))

        main_env$local_rv$custom[[id]]$description <- input$site_description
      },
      label = session$ns("description")
    )

    # Change type ====
    observeEvent(input$type, {
        req(main_env$EAL$page == 5)

        # Properly change type of spatial feature
        main_env$local_rv$custom[[id]]$type <- input$type
      },
      label = "change type"
    )

    # Hide/show divs ====
    observe({
      .type <- main_env$local_rv$custom[[id]]$type
      req(!is.null(.type))
      # hide all useless point
      ## Marker ----
      ## Hide all but 1-box (never hidden)
      if (.type == "marker") {
        sapply(
          paste0(
            main_env$local_rv$custom[[id]]$points |>
              filter(as.numeric(id) > 1) |>
              select(id) |>
              unlist(),
            "-box"
          ),
          shinyjs::hide
        )
      }

      ## Rectangle ----
      ## Hide all but 1-box (never hidden) and 2-box (show here)
      if (.type == "rectangle") {
        sapply(
          paste0(
            main_env$local_rv$custom[[id]]$points |>
              filter(as.numeric(id) > 2) |>
              select(id) |>
              unlist(),
            "-box"
          ),
          shinyjs::hide
        )
        shinyjs::show("2-box")
      }

      ## Polygon ----
      ## Show all
      if (.type == "polygon") {
        sapply(
          paste0(
            main_env$local_rv$custom[[id]]$points |>
              select(id) |>
              unlist(),
            "-box"
          ),
          shinyjs::show
        )
      }

      # Hide New point box unless type is polygon
      shinyjs::toggle("new_point", condition = main_env$local_rv$custom[[id]]$
        type == "polygon")
    })

    # Add points ====
    observeEvent(input$new_point, {
      main_env$local_rv$custom[[id]]$count <<- main_env$local_rv$custom[[id]]$
        count + 1

      insertCustomLocationInput(
        session$ns(as.character(main_env$local_rv$custom[[id]]$count)),
        outer_id = id,
        default = list(lat = 0, lon = 0),
        selector = sprintf("#inserthere_eal5_custom_%s", id),
        local_rv = main_env$local_rv
      )
    })

    # Get color ====
    observeEvent(input$color, {
      req(isTruthy(input$color))

      main_env$local_rv$custom[[id]]$color <- input$color
      
      # update widget color (badly handled for small sized widget)
      shinyjs::runjs(sprintf(
        "$('#%s')[0].setAttribute('style', 
        'background-color: %s !important; color: transparent !important;')"
      , session$ns("color"), input$color))
    })

    # Remove ====
    observeEvent(input$rmv, {
      browser()
      message(paste0("remove #", NS(id, "box")))
      # remove UI
      removeUI(selector = paste0("#", session$ns("box")), immediate = TRUE)
      # remove data
      main_env$local_rv$custom[[id]] <- NULL
      # https://github.com/rstudio/shiny/issues/2439
      .subset2(main_env$local_rv$custom, "impl")$.values$remove("1")
    })
  })
}

saveCustomGeoCov <- function(main_env) {
  .local_rv <- main_env$local_rv$custom
  .features_ids <- names(.local_rv)[names(.local_rv) != "count"]

  out <- lapply(.features_ids, function(feat_id) {
    .points <- .local_rv[[feat_id]]$points
    if (.local_rv[[feat_id]]$type == "marker") {
      .points <- .points[1, ]
    }
    if (.local_rv[[feat_id]]$type == "rectangle") {
      .points <- .points[1:2, ]
    }

    data.frame(
      geographicDescription = .local_rv[[feat_id]]$description,
      northBoundingCoordinate = max(.points$lat),
      southBoundingCoordinate = min(.points$lat),
      eastBoundingCoordinate = max(.points$lon),
      westBoundingCoordinate = min(.points$lon),
      wkt = if (.local_rv[[feat_id]]$type == "polygon") {
        .points[c(seq_row(.points), 1), 2:3] |>
          as.matrix() |>
          list() |>
          st_polygon() |>
          st_as_text()
      } else {
        ""
      }
    ) # end of data.frame
  }) |>
    bind_rows()
}