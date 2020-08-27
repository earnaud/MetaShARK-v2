#' @import shiny
#' @importFrom shinyjs hidden
#'
#' @noRd
GeoCovUI <- function(id, main.env) {
  return(
    fluidPage(
      fluidRow(
        tags$p("You can fill in geographic coverage through two
          methods: either  by chosing columns from your file, or 
          using the custom input UI. If both are filled, the software 
          will prefer to save the columns, but on proceeding to next
          step, the choice will be yours."),
        tags$p("Make sure all of your locations are stored in a
            single file, under 3 to 5 columns: one for the site description
            and one or two others for latitude and longitude. Southern latitude
            and western longitude shall be noted with negative values."),
        bsCollapse(
          id = NS(id, "method"),
          bsCollapsePanel(
            title = "Use dataset's geographic variables",
            value = 1,
            tagList(
              uiOutput(NS(id, "site-ui")),
              uiOutput(NS(id, "latitude-ui")),
              div(
                id = NS(id, "latiWarn"),
                textOutput(NS(id, "latitude-warning"))
              ),
              uiOutput(NS(id, "longitude-ui")),
              div(
                id = NS(id, "longWarn"),
                textOutput(NS(id, "longitude-warning"))
              )
            )
          ),
          bsCollapsePanel(
            title = "Fill geographic template",
            value = 2,
            fluidRow(
              column(
                2,
                actionButton(NS(id, "addui"), "", icon("plus")),
              ),
              column(
                10,
                hidden(
                  tags$div(
                    id = NS(id, "slider_tips"),
                    HTML("You can detail a more precise number by using the 
                      left/right (or down/up) arrows of your keyboard. Precision 
                      can be given at 0.01 &#176. Items can be removed by using the
                      Delete (&#9003) key."),
                    style = "position: left"
                  )
                )
              )
            ),
            tags$div(id = "inserthere")
          ), # end BScollapse Panel 2
          multiple = FALSE
        )
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom shinyBS updateCollapse
#' @importFrom stringr str_extract_all
#' @importFrom dplyr %>%
#' @importFrom EMLassemblyline template_geographic_coverage
#' @importFrom shinyjs onclick show
#'
#' @noRd
GeoCov <- function(id, full.id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Variable initialization ----
    observe({
      req(main.env$EAL$page == 5)
      main.env$EAL$page.load$depend()
      
      req(isTruthy(main.env$save.variable$GeoCov) &&
        isTruthy(names(main.env$save.variable$GeoCov)))
      . <- isolate(main.env$save.variable$GeoCov)
      if (names(.) == "columns") {
        main.env$local.rv$columns <- .$columns
      }
      if (names(.) == "custom") {
        main.env$local.rv$custom <- .$custom
      }
    },
    label = "EAL5: set values"
    )

    # Pre-fill ----
    # * Set choices ====
    observeEvent(main.env$save.variable$Attributes,
      {
        # Get columns
        columns <- lapply(
          seq_along(main.env$save.variable$Attributes),
          function(n) {
            .tmp <- paste(
              main.env$save.variable$Attributes[[n]]$attributeName,
              main.env$save.variable$DataFiles$name[n],
              sep = "///"
            )
            names(.tmp) <- main.env$save.variable$Attributes[[n]]$attributeName
            return(.tmp)
          }
        )
        names(columns) <- main.env$save.variable$DataFiles$name

        # Extract "coordinatable" columns
        columns.coordinates <- lapply(
          seq_along(main.env$save.variable$Attributes),
          function(n) {
            .ind <- which(main.env$save.variable$Attributes[[n]]$class == "numeric")

            .tmp <- paste(
              main.env$save.variable$Attributes[[n]]$attributeName[.ind],
              main.env$save.variable$DataFiles$name[n],
              sep = "///"
            )
            names(.tmp) <- main.env$save.variable$Attributes[[n]]$attributeName[.ind]
            return(.tmp)
          }
        )
        names(columns.coordinates) <- main.env$save.variable$DataFiles$name

        .file <- main.env$local.rv$columns$choices$files

        if (.file == "all" || length(.file) > 1) {
          main.env$local.rv$columns$choices$sites <- columns
          main.env$local.rv$columns$choices$coords <- columns.coordinates
        }
        else if (length(.file) == 1) {
          main.env$local.rv$columns$choices$sites <- columns[.file]
          main.env$local.rv$columns$choices$coords <- columns.coordinates[.file]
        }
      },
      label = "EAL5 file selection"
    )

    # * Read saved values ----
    observeEvent(main.env$save.variable$GeoCov, {
      if (isTRUE(grepl("columns", names(main.env$save.variable$GeoCov)))) {
        site.name <- main.env$save.variable$GeoCov$columns$site$col
        lat.col <- main.env$save.variable$GeoCov$columns$lat$col
        lon.col <- main.env$save.variable$GeoCov$columns$lon$col

        if (any(grepl(site.name, columns))) {
          main.env$local.rv$columns$site <- main.env$save.variable$GeoCov$columns$site
        }
        if (any(grepl(lat.col, columns.coordinates))) {
          main.env$local.rv$columns$lat$col <- main.env$save.variable$GeoCov$columns$lat$col
          main.env$local.rv$columns$lat$file <- main.env$save.variable$GeoCov$columns$lat$file
        }
        if (any(grepl(lon.col, columns.coordinates))) {
          main.env$local.rv$columns$lon$col <- main.env$save.variable$GeoCov$columns$lon$col
          main.env$local.rv$columns$lon$file <- main.env$save.variable$GeoCov$columns$lon$file
        }
      }
      if (isTRUE(grepl("custom", names(main.env$save.variable$GeoCov)))) {
        saved_table <- main.env$save.variable$GeoCov$custom$coordinates
        if (all(dim(saved_table) != 0)) {
          main.env$local.rv$custom$coordinates <- saved_table
        }
      }
    },
    label = "EAL5: read saved values")

    # * File ----
    observeEvent(
      {
        main.env$local.rv$columns$site$file
        main.env$local.rv$columns$lat$file
        main.env$local.rv$columns$lon$file
      },
      {
        .files <- c(main.env$local.rv$columns$site$file, main.env$local.rv$columns$lat$file, main.env$local.rv$columns$lon$file)

        # validity check
        if (isFALSE(all(sapply(.files, isTruthy)))) {
          main.env$local.rv$columns$choices$files <- "all"
        } else {
          main.env$local.rv$columns$choices$file <- unique(.files[which(isTruthy(.files))])
        }
      },
      ignoreInit = TRUE,
      priority = 1,
      ignoreNULL = FALSE,
      label = "EAL5 init choices"
    )

    # Input management ----
    {
      # * Site description ----
      output$`site-ui` <- renderUI({
        # Set value
        if (length(main.env$local.rv$columns$site$col) > 0) {
          .site.name <- paste(
            main.env$local.rv$columns$site$col,
            main.env$local.rv$columns$site$file,
            sep = "///"
          )
          names(.site.name) <- main.env$local.rv$columns$site$col
        }
        else {
          .site.name <- c(None = "")
        }

        selectInput(
          NS(full.id, "site"),
          "Choose a column for sites:",
          c("None" = "", main.env$local.rv$columns$choices$sites),
          selected = .site.name
        )
      })

      # Get input
      observeEvent(input$site,
        {
          site.name <- input$site

          if (!isTruthy(site.name)) {
            main.env$local.rv$columns$site$col <- ""
            main.env$local.rv$columns$site$file <- ""
          }

          req(isTruthy(site.name))

          .tmp <- site.name %>%
            strsplit(., "///", TRUE) %>%
            unlist()
          main.env$local.rv$columns$site$col <- .tmp[1]
          main.env$local.rv$columns$site$file <- main.env$local.rv$columns$choices$file <- .tmp[2]
        },
        ignoreInit = TRUE,
        label = "EAL5: get site"
      )

      # * Latitude ----
      output$`latitude-ui` <- renderUI({
        isolate({
          .lat.cols <- printReactiveValues(main.env$local.rv$columns$lat)
        })

        # Init value
        if (length(.lat.cols["col"]) > 0) {
          .lat.cols <- paste(
            .lat.cols["col"],
            .lat.cols["file"],
            sep = "///"
          )
          names(.lat.cols) <- .lat.cols["col"][1]
        }
        else {
          .lat.cols <- c(None = "")
        }

        # Update UI
        selectizeInput(
          NS(full.id, "latitude"),
          "Choose a column for latitude:",
          c("None" = "", main.env$local.rv$columns$choices$coords),
          selected = .lat.cols,
          options = list(
            maxItems = 2
          )
        )
      })

      # Get input
      observeEvent(input$latitude,
        {
          latCols <- input$latitude

          if (!isTruthy(latCols)) {
            main.env$local.rv$columns$lat$col <- ""
            main.env$local.rv$columns$lat$file <- ""
          }

          req(isTruthy(latCols))

          .tmp <- latCols %>%
            strsplit(., "///", TRUE) %>%
            unlist()

          main.env$local.rv$columns$lat$col <- .tmp[1]
          main.env$local.rv$columns$lat$file <- .tmp[2]
          main.env$local.rv$columns$choices$file <- .tmp[2]
        },
        ignoreInit = TRUE,
        priority = 1,
        ignoreNULL = FALSE,
        label = "EAL5: get latitude"
      )

      # * Longitude ----
      output$`longitude-ui` <- renderUI({
        isolate({
          .lon.cols <- printReactiveValues(main.env$local.rv$columns$lon)
        })

        # Init value
        if (length(.lon.cols["col"]) > 0) {
          .lon.cols <- paste(
            .lon.cols["col"],
            .lon.cols["file"],
            sep = "///"
          )
          names(.lon.cols) <- .lon.cols["col"][1]
        }
        else {
          .lon.cols <- c(None = "")
        }

        # Update UI
        selectizeInput(
          NS(id, "longitude"),
          "Choose a column for longitude:",
          c("None" = "", main.env$local.rv$columns$choices$coords),
          selected = .lon.cols,
          options = list(
            maxItems = 2
          )
        )
      })

      # Get input
      observeEvent(input$longitude,
        {
          .lon.cols <- input$longitude

          if (!isTruthy(.lon.cols)) {
            main.env$local.rv$columns$lon$col <- ""
            main.env$local.rv$columns$lon$file <- ""
          }

          req(isTruthy(.lon.cols))

          .tmp <- .lon.cols %>%
            strsplit(., "///", TRUE) %>%
            unlist()

          main.env$local.rv$columns$lon$col <- .tmp[1]
          main.env$local.rv$columns$lon$file <- main.env$local.rv$columns$choices$file <- .tmp[2]
        },
        ignoreInit = TRUE,
        priority = 1,
        ignoreNULL = FALSE,
        label = "EAL5: get longitude"
      )
    }

    # Fill custom ----
    # * Setup ----
    observeEvent(main.env$local.rv$custom$coordinates, {
      if (dim(main.env$local.rv$custom$coordinates)[1] > 0) {
        sapply(1:nrow(main.env$local.rv$custom$coordinates), function(ind) {
          id <- -ind
          main.env$local.rv$custom <- insertGeoCovInput(
            as.numeric(id), # from -n to -1
            main.env$local.rv$custom,
            ns,
            default = main.env$local.rv$custom$coordinates[ind]
          )
        })
      }
    },
    label = "EAL5: set custom")

    # * Manage input ----
    observeEvent(input$addui, {
      show("slider_tips")
      main.env$local.rv$custom <- insertGeoCovInput(
        as.numeric(input$addui), # from 1 to n
        main.env$local.rv$custom,
        ns
      )
    },
    label = "EAL5: get custom")

    # Saves ----
    observeEvent(
      {
        main.env$local.rv$columns$site$col
        main.env$local.rv$columns$lat$col
        main.env$local.rv$columns$lon$col
      },
      {
        main.env$local.rv$columns$complete <- isTruthy(main.env$local.rv$columns$site$col) &&
          isTruthy(main.env$local.rv$columns$lat$col) &&
          isTruthy(main.env$local.rv$columns$lon$col)
      },
      label = "EAL5: set column completed",
      ignoreNULL = FALSE
    )

    observeEvent(main.env$local.rv$custom$coordinates,
      {
        main.env$local.rv$custom$complete <- isContentTruthy(main.env$local.rv$custom$coordinates)
      },
      label = "EAL5: set custom completed",
      ignoreNULL = FALSE
    )

    observe({
      req(main.env$EAL$page == 5)
      
      main.env$EAL$completed <- any(
        isTRUE(main.env$local.rv$custom$complete),
        isTRUE(main.env$local.rv$columns$complete)
      )
    },
    label = "EAL5: set completed")

    # Process data ----
    # * Previous ----
    observeEvent(main.env$EAL$.prev,
      {
        req(main.env$EAL$current == "Geographic Coverage")

        if (!"Categorical Variables" %in% main.env$EAL$history) {
          main.env$EAL$page <- main.env$EAL$page - 1
        }
      },
      label = "EAL5: process back",
      ignoreInit = TRUE
    )

    # * Next ----
    observeEvent(main.env$EAL$.next,
      {
        req(main.env$EAL$current == "Geographic Coverage")

        # Create modal
        choices <- c(
          "Columns selection" = if (main.env$local.rv$columns$complete) "columns" else NULL,
          "Custom edition" = if (main.env$local.rv$custom$complete) "custom" else NULL
        )

        req(isTruthy(choices))

        nextTabModal <- modalDialog(
          title = "Proceed Geographic Coverage",
          tagList(
            "You are getting ready to proceed. Please select one of the following:",
            radioButtons(
              NS(id, "method"),
              "Method for Geographic Coverage:",
              choices = choices
            )
          ),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(NS(id, "confirm"), "Proceed")
          )
        )

        showModal(nextTabModal)
      },
      label = "EAL5: process data",
      priority = 1,
      ignoreInit = TRUE
    )

    observeEvent(input$confirm,
      {
        removeModal()
        main.env$EAL$page <- 6
        main.env$EAL$tag.list <- tagList()

        .method <- input$method

        if (.method == "columns") {
          main.env$local.rv$custom$complete <- FALSE
        }
        if (.method == "custom") {
          main.env$local.rv$columns$complete <- FALSE
        }
        
        saveReactive(main.env)
        #   main.env$save.variable,
        #   rv = list(GeoCov = rv),
        #   main.env = main.env
        # )
      },
      label = "EAL5: confirm process data",
      ignoreInit = TRUE
    )
  })
}
