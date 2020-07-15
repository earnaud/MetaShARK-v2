#' @import shiny
#' @importFrom shinyjs hidden
#' 
#' @noRd
GeoCovUI <- function(id, main.env) {
  ns <- NS(id)

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
GeoCov <- function(id, main.env) {
  moduleServer(id, function(input, output, session){
    save.variable <- main.env$save.variable
  ns <- session$ns
  
  # Variable initialization ----

  # Reactive Values
  rv <- reactiveValues(
    columns = reactiveValues(
      choices = reactiveValues(
        files = "all",
        sites = NA_character_,
        coords = NA_character_
      ),
      site = reactiveValues(
        col = character(),
        file = character()
      ),
      lat = reactiveValues(
        col = character(),
        file = character()
      ),
      lon = reactiveValues(
        col = character(),
        file = character()
      ),
      complete = FALSE
    ),
    custom = reactiveValues(
      id = numeric(),
      coordinates = data.frame(
        geographicDescription = character(),
        northBoundingCoordinate = numeric(),
        southBoundingCoordinate = numeric(),
        eastBoundingCoordinate = numeric(),
        westBoundingCoordinate = numeric(),
        stringsAsFactors = FALSE
      ),
      complete = FALSE
    )
  )
  if(isTruthy(save.variable$emlal$GeoCov) &&
      isTruthy(names(save.variable$emlal$GeoCov))){
    . <- isolate(save.variable$emlal$GeoCov)
    if(names(.) == "columns"){
      rv$columns <- .$columns
    }
    if(names(.) == "custom"){
      rv$custom <- .$custom
    }
  }

  # Pre-fill ----
  # * Set choices ====
  columns <- lapply(
    seq_along(save.variable$emlal$Attributes),
    function(n) {
      .tmp <- paste(
        save.variable$emlal$Attributes[[n]]$attributeName,
        save.variable$emlal$DataFiles$name[n],
        sep = "///"
      )
      names(.tmp) <- save.variable$emlal$Attributes[[n]]$attributeName
      return(.tmp)
    }
  )
  names(columns) <- save.variable$emlal$DataFiles$name
  
  columns.coordinates <- lapply(
    seq_along(save.variable$emlal$Attributes),
    function(n) {
      .ind <- which(save.variable$emlal$Attributes[[n]]$class == "numeric")
      
      .tmp <- paste(
        save.variable$emlal$Attributes[[n]]$attributeName[.ind],
        save.variable$emlal$DataFiles$name[n],
        sep = "///"
      )
      names(.tmp) <- save.variable$emlal$Attributes[[n]]$attributeName[.ind]
      return(.tmp)
    }
  )
  names(columns.coordinates) <- save.variable$emlal$DataFiles$name

  observeEvent(rv$columns$choices$files, 
    {
      .file <- rv$columns$choices$files
      
      if(.file == "all" || length(.file) > 1){
        rv$columns$choices$sites <- columns
        rv$columns$choices$coords <- columns.coordinates
      }
      else if(length(.file) == 1){
        rv$columns$choices$sites <- columns[.file]
        rv$columns$choices$coords <- columns.coordinates[.file]
      }
    },
    label = "EAL5 file selection"
  )
  
  # * Read saved values ----
  if (isTRUE(grepl("columns", names(save.variable$emlal$GeoCov)))) {
    site.name <- save.variable$emlal$GeoCov$columns$site$col
    lat.col <- save.variable$emlal$GeoCov$columns$lat$col
    lon.col <- save.variable$emlal$GeoCov$columns$lon$col
    
    if (any(grepl(site.name, columns)))
      rv$columns$site <- save.variable$emlal$GeoCov$columns$site
    if (any(grepl(lat.col, columns.coordinates))){
      rv$columns$lat$col <- save.variable$emlal$GeoCov$columns$lat$col
      rv$columns$lat$file <- save.variable$emlal$GeoCov$columns$lat$file
    }
    if (any(grepl(lon.col, columns.coordinates))){
      rv$columns$lon$col <- save.variable$emlal$GeoCov$columns$lon$col
      rv$columns$lon$file <- save.variable$emlal$GeoCov$columns$lon$file
    }
  }
  if (isTRUE(grepl("custom", names(save.variable$emlal$GeoCov)))) {
    saved_table <- save.variable$emlal$GeoCov$custom$coordinates
    if (all(dim(saved_table) != 0)) {
      rv$custom$coordinates <- saved_table
    }
  }

  # * File ----
  observeEvent(
    {
      rv$columns$site$file
      rv$columns$lat$file
      rv$columns$lon$file
    },
    {
      .files <- c(rv$columns$site$file, rv$columns$lat$file, rv$columns$lon$file)

      # validity check
      if (isFALSE(all(sapply(.files, isTruthy))))
        rv$columns$choices$files <- "all"
      else
        rv$columns$choices$file <- unique(.files[which(isTruthy(.files))])
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
      if (length(rv$columns$site$col) > 0) {
        .site.name <- paste(
          rv$columns$site$col,
          rv$columns$site$file,
          sep = "///"
        )
        names(.site.name) <- rv$columns$site$col
      }
      else {
        .site.name <- c(None = "")
      }
      
      selectInput(
        NS(id, "site"),
        "Choose a column for sites:",
        c("None" = "", rv$columns$choices$sites),
        selected = .site.name
      )
    })

    # Get input
    observeEvent(input$site,
      {
        site.name <- input$site
        
        if (!isTruthy(site.name)) {
          rv$columns$site$col <- ""
          rv$columns$site$file <- ""
        }

        req(isTruthy(site.name))

        .tmp <- site.name %>%
          strsplit(., "///", TRUE) %>%
          unlist()
        rv$columns$site$col <- .tmp[1]
        rv$columns$site$file <- rv$columns$choices$file <- .tmp[2]
      },
      ignoreInit = TRUE,
      label = "EAL5 get site"
    )

    # * Latitude ----
    output$`latitude-ui` <- renderUI({
      isolate({
        .lat.cols <- printReactiveValues(rv$columns$lat)
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
        NS(id, "latitude"),
        "Choose a column for latitude:",
        c("None" = "", rv$columns$choices$coords),
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
        
        if(!isTruthy(latCols)){
          rv$columns$lat$col <- ""
          rv$columns$lat$file <- ""
        }
        
        req(isTruthy(latCols))
        
        .tmp <- latCols %>%
          strsplit(., "///", TRUE) %>%
          unlist()
        
        rv$columns$lat$col <- .tmp[1]
        rv$columns$lat$file <- .tmp[2]
        rv$columns$choices$file <- .tmp[2]
      },
      ignoreInit = TRUE,
      priority = 1,
      ignoreNULL = FALSE,
      label = "EAL5 get latitude"
    )

    # * Longitude ----
    output$`longitude-ui` <- renderUI({
      isolate({
        .lon.cols <- printReactiveValues(rv$columns$lon)
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
        c("None" = "", rv$columns$choices$coords),
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
        
        if(!isTruthy(.lon.cols)){
          rv$columns$lon$col <- ""
          rv$columns$lon$file <- ""
        }
        
        req(isTruthy(.lon.cols))
        
        .tmp <- .lon.cols %>%
          strsplit(., "///", TRUE) %>%
          unlist()
        
        rv$columns$lon$col <- .tmp[1]
        rv$columns$lon$file <- rv$columns$choices$file <- .tmp[2]
      },
      ignoreInit = TRUE,
      priority = 1,
      ignoreNULL = FALSE,
      label = "EAL5 get longitude"
    )
  }
  
  # Fill custom ----
  # * Setup ----
  if (dim(rv$custom$coordinates)[1] > 0) {
    sapply(1:nrow(rv$custom$coordinates), function(ind) {
      id <- -ind
      rv$custom <- insertGeoCovInput(
        as.numeric(id), # from -n to -1
        rv$custom,
        ns,
        default = rv$custom$coordinates[ind]
      )
    })
  }

  # * Manage input ----
  observeEvent(input$addui, {
    show("slider_tips")
    rv$custom <- insertGeoCovInput(
      as.numeric(input$addui), # from 1 to n
      rv$custom,
      ns
    )
  })

  # Saves ----
  observeEvent(
    {
      rv$columns$site$col
      rv$columns$lat$col
      rv$columns$lon$col
    },
    {
      rv$columns$complete <- isTruthy(rv$columns$site$col) &&
        isTruthy(rv$columns$lat$col) &&
        isTruthy(rv$columns$lon$col)
    },
    ignoreNULL = FALSE
  )

  observeEvent(rv$custom$coordinates,
    {
      rv$custom$complete <- checkTruth(rv$custom$coordinates)
    },
    ignoreNULL = FALSE
  )

  observe({
    main.env$EAL$completed <- any(
      isTRUE(rv$custom$complete),
      isTRUE(rv$columns$complete)
    )
  })

  # observeEvent(NSB$SAVE,
  shinyjs::onclick(
    "fill-wizard-save",
    asis = TRUE,
    add = TRUE,
    {
      req(utils::tail(main.env$EAL$history, 1) == "Geographic Coverage")

      save.variable <- saveReactive(
        save.variable,
        rv = list(GeoCov = rv),
        main.env = main.env
      )
      showNotification(
        "Geographic Coverage has been saved",
        type = "message"
      )
    },
    ignoreInit = TRUE
  )

  # Process data ----
  # * Previous ----
  observeEvent(EAL$.prev,
    {
      req(main.env$EAL$current == "Geographic Coverage")

      if (!"Categorical Variables" %in% main.env$EAL$history) {
        main.env$EAL$page <- main.env$EAL$page - 1
      }
    },
    ignoreInit = TRUE
  )

  # * Next ----
  observeEvent(EAL$.next,
    {
      req(main.env$EAL$current == "Geographic Coverage")

      # Create modal
      choices <- c(
        "Columns selection" = if (rv$columns$complete) "columns" else NULL,
        "Custom edition" = if (rv$custom$complete) "custom" else NULL
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
    priority = 1,
    ignoreInit = TRUE
  )

  observeEvent(input$confirm, {
    removeModal()
    main.env$EAL$page <-  6
    EAL$tag.list <- tagList()

    .method <- input$method
    
    if(.method == "columns")
      rv$custom$complete <- FALSE
    if(.method == "custom")
      rv$columns$complete <- FALSE
    
    save.variable <- saveReactive(
      save.variable,
      rv = list(GeoCov = rv),
      main.env = main.env
    )
  }, ignoreInit = TRUE)

  # Output ----
  return(save.variable)
})
}
