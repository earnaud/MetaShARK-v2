# Insert Custom GeoCov Form ====
insertCustomGeoCov <- function(id, main.env) {
  # create the UI
  new.ui <- customGeoCov_UI(
    id,
    value = optional(listReactiveValues(main.env$custom[[id]]))
  )
  # insert the UI
  insertUI(selector = "#inserthere", ui = new.ui, immediate = TRUE)
  # create the server
  customGeoCov(unns(id), main.env)
}

# Custom GeoCov Form UI ====
customGeoCov_UI <- function(id, value = NULL) {
  ns <- NS(id)
  
  value <- optional(
    value,
    list(
      type = "rectangle",
      description = "", # length == 1
      points = data.frame( # length >= 1
        id = 1:3,
        lat = c(30,60,45),
        lon = c(-15,35,-15),# lon of the point
        stringsAsFactors = FALSE
      ),
      color = "#03f" # length == 1
    )
  )
  
  tags$div(
    id = ns("box"),
    class = "inputBox",
    style = "max-height: 300px; overflow: scroll",
    fluidRow(
      column(
        10,
        fluidRow(
          column(
            6,
            textInput(
              ns("site_description"),
              "Site description",
              optional(value$description, "")
            )
          ),
          column(
            6,
            shinyWidgets::radioGroupButtons(
              inputId = ns("type"),
              label = "Type",
              choices =  c("marker", "rectangle", "polygon"),
              selected = optional(value$type, "rectangle"),
              justified = TRUE
            )
          )
        ),
        customLocationInput_UI(
          ns("1"), 
          list(
            lat = optional(value$points$lat[1], 35),
            lon = optional(value$points$lon[1], -15)
          )
        ),
        if(length(value$points$lat) > 1)
          customLocationInput_UI(
            ns("2"), 
            list(
              lat = optional(value$points$lat[2], 60),
              lon = optional(value$points$lon[2], 35)
            )
          ),
        if(length(value$points$lat) > 2)
          customLocationInput_UI(
            ns("3"), 
            list(
              lat = optional(value$points$lat[3], 45),
              lon = optional(value$points$lon[3], -15)
            )
          ),
        tags$div(id=sprintf("inserthere_eal5_custom_%s", unns(id))),
        actionButton(
          ns("new_point"),
          "New point", 
          icon("map-marker-alt")
        )
      ),
      column(
        2,
        actionButton(
          ns("rmv"),"",
          icon("trash"), 
          class="danger"
        ),
        colorPickerInput(
          ns("color"),
          label = "",
          width = "40px"
        )
      )
    ),
    actionButton(ns("devinside"), "DEV")
  )
}

# Custom GeoCov Server ====
customGeoCov <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$devinside, {
      browser()
    })
    
    # Set reactive value ----
    # Do not set if already set
    main.env$custom[[id]] <- optional(
      main.env$custom[[id]],
      reactiveValues(
        count = 3, # number of locationInputs
        # Values 
        type = "rectangle",
        description = "", # length == 1
        points = data.frame( # length >= 1
          id = 1:3, 
          lat = c(30,60,45),
          lon = c(-15,35,-15),# lon of the point
          stringsAsFactors = FALSE
        ),
        color = "#03f" # length == 1
      )
    )
    
    # Load [WIP] ====
    # Adds as many locations input as necessary
    if(nrow(main.env$custom[[id]]$points) > 3) {
      sapply(
        main.env$custom[[id]]$points$id[
          4:nrow(main.env$custom[[id]]$points)
        ],
        function(rowid) {
          main.env$custom[[id]]$count <<- main.env$custom[[id]]$count+1
          
          insertCustomLocationInput(
            session$ns(as.character(main.env$custom[[id]]$count)), 
            outer.id = id,
            default = main.env$custom[[id]]$points |>
              filter(id == rowid) |>
              select(c("lat","lon")),
            selector = sprintf("#inserthere_eal5_custom_%s", id),
            main.env = main.env
          )
        }
      )
    }
    
    # Change type ====
    observeEvent(input$type, {
      main.env$custom[[id]]$type <- input$type
      
      .polygon.points.ids <- (1:nrow(main.env$custom[[id]]$points))[-c(1,2)]
      
      if(input$type == "marker") {
        shinyjs::hide("2-box")
        shinyjs::hide("new_point")
        if(length(.polygon.points.ids) > 0){
          sapply(
            paste0(
              main.env$custom[[id]]$points[.polygon.points.ids, "id"],
              "-box"
            ),
            shinyjs::hide
          )
        }
      } 
      
      if(input$type == "rectangle") {
        shinyjs::show("2-box")
        shinyjs::hide("new_point")
        if(length(.polygon.points.ids) > 0){
          sapply(
            paste0(
              main.env$custom[[id]]$points[.polygon.points.ids, "id"],
              "-box"
            ),
            shinyjs::hide
          )
        }
        
      } 
      
      if(input$type == "polygon") {
        shinyjs::show("2-box")
        shinyjs::show("new_point")
        if(length(.polygon.points.ids) > 0){
          sapply(
            paste0(
              main.env$custom[[id]]$points[.polygon.points.ids, "id"],
              "-box"
            ),
            shinyjs::show
          )
        }
        
      }
    })
    
    # Add points ====
    customLocationInput("1", outer.id = id, main.env)
    customLocationInput("2", outer.id = id, main.env)
    customLocationInput("3", outer.id = id, main.env)
    
    observeEvent(input$new_point, {
      main.env$custom[[id]]$count <<- main.env$custom[[id]]$count+1
      
      insertCustomLocationInput(
        session$ns(as.character(main.env$custom[[id]]$count)), 
        outer.id = id,
        default = list(lat = 0, lon = 0),
        selector = sprintf("#inserthere_eal5_custom_%s", id),
        main.env = main.env
      )
    })
    
    # Get color ====
    observeEvent(input$color, {
      req(isTruthy(input$color))
      main.env$custom[[id]]$color <- input$color
    })
    
    # Remove ====
    observeEvent(input$rmv, {
      message(paste0("#", NS(id, "box")))
      # remove UI
      removeUI(selector = paste0("#", NS(id, "box")), immediate = TRUE)
      # remove data
      main.env$custom[[id]] <- NULL
    })
    
  })
}



# TEST ====
testCustomInput <- function() {
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    htmltools::includeCSS(
      system.file("app/www/styles.css", package = "MetaShARK")
    ),
    actionButton("dev", "dev", width = "100%"),
    fluidRow(
      column(
        7,
        shinyWidgets::actionBttn(
          "add", "", icon("plus"), style = "simple", color="primary"
        ),
        tags$div(id="inserthere")
      ),
      column(
        5,
        leaflet::leafletOutput("leaflet")
      )
    )
  )
  
  server <- function(input, output, session) {
    observeEvent(input$dev, {
      browser()
    })
    
    main.env <- new.env()
    assign(
      "custom",
      reactiveValues(
        # will be inserted reactiveValues() named as numbers
        count = 0
      ),
      envir = main.env
    )
    
    # Add UIs ====
    ## Click add ----
    observeEvent(input$add, {
      main.env$custom$count <<- main.env$custom$count+1
      
      ## proper insert
      insertCustomGeoCov(as.character(main.env$custom$count), main.env)
    }, ignoreInit = TRUE)
    
    ## Get leaflet drawing ----
    observeEvent(input$leaflet_draw_new_feature,{
      main.env$custom$count <<- main.env$custom$count+1
      
      .nm <- as.character(main.env$custom$count)
      main.env$custom[[as.character(.nm)]] <- reactiveValues(
        count = 3,
        # get feature type
        type = input$leaflet_draw_new_feature$properties$feature_type,
        # get site description
        description = sprintf("site %s", main.env$custom$count),
        # set default color
        color = "#03f"
      )
      # add coordinates
      .coor <- unlist(input$leaflet_draw_new_feature$geometry$coordinates)
      .lat = .coor[seq(2,length(.coor), 2)]
      .lon = .coor[seq(1,length(.coor), 2)]
      
      .points = switch(
        main.env$custom[[as.character(.nm)]]$type,
        marker = {
          data.frame(
            id = 1:3, 
            lat = c(.lat,60,45),
            lon = c(.lon,35,-15),
            stringsAsFactors = FALSE
          )
        },
        rectangle = {
          data.frame(
            id = 1:3,
            lat = c(min(.lat), max(.lat), 45),
            lon = c(min(.lon), max(.lon), -15),
            stringsAsFactors = FALSE
          )
        },
        polygon = {
          data.frame(
            id = seq_along(.coor[seq(2,length(.coor), 2)]),
            lat = .lat,
            lon = .lon,
            stringsAsFactors = FALSE
          ) |> tail(-1)
        }
      )
      main.env$custom[[as.character(.nm)]]$points <- .points
      
      insertCustomGeoCov(as.character(main.env$custom$count), main.env)
    })
    
    # Render leaflet ====
    ## Get values ----
    areas <- reactive({
      .nms <- names(main.env$custom)[
        sapply(
          names(main.env$custom), 
          function(n) 
            n != "count" &&
            isContentTruthy(main.env$custom[[n]])
        )
      ]
      
      lapply(.nms, function(id) {
        switch(
          main.env$custom[[id]]$type,
          marker = {
            list(
              type = main.env$custom[[id]]$type,
              lat = main.env$custom[[id]]$points$lat[1],
              lon = main.env$custom[[id]]$points$lon[1],
              col = main.env$custom[[id]]$color
            )
          },
          rectangle = {
            list(
              type = main.env$custom[[id]]$type,
              lat1 = min(main.env$custom[[id]]$points$lat[1:2]),
              lat2 = max(main.env$custom[[id]]$points$lat[1:2]),
              lon1 = min(main.env$custom[[id]]$points$lon[1:2]),
              lon2 = max(main.env$custom[[id]]$points$lon[1:2]),
              col = main.env$custom[[id]]$color
            )
          },
          polygon = {
            list(
              type = main.env$custom[[id]]$type,
              lat = c(
                main.env$custom[[id]]$points$lat,
                main.env$custom[[id]]$points$lat[1]
              ),
              lon = c(
                main.env$custom[[id]]$points$lon,
                main.env$custom[[id]]$points$lon[1]
              ),
              col = main.env$custom[[id]]$color
            )
          }
        )
      }) |>
        setNames(.nms)
    })
    
    ## Render ----
    map <- reactive({
      .map <- leaflet::leaflet("geocov") |>
        leaflet::addTiles() |>
        leaflet.extras::addDrawToolbar(
          polylineOptions = FALSE,
          circleOptions = FALSE,
          circleMarkerOptions = FALSE
        )
      if(isContentTruthy(areas())){
        .nms <- names(areas())
        sapply(names(areas()), function(nm) {
          if(areas()[[nm]]$type == "marker") {
            .map <<- leaflet::addAwesomeMarkers(
              .map,
              lng = areas()[[nm]]$lon,
              lat = areas()[[nm]]$lat,
              icon = leaflet::makeAwesomeIcon(
                "circle",
                library = "fa",
                markerColor = areas()[[nm]]$col,
                iconColor = "black"
              )
            )
          } else if(areas()[[nm]]$type == "rectangle") {
            .map <<- leaflet::addRectangles(
              .map,
              lat1=areas()[[nm]]$lat1,
              lat2=areas()[[nm]]$lat2,
              lng1=areas()[[nm]]$lon1,
              lng2=areas()[[nm]]$lon2,
              color=areas()[[nm]]$col
            )
          } else if(areas()[[nm]]$type == "polygon") {
            .map <<- leaflet::addPolygons(
              .map,
              lng = areas()[[nm]]$lon,
              lat = areas()[[nm]]$lat,
              color=areas()[[nm]]$col
            )
          }
          
          
        })
      }
      return(.map)
    }) |> 
      debounce(1000)
    
    output$leaflet <- leaflet::renderLeaflet({
      map()
    })
    
  }
  
  shinyApp(ui, server)
}
