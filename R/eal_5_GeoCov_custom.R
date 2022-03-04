# Insert Custom GeoCov Form ====
insertCustomGeoCov <- function(id, main.env) {
  # create the UI
  new.ui <- customGeoCov_UI(
    id,
    value = optional(listReactiveValues(main.env$local.rv$custom[[unns(id)]]))
  )
  # insert the UI
  insertUI(selector = "#inserthere_eal5", ui = new.ui, immediate = TRUE)
  
  # create the server
  customGeoCov(unns(id), main.env)
}

# Custom GeoCov Form UI ====
#' @noRd
#' @importFrom colourpicker colourInput
customGeoCov_UI <- function(id, value = NULL) {
  ns <- NS(id)
  
  value <- optional(
    value,
    list(
      type = "rectangle",
      description = sprintf("site %s", unns(id)), # length == 1
      points = data.frame( # length >= 1
        id = 1:3,
        lat = c(30,60,45),
        lon = c(-15,35,-15),# lon of the point
        stringsAsFactors = FALSE
      ),
      color = "#0033ff" # length == 1
    )
  )
  
  tags$div(
    id = ns("box"),
    class = "inputBox",
    style = "
    max-height: 10vw !important;
    background: #ffffff57;
    overflow: hidden;",
    fluidRow(
      # Form column ----
      column(
        10,
        fluidRow(
          ## Site description ----
          column(
            6,
            textInput(
              ns("site_description"),
              "Site description",
              optional(value$description, "")
            )
          ),
          ## Site type ----
          column(
            6,
            radioButtons(
              inputId = ns("type"),
              label = "Type",
              choices =  c("marker", "rectangle", "polygon"),
              selected = optional(value$type, "rectangle"),
              inline = TRUE
            )
            # shinyWidgets::radioGroupButtons(
            #   inputId = ns("type"),
            #   label = "Type",
            #   choices =  c("marker", "rectangle", "polygon"),
            #   selected = optional(value$type, "rectangle"),
            #   justified = TRUE
            # )
          )
        ),
        ## Points input ----
        # tags$div(
        #   customLocationInput_UI(
        #     ns("1"), 
        #     list(
        #       lat = optional(value$points$lat[1], 35),
        #       lon = optional(value$points$lon[1], -15)
        #     )
        #   ),
        #   if(length(value$points$lat) > 1)
        #     customLocationInput_UI(
        #       ns("2"), 
        #       list(
        #         lat = optional(value$points$lat[2], 60),
        #         lon = optional(value$points$lon[2], 35)
        #       )
        #     ),
        #   if(length(value$points$lat) > 2)
        #     customLocationInput_UI(
        #       ns("3"), 
        #       list(
        #         lat = optional(value$points$lat[3], 45),
        #         lon = optional(value$points$lon[3], -15)
        #       )
        #     )
        # ),
        tags$div(
          style = "overflow-y: scroll; height: 6vw;",
          tags$div(id=sprintf("inserthere_eal5_custom_%s", unns(id))),
          actionButton(ns("new_point"), "New point", icon("plus"))
        )
      ),
      # Accessory column ----
      column(
        2,
        style = "top: 2em",
        fluidRow(
          actionButton(
            ns("rmv"),"",
            icon("trash"), 
            class="danger"
          )
        ),
        fluidRow(
          colourpicker::colourInput(
            ns("color"),
            label = "Color",
            showColour = "background",
            value = value$color
          ) |>
            tagAppendAttributes(style="width:40px;")
          # colorPickerInput(
          #   ns("color"),
          #   label = "",
          #   width = "40px"
          # )
        )
      )
    ),
    actionButton(ns("devinside"), "DEV")
  )
}

# Custom GeoCov Server ====
customGeoCov <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Dev ----
    observeEvent(input$devinside, {
      browser()
    })
    
    # Set reactive value ----
    # Do not set if already set
    main.env$local.rv$custom[[id]] <- optional(
      main.env$local.rv$custom[[id]],
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
    # Correct some values
    .npoints = nrow(main.env$local.rv$custom[[id]]$points)
    # Ensure points ids match the numbers of rows
    main.env$local.rv$custom[[id]]$points$id <- 1:.npoints
    # Fix the count
    main.env$local.rv$custom[[id]]$count <- .npoints
    
    # Load ====
    # !!! Initialized on click, not with main.env$EAL$page
    
    # Count locations
    # .npoints = nrow(main.env$local.rv$custom[[id]]$points)
    
    ## Adds as many locations input as necessary ----
    # if(.npoints > 3) {
    sapply(
      1:.npoints,
      function(rowid) { # enforced previously
        insertCustomLocationInput(
          session$ns(as.character(rowid)), # 1:nrow
          outer.id = id,
          default = main.env$local.rv$custom[[id]]$points |>
            filter(id == rowid) |>
            select(c("lat","lon")),
          selector = sprintf("#inserthere_eal5_custom_%s", id),
          local.rv = main.env$local.rv,
          removable = main.env$local.rv$custom[[id]]$count > 3 # do not make first 3 points removable
        )
      }
    ) # end of sapply
    # }
    
    # # Serve first three points
    # customLocationInput("1", outer.id = id, main.env$local.rv)
    # customLocationInput("2", outer.id = id, main.env$local.rv)
    # customLocationInput("3", outer.id = id, main.env$local.rv)
    
    ## Init visibility ----
    # Hide 2nd box if marker
    shinyjs::toggle("2-box", condition = main.env$local.rv$custom[[id]]$type != "marker")
    # Hide 3rd box if not polygon
    shinyjs::toggle("3-box", condition = main.env$local.rv$custom[[id]]$type == "polygon")
    # Hide New point box unless type is polygon
    shinyjs::toggle("new_point", condition = main.env$local.rv$custom[[id]]$type == "polygon")
    
    # Get description ====
    observeEvent(input$site_description, {
      req(main.env$EAL$page == 5)
      req(isTruthy(input$site_description))
      
      main.env$local.rv$custom[[id]]$description <- input$site_description
    }, label = session$ns("description"))
    
    # Change type ====
    observeEvent(input$type, {
      req(main.env$EAL$page == 5)
      # Number of points in polygon
      .polygon.points.ids <- (1:nrow(main.env$local.rv$custom[[id]]$points))[-c(1,2)]
      # Keep old type in local
      .old.type <- main.env$local.rv$custom[[id]]$type
      # Properly change type of spatial feature
      main.env$local.rv$custom[[id]]$type <- input$type
      
      # ## Set to marker ---
      # if(input$type == "marker") {
      #   # if(length(.polygon.points.ids) > 0){
      #   #   sapply(
      #   #     # hide all point inputs whose ID > 1 (it can't have been added)
      #   #     paste0(
      #   #       main.env$local.rv$custom[[id]]$points |> 
      #   #         filter(as.numeric(id) > 1) |>
      #   #         select(id) |> 
      #   #         unlist(),
      #   #       # main.env$local.rv$custom[[id]]$points[.polygon.points.ids, "id"],
      #   #       "-box"
      #   #     ),
      #   #     shinyjs::hide
      #   #   )
      #   # }
      # }
      # 
      # # Set to rectangle ---
      # if(input$type == "rectangle") {
      #   # shinyjs::show("2-box")
      #   # shinyjs::hide("new_point")
      #   # newly created rectangle
      #   # if(.old.type == input$type){
      #   #   main.env$local.rv$custom[[id]]$points$lat[3] <- mean(main.env$local.rv$custom[[id]]$points$lat[1:2])
      #   #   main.env$local.rv$custom[[id]]$points$lon[3] <- mean(main.env$local.rv$custom[[id]]$points$lon[1:2])
      #   # }
      #   # if(length(.polygon.points.ids) > 0){
      #   #   sapply(
      #   #     # hide all point inputs whose ID > 2 (they can't have been added)
      #   #     paste0(
      #   #       main.env$local.rv$custom[[id]]$points |> 
      #   #         filter(as.numeric(id) > 2) |>
      #   #         select(id) |> 
      #   #         unlist(),
      #   #       # main.env$local.rv$custom[[id]]$points[.polygon.points.ids, "id"],
      #   #       "-box"
      #   #     ),
      #   #     shinyjs::hide
      #   #   )
      #   # }
      # }
      # 
      # # Set to polygon ---
      # if(input$type == "polygon") {
      #   # shinyjs::show("2-box")
      #   # shinyjs::show("new_point")
      #   # if(length(.polygon.points.ids) > 0){
      #   #   sapply(
      #   #     paste0(
      #   #       main.env$local.rv$custom[[id]]$points[.polygon.points.ids, "id"],
      #   #       "-box"
      #   #     ),
      #   #     shinyjs::show
      #   #   )
      #   # }
      #   
      # }
    },label = "change type")
    
    # Hide/show divs ====
    observe({
      .type <- main.env$local.rv$custom[[id]]$type
      req(!is.null(.type))
      # hide all useless point
      ## Marker ----
      ## Hide all but 1-box (never hidden)
      if(.type == "marker") {
        sapply(
          paste0(
            main.env$local.rv$custom[[id]]$points |> 
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
      if(.type == "rectangle") {
        sapply(
          paste0(
            main.env$local.rv$custom[[id]]$points |> 
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
      if(.type == "polygon") {
        sapply(
          paste0(
            main.env$local.rv$custom[[id]]$points |> 
              filter(as.numeric(id) > 3) |>
              select(id) |> 
              unlist(),
            "-box"
          ),
          shinyjs::show
        )
      }
      # # Hide 2nd box if marker
      # shinyjs::toggle("2-box", condition = main.env$local.rv$custom[[id]]$type != "marker")
      # # Hide 3rd box if not polygon
      # shinyjs::toggle("3-box", condition = main.env$local.rv$custom[[id]]$type == "polygon")
      
      # Hide New point box unless type is polygon
      shinyjs::toggle("new_point", condition = main.env$local.rv$custom[[id]]$type == "polygon")
    })
    
    # Add points ====
    observeEvent(input$new_point, {
      main.env$local.rv$custom[[id]]$count <<- main.env$local.rv$custom[[id]]$count+1
      
      insertCustomLocationInput(
        session$ns(as.character(main.env$local.rv$custom[[id]]$count)), 
        outer.id = id,
        default = list(lat = 0, lon = 0),
        selector = sprintf("#inserthere_eal5_custom_%s", id),
        local.rv = main.env$local.rv
      )
    })
    
    # Get color ====
    observeEvent(input$color, {
      req(isTruthy(input$color))
      
      main.env$local.rv$custom[[id]]$color <- input$color
    })
    
    # Remove ====
    observeEvent(input$rmv, {
      message(paste0("#", NS(id, "box")))
      # remove UI
      removeUI(selector = paste0("#", session$ns("box")), immediate = TRUE)
      # remove data
      main.env$local.rv$custom[[id]] <- NULL
      # https://github.com/rstudio/shiny/issues/2439
      .subset2(main.env$local.rv$custom, "impl")$.values$remove("1")
    })
    
  })
}

saveCustomGeoCov <- function() {
  .local.rv <- main.env$local.rv$custom
  .features.ids <- names(.local.rv)[names(.local.rv) != "count"]
  
  out <- lapply(.features.ids, function(feat.id) {
    .points <- .local.rv[[feat.id]]$points
    if(.local.rv[[feat.id]]$type == "marker")
      .points <- .points[1,]
    if(.local.rv[[feat.id]]$type == "rectangle")
      .points <- .points[1:2,]
    
    data.frame(
      geographicDescription = .local.rv[[feat.id]]$description, 
      northBoundingCoordinate = max(.points$lat),
      southBoundingCoordinate = min(.points$lat),
      eastBoundingCoordinate = max(.points$lon),
      westBoundingCoordinate = min(.points$lon),
      wkt = if(.local.rv[[feat.id]]$type == "polygon") {
        .points[c(1:nrow(.points), 1), 2:3] |>
          as.matrix() |>
          list() |>
          st_polygon() |>
          st_as_text()
      } else ""
    ) # end of data.frame
  }) |>
    bind_rows()
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
        tags$div(id="inserthere_eal5")
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
      "local.rv",
      reactiveValues(
        custom = reactiveValues(
          # will be inserted reactiveValues() named as numbers
          count = 0
        )
      ),
      envir = main.env
    )
    
    # Add UIs ====
    ## Click add ----
    observeEvent(input$add, {
      main.env$local.rv$custom$count <<- main.env$local.rv$custom$count+1
      
      ## proper insert
      insertCustomGeoCov(as.character(main.env$local.rv$custom$count), main.env)
    }, ignoreInit = TRUE)
    
    ## Get leaflet drawing ----
    observeEvent(input$leaflet_draw_new_feature,{
      main.env$local.rv$custom$count <<- main.env$local.rv$custom$count+1
      
      .nm <- as.character(main.env$local.rv$custom$count)
      main.env$local.rv$custom[[as.character(.nm)]] <- reactiveValues(
        count = 3,
        # get feature type
        type = input$leaflet_draw_new_feature$properties$feature_type,
        # get site description
        description = sprintf("site %s", main.env$local.rv$custom$count),
        # set default color
        color = "#03f"
      )
      
      # add coordinates
      .coor <- unlist(input$leaflet_draw_new_feature$geometry$coordinates)
      .lat = .coor[seq(2,length(.coor), 2)]
      .lon = .coor[seq(1,length(.coor), 2)]
      
      .points = switch(
        main.env$local.rv$custom[[as.character(.nm)]]$type,
        marker = {
          data.frame(
            id = 1:3, 
            lat = c(.lat,60,mean(c(.lat, 60))),
            lon = c(.lon,35,mean(c(.lat, 35))),
            stringsAsFactors = FALSE
          )
        },
        rectangle = {
          data.frame(
            id = 1:3,
            lat = c(min(.lat), max(.lat), mean(.lat)),
            lon = c(min(.lon), max(.lon), mean(.lon)),
            stringsAsFactors = FALSE
          )
        },
        polygon = {
          data.frame(
            id = seq_along(.coor[seq(2,length(.coor), 2)]),
            lat = .lat,
            lon = .lon,
            stringsAsFactors = FALSE
          ) |>
            tail(-1) # do not repeat last point
        }
      )
      main.env$local.rv$custom[[as.character(.nm)]]$points <- .points
      
      insertCustomGeoCov(as.character(main.env$local.rv$custom$count), main.env)
    })
    
    # Render leaflet ====
    ## Get values ----
    areas <- reactive({
      .nms <- names(main.env$local.rv$custom)[
        sapply(
          names(main.env$local.rv$custom), 
          function(n) 
            n != "count" &&
            isContentTruthy(main.env$local.rv$custom[[n]])
        )
      ]
      
      lapply(.nms, function(id) {
        switch(
          main.env$local.rv$custom[[id]]$type,
          marker = {
            list(
              type = main.env$local.rv$custom[[id]]$type,
              lat = main.env$local.rv$custom[[id]]$points$lat[1],
              lon = main.env$local.rv$custom[[id]]$points$lon[1],
              col = main.env$local.rv$custom[[id]]$color
            )
          },
          rectangle = {
            list(
              type = main.env$local.rv$custom[[id]]$type,
              lat1 = min(main.env$local.rv$custom[[id]]$points$lat),
              lat2 = max(main.env$local.rv$custom[[id]]$points$lat),
              lon1 = min(main.env$local.rv$custom[[id]]$points$lon),
              lon2 = max(main.env$local.rv$custom[[id]]$points$lon),
              col = main.env$local.rv$custom[[id]]$color
            )
          },
          polygon = {
            list(
              type = main.env$local.rv$custom[[id]]$type,
              lat = c(
                main.env$local.rv$custom[[id]]$points$lat,
                main.env$local.rv$custom[[id]]$points$lat[1]
              ),
              lon = c(
                main.env$local.rv$custom[[id]]$points$lon,
                main.env$local.rv$custom[[id]]$points$lon[1]
              ),
              col = main.env$local.rv$custom[[id]]$color
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
