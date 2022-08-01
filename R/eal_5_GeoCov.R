#' @import shiny
#' @importFrom shinyWidgets materialSwitch
#' @importFrom leaflet leafletOutput
#'
#' @noRd
GeoCovUI <- function(id) {
  ns <- NS(id)
  
  return(
    fluidPage(
      fluidRow(
        tags$p(
          "You can fill in geographic coverage through two methods: either
          by chosing variables from your files, or manually define it. Mixed
          approach is not supported. Check the help above for more details.",
          tags$b("Only WGS84 is currently supported.")
        ),
        tags$fieldset(
          # Method ====
          tags$legend(
            tags$span(
              "columns",
              shinyWidgets::materialSwitch(
                ns("method"),
                tags$h5("Method"),
                inline = TRUE
              ),
              "custom"
            ) # end of method
          ),
          # Columns ====
          tags$div(
            id = ns("columns_input"),
            tagList(
              # Site description
              uiOutput(ns("site")),
              # Latitude
              uiOutput(ns("latitude")),
              # Longitude
              uiOutput(ns("longitude"))
            )
          ), # end of columns
          # Custom ====
          shinyjs::hidden(
            tags$div(
              id = ns("custom_input"),
              fluidRow(
                column(
                  7,
                  shinyWidgets::actionBttn(
                    ns("add"), "Add site", icon("plus"),
                    style = "simple",
                    color = "primary"
                  ),
                  tags$div(id = "inserthere_eal5")
                ),
                column(
                  5,
                  leaflet::leafletOutput(ns("leaflet")) |>
                    withSpinner()
                )
              )
            )
          ) # end of custom
        )
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom shinyWidgets updateMaterialSwitch
#' @importFrom shinyjs toggle
#' @importFrom leaflet renderLeaflet leaflet addTiles addAwesomeMarkers
#' makeAwesomeIcon addRectangles addPolygons
#'
#' @noRd
GeoCov <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    if (main_env$dev) .browse_dev(main_env, 5, input, output, session)
    
    # Columns method ====
    
    ## Setup ====
    observeEvent(main_env$EAL$page, {
      req(main_env$EAL$page == 5)
      req(main_env$local_rv$method)
      devmsg(tag = "Geocov", "load")
      
      shinyWidgets::updateMaterialSwitch(
        session,
        "method",
        switch(main_env$local_rv$method,
               columns = FALSE,
               custom = TRUE
        )
      )
      
      # Display correct UI
      shinyjs::toggle(
        "columns_input",
        condition = main_env$local_rv$method == "columns"
      )
      
      shinyjs::toggle(
        "custom_input",
        condition = main_env$local_rv$method == "custom"
      )
    },
    label = "EAL5 set method switch",
    priority = -1
    )
    
    ## Method selection ====
    observeEvent(input$method, {
      req(main_env$EAL$page == 5)
      devmsg(tag = "Geocov", "method")
      main_env$local_rv$method <- ifelse(input$method, "custom", "columns")
      
      shinyjs::toggle(
        "columns_input",
        condition = main_env$local_rv$method == "columns"
      )
      
      shinyjs::toggle(
        "custom_input",
        condition = main_env$local_rv$method == "custom"
      )
    },
    label = "EAL4: switch method"
    )
    
    ## Variables input ====
    ### Set inputs ----
    # Site description
    output$site <- renderUI({
      req(main_env$EAL$page == 5)
      req(isContentTruthy(main_env$local_rv$columns$choices$sites))
      
      devmsg(tag = "Geocov", "render site")
      
      # Site description
      selectInput(
        session$ns("site"),
        "Choose a column for site descriptions",
        choices = c("None" = "", main_env$local_rv$columns$choices$sites),
        selected = if (isTruthy(main_env$local_rv$columns$site$file) &&
                       isTruthy(main_env$local_rv$columns$site$col)) {
          main_env$local_rv$columns$choices$sites[[
            main_env$local_rv$columns$site$file
          ]][main_env$local_rv$columns$site$col]
        }
      )
    })
    
    # Latitude
    output$latitude <- renderUI({
      req(main_env$EAL$page == 5)
      req(isContentTruthy(main_env$local_rv$columns$choices$coords))
      devmsg(tag = "Geocov", "render latitude")
      
      selectizeInput(
        session$ns("latitude"),
        "Choose a column for latitude values",
        choices = c("None" = "", main_env$local_rv$columns$choices$coords),
        selected = if (isTruthy(main_env$local_rv$columns$lat$file) &&
                       isTruthy(main_env$local_rv$columns$lat$col)) {
          main_env$local_rv$columns$choices$coords[[
            main_env$local_rv$columns$lat$file
          ]][main_env$local_rv$columns$lat$col]
        },
        options = list(maxItems = 2)
      )
    })
    
    # Longitude
    output$longitude <- renderUI({
      req(main_env$EAL$page == 5)
      req(isContentTruthy(main_env$local_rv$columns$choices$coords))
      devmsg(tag = "Geocov", "render longitude")
      
      selectizeInput(
        session$ns("longitude"),
        "Choose a column for longitude values",
        choices = c("None" = "", main_env$local_rv$columns$choices$coords),
        selected = if (isTruthy(main_env$local_rv$columns$lon$file) &&
                       isTruthy(main_env$local_rv$columns$lon$col)) {
          main_env$local_rv$columns$choices$coords[[
            main_env$local_rv$columns$lon$file
          ]][main_env$local_rv$columns$lon$col]
        },
        options = list(maxItems = 2)
      )
    })
    
    ### Get input ----
    # Site description
    observeEvent(input$site, {
      req(main_env$EAL$page == 5)
      devmsg(tag = "Geocov", "input site")
      
      if (!isTruthy(input$site)) {
        main_env$local_rv$columns$site$col <- ""
        main_env$local_rv$columns$site$file <- ""
      } else {
        .tmp <- input$site |>
          strsplit("/", TRUE) |>
          unlist()
        main_env$local_rv$columns$site$col <- .tmp[2]
        main_env$local_rv$columns$site$file <-
          main_env$local_rv$columns$choices$file <- .tmp[1]
      }
      
      # Add feedback
      checkFeedback(input, "site", type = "danger")
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE,
    label = "EAL5: get site"
    )
    
    # Latitude
    observeEvent(input$latitude, {
      req(main_env$EAL$page == 5)
      devmsg(tag = "Geocov", "input latitude")
      
      if (!isTruthy(input$latitude)) {
        main_env$local_rv$columns$lat$col <- ""
        main_env$local_rv$columns$lat$file <- ""
      } else {
        .tmp <- input$latitude |>
          strsplit("/", TRUE) |>
          unlist()
        main_env$local_rv$columns$lat$col <- .tmp[2]
        main_env$local_rv$columns$lat$file <-
          main_env$local_rv$columns$choices$file <- .tmp[1]
      }
      
      # Add feedback
      checkFeedback(input, "latitude", type = "danger")
    },
    ignoreInit = TRUE,
    priority = 1,
    ignoreNULL = FALSE,
    label = "EAL5: get latitude"
    )
    
    # Longitude
    observeEvent(input$longitude, {
      req(main_env$EAL$page == 5)
      devmsg(tag = "Geocov", "input longitude")
      
      if (!isTruthy(input$longitude)) {
        main_env$local_rv$columns$lon$col <- ""
        main_env$local_rv$columns$lon$file <- ""
      } else {
        .tmp <- input$longitude |>
          strsplit("/", TRUE) |>
          unlist()
        main_env$local_rv$columns$lon$col <- .tmp[2]
        main_env$local_rv$columns$lon$file <-
          main_env$local_rv$columns$choices$file <- .tmp[1]
      }
      
      # Add feedback
      checkFeedback(input, "longitude", type = "danger")
    },
    ignoreInit = TRUE,
    priority = 1,
    ignoreNULL = FALSE,
    label = "EAL5: get longitude"
    )
    
    # Custom server =====
    ## Setup ----
    # observeEvent(main_env$EAL$page, {
    #     req(main_env$EAL$page == 5)
    #     req(main_env$local_rv$method == "custom")
    #     req(main_env$local_rv$custom$count > 0)
    # 
    #     try(sapply(1:main_env$local_rv$custom$count, function(.count) {
    #       insertCustomGeoCov(
    #         session$ns(as.character(.count)),
    #         main_env
    #       )
    #     }))
    #   },
    #   priority = -2
    # )
    
    
    ## Click add ----
    observeEvent(input$add, {
      req(input$add)
      req(main_env$EAL$page == 5)
      devmsg(tag = "Geocov", "custom add")
      
      main_env$local_rv$custom$count <<- main_env$local_rv$custom$count + 1
      
      ## proper insert
      insertCustomGeoCov(
        session$ns(as.character(main_env$local_rv$custom$count)),
        main_env
      )
    },
    ignoreInit = TRUE
    )
    
    ## Get leaflet drawing ----
    observeEvent(input$leaflet_draw_new_feature, {
      req(main_env$EAL$page == 5)
      devmsg(tag = "Geocov", "leaflet draw")
      
      main_env$local_rv$custom$count <<- main_env$local_rv$custom$count + 1
      
      .nm <- as.character(main_env$local_rv$custom$count)
      main_env$local_rv$custom[[as.character(.nm)]] <- reactiveValues(
        count = 0,
        # get feature type
        type = input$leaflet_draw_new_feature$properties$feature_type,
        # get site description
        description = sprintf("site %s", main_env$local_rv$custom$count),
        # set default color
        color = "#0000FF"
      )
      
      # add coordinates
      .coor <- unlist(input$leaflet_draw_new_feature$geometry$coordinates)
      .lat <- .coor[seq(2, length(.coor), 2)]
      .lon <- .coor[seq(1, length(.coor), 2)]
      
      .points <- switch(main_env$local_rv$custom[[as.character(.nm)]]$type,
                        marker = data.frame(
                          id = 1:3,
                          lat = c(.lat, 60, mean(c(.lat, 60))),
                          lon = c(.lon, 35, mean(c(.lat, 35))),
                          stringsAsFactors = FALSE
                        ),
                        rectangle = data.frame(
                          id = 1:3,
                          lat = c(min(.lat), max(.lat), mean(.lat)),
                          lon = c(min(.lon), max(.lon), mean(.lon)),
                          stringsAsFactors = FALSE
                        ),
                        polygon = data.frame(
                          id = 1:(length(.coor) / 2),
                          lat = .lat,
                          lon = .lon,
                          stringsAsFactors = FALSE
                        ) |>
                          tail(-1) # save without repeating last point
      )
      main_env$local_rv$custom[[as.character(.nm)]]$points <- .points
      
      insertCustomGeoCov(
        session$ns(as.character(main_env$local_rv$custom$count)),
        main_env
      )
    })
    
    ## Render leaflet ====
    
    ### Output ----
    ### TODO optimize the input$leaflet_drawing_* to get only the full feature,
    ### not react to each point
    output$leaflet <- leaflet::renderLeaflet({
      req(main_env$EAL$page == 5)
      devmsg(tag = "Geocov", "leaflet render")
      
      #### Get names ----
      
      .nms <- try(names(main_env$local_rv$custom)[
        sapply(
          names(main_env$local_rv$custom),
          function(n) {
            n != "count" &&
              n != "complete" &&
              isContentTruthy(main_env$local_rv$custom[[n]])
          }
        )
      ])
      
      validate(
        need(class(.nms) != "try-error", "Error in main_env$local_rv$custom")
      )
      
      #### Get values ----
      areas <- try({
        lapply(.nms, function(id) {
          switch(main_env$local_rv$custom[[id]]$type,
                 marker = {
                   list(
                     type = main_env$local_rv$custom[[id]]$type,
                     lat = main_env$local_rv$custom[[id]]$points$lat[1],
                     lon = main_env$local_rv$custom[[id]]$points$lon[1],
                     col = main_env$local_rv$custom[[id]]$color
                   )
                 },
                 rectangle = {
                   list(
                     type = main_env$local_rv$custom[[id]]$type,
                     lat1 = min(main_env$local_rv$custom[[id]]$points$lat),
                     lat2 = max(main_env$local_rv$custom[[id]]$points$lat),
                     lon1 = min(main_env$local_rv$custom[[id]]$points$lon),
                     lon2 = max(main_env$local_rv$custom[[id]]$points$lon),
                     col = main_env$local_rv$custom[[id]]$color
                   )
                 },
                 polygon = {
                   list(
                     type = main_env$local_rv$custom[[id]]$type,
                     lat = c(
                       main_env$local_rv$custom[[id]]$points$lat,
                       main_env$local_rv$custom[[id]]$points$lat[1]
                     ),
                     lon = c(
                       main_env$local_rv$custom[[id]]$points$lon,
                       main_env$local_rv$custom[[id]]$points$lon[1]
                     ),
                     col = main_env$local_rv$custom[[id]]$color
                   )
                 }
          )
        }) |>
          setNames(.nms)
      })
      
      validate(
        need(class(areas) != "try-error", "Error while defining areas to draw")
      )
      
      #### Render map ----
      .map <- try({
        .map <- leaflet::leaflet("geocov") |>
          leaflet::addTiles() |>
          leaflet.extras::addDrawToolbar(
            polylineOptions = FALSE,
            circleOptions = FALSE,
            circleMarkerOptions = FALSE
          )
        
        if (isContentTruthy(areas)) {
          .nms <- names(areas)
          sapply(names(areas), function(nm) {
            if (areas[[nm]]$type == "marker") {
              .map <<- leaflet::addAwesomeMarkers(
                .map,
                lng = areas[[nm]]$lon,
                lat = areas[[nm]]$lat,
                icon = leaflet::makeAwesomeIcon(
                  "circle",
                  library = "fa",
                  markerColor = areas[[nm]]$col,
                  iconColor = "black"
                )
              )
            } else if (areas[[nm]]$type == "rectangle") {
              .map <<- leaflet::addRectangles(
                .map,
                lat1 = areas[[nm]]$lat1,
                lat2 = areas[[nm]]$lat2,
                lng1 = areas[[nm]]$lon1,
                lng2 = areas[[nm]]$lon2,
                color = areas[[nm]]$col
              )
            } else if (areas[[nm]]$type == "polygon") {
              .map <<- leaflet::addPolygons(
                .map,
                lng = areas[[nm]]$lon,
                lat = areas[[nm]]$lat,
                color = areas[[nm]]$col
              )
            }
          })
        }
        
        .map
      })
      
      
      validate(
        need(class(.map) != "try-error", "Error in main_env$local_rv$custom")
      )
      
      .map
    })
    
    # Completeness ====
    columns_complete <- reactive({
      req(main_env$EAL$page == 5)
      req(is.reactive(main_env$local_rv$columns$complete))
      
      devmsg(main_env$local_rv$columns$complete())
      main_env$local_rv$columns$complete()
    })
    
    custom_complete <- reactive({
      req(main_env$EAL$page == 5)
      
      devmsg(main_env$local_rv$custom$complete())
      
      main_env$local_rv$custom$complete()
    })
    
    observe({
      req(main_env$EAL$page == 5)
      
      # trigger test
      .trig <- isTRUE(columns_complete()) || isTRUE(custom_complete())
      if (main_env$dev) {
        devmsg(tag = "Geocov", "completeness")
      }
      
      # Set full completeness according to selected method
      main_env$EAL$completed <- switch(main_env$local_rv$method,
                                       columns = isTRUE(tryCatch(columns_complete(), finally = FALSE)),
                                       custom = isTRUE(tryCatch(custom_complete(), finally = FALSE))
      )
    },
    label = "EAL5 module completed",
    priority = -1
    )
    
    # Remove inserted UI ----
    observeEvent(main_env$EAL$page, {
      req(main_env$EAL$old_page == 5) # left the GeoCov step
      .ids <- names(main_env$local_rv$custom)
      .ids <- .ids[!.ids %in% c("complete", "count")]
      sapply(paste0(.ids, "-box"), function(id) {
        removeUI(sprintf("#%s", session$ns(id)), immediate = TRUE)
      })
    },
    priority = 1
    )
  }) # end of server
}
