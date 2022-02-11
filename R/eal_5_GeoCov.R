#' @import shiny
#' @importFrom shinyWidgets materialSwitch
#'
#' @noRd
GeoCovUI <- function(id) {
  return(
    fluidPage(
      fluidRow(
        tags$p("You can fill in geographic coverage through two methods: either 
          by chosing variables from your files, or manually define it. Mixed 
          approach is not supported. Check the help above for more details.",
               tags$b("Only WGS84 is currently supported.")),
        # Method ====
        # - method choice itself
        tags$span(
          "columns",
          shinyWidgets::materialSwitch(
            NS(id, "method"),
            tags$h5("Method"),
            inline = TRUE
          ),
          "custom"
          #,
          # textOutput(
          #   NS(id, "selected_method"),
          #   inline = TRUE
          # )
        ), # end of method
        # Columns ====
        tags$div(
          id = NS(id, "columns_input"),
          tagList(
            # Site description
            uiOutput(NS(id, "site")),
            # Latitude
            uiOutput(NS(id, "latitude")),
            # Longitude
            uiOutput(NS(id, "longitude"))
          )
        ), # end of columns
        # Custom ====
        shinyjs::hidden(
          tags$div(
            id = NS(id, "custom_input"),
            fluidRow(
              column(
                7,
                shinyWidgets::actionBttn(
                  NS(id, "add"), "", icon("plus"), style = "simple", color="primary"
                ),
                tags$div(id="inserthere_eal5")
              ),
              column(
                5,
                leaflet::leafletOutput(NS(id, "leaflet"))
              )
            )
            # fluidRow(
            #   column(1, actionButton(NS(id, "addui"), "", icon("plus"))),
            #   column(11, tags$div(id = "inserthere_eal5"))
            # )
          ) 
        )# end of custom
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom shinyWidgets updateMaterialSwitch
#' @importFrom shinyjs toggle
#'
#' @noRd
GeoCov <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    if (main.env$dev){
      observeEvent(
        main.env$dev.browse(), 
        {
          if (main.env$current.tab() == "fill" &&
              main.env$EAL$page == 5) {
            browser()
          }
        },
        label = "EAL5: dev"
      )
    }
    
    # Setup ====
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page == 5)
      req(main.env$local.rv$method)
      devmsg(tag="Geocov", "load")
      
      shinyWidgets::updateMaterialSwitch(
        session,
        "method",
        switch(
          main.env$local.rv$method,
          columns = FALSE,
          custom = TRUE
        )
      )
      
      # Display correct UI
      shinyjs::toggle(
        "columns_input", 
        condition = main.env$local.rv$method == "columns"
      )
      
      shinyjs::toggle(
        "custom_input", 
        condition = main.env$local.rv$method == "custom"
      )
    },
    label = "EAL5 set method switch",
    priority = -1
    )
    
    # Method selection ====
    observeEvent(input$method, {
      req(main.env$EAL$page == 5)
      devmsg(tag="Geocov", "method")
      main.env$local.rv$method <- ifelse(input$method, "custom", "columns")
      
      shinyjs::toggle(
        "columns_input", 
        condition = main.env$local.rv$method == "columns"
      )
      
      shinyjs::toggle(
        "custom_input", 
        condition = main.env$local.rv$method == "custom"
      )
    },
    label = "EAL4: switch method"
    )
    
    # output$selected_method <- renderText(main.env$local.rv$method)
    
    # Variables input ====
    ## Set inputs ----
    # Site description
    output$site <- renderUI({
      req(main.env$EAL$page == 5)
      req(isContentTruthy(main.env$local.rv$columns$choices$sites))
      devmsg(tag="Geocov", "render site")
      
      # Site description
      selectInput(
        session$ns("site"),
        "Choose a column for site descriptions",
        choices = c("None" = "", main.env$local.rv$columns$choices$sites),
        selected = if(isTruthy(main.env$local.rv$columns$site$file) &&
                      isTruthy(main.env$local.rv$columns$site$col))
          main.env$local.rv$columns$choices$sites[[
            main.env$local.rv$columns$site$file
          ]][main.env$local.rv$columns$site$col]
      )
    })
    
    # Latitude
    output$latitude <- renderUI({
      req(main.env$EAL$page == 5)
      req(isContentTruthy(main.env$local.rv$columns$choices$coords))
      devmsg(tag="Geocov", "render latitude")
      
      selectizeInput(
        session$ns("latitude"),
        "Choose a column for latitude values",
        choices = c("None" = "", main.env$local.rv$columns$choices$coords),
        selected = if(isTruthy(main.env$local.rv$columns$lat$file) &&
                      isTruthy(main.env$local.rv$columns$lat$col))
          main.env$local.rv$columns$choices$coords[[
            main.env$local.rv$columns$lat$file
          ]][main.env$local.rv$columns$lat$col],
        options = list(maxItems = 2)
      )
    })
    
    # Longitude
    output$longitude <- renderUI({
      req(main.env$EAL$page == 5)
      req(isContentTruthy(main.env$local.rv$columns$choices$coords))
      devmsg(tag="Geocov", "render longitude")
      
      selectizeInput(
        session$ns("longitude"),
        "Choose a column for longitude values",
        choices = c("None" = "", main.env$local.rv$columns$choices$coords),
        selected = if(isTruthy(main.env$local.rv$columns$lon$file) &&
                      isTruthy(main.env$local.rv$columns$lon$col))
          main.env$local.rv$columns$choices$coords[[
            main.env$local.rv$columns$lon$file
          ]][main.env$local.rv$columns$lon$col],
        options = list(maxItems = 2)
      )
    })
    
    ## Get input ----
    # Site description
    observeEvent(input$site, {
      req(main.env$EAL$page == 5)
      devmsg(tag="Geocov", "input site")
      
      if (!isTruthy(input$site)) {
        main.env$local.rv$columns$site$col <- ""
        main.env$local.rv$columns$site$file <- ""
      } else {
        .tmp <- input$site |>
          strsplit("/", TRUE) |>
          unlist()
        main.env$local.rv$columns$site$col <- .tmp[2]
        main.env$local.rv$columns$site$file <- 
          main.env$local.rv$columns$choices$file <- .tmp[1]
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
      req(main.env$EAL$page == 5)
      devmsg(tag="Geocov", "input latitude")
      
      if (!isTruthy(input$latitude)) {
        main.env$local.rv$columns$lat$col <- ""
        main.env$local.rv$columns$lat$file <- ""
      } else {
        .tmp <- input$latitude |>
          strsplit("/", TRUE) |>
          unlist()
        main.env$local.rv$columns$lat$col <- .tmp[2]
        main.env$local.rv$columns$lat$file <-
          main.env$local.rv$columns$choices$file <- .tmp[1]
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
      req(main.env$EAL$page == 5)
      devmsg(tag="Geocov", "input longitude")
      
      if (!isTruthy(input$longitude)) {
        main.env$local.rv$columns$lon$col <- ""
        main.env$local.rv$columns$lon$file <- ""
      } else {
        .tmp <- input$longitude |>
          strsplit("/", TRUE) |>
          unlist()
        main.env$local.rv$columns$lon$col <- .tmp[2]
        main.env$local.rv$columns$lon$file <-
          main.env$local.rv$columns$choices$file <- .tmp[1]
      }
      
      # Add feedback
      checkFeedback(input, "longitude", type = "danger")
    },
    ignoreInit = TRUE,
    priority = 1,
    ignoreNULL = FALSE,
    label = "EAL5: get longitude"
    )
    
    # Fill custom ====
    # # * Setup ----
    # observeEvent(main.env$EAL$page, { # on load
    #   req(main.env$EAL$page == 5)
    #   
    #   if (dim(main.env$local.rv$custom$coordinates)[1] > 0)
    #     sapply(1:nrow(main.env$local.rv$custom$coordinates), function(.ind) {
    #       insertGeoCovInput(
    #         session$ns(as.character(-.ind)), # from -n to -1, NS-ed
    #         main.env,
    #         default = main.env$local.rv$custom$coordinates[.ind,]
    #       )
    #     })
    # },
    # priority = -1,
    # label = "EAL5: set custom UI"
    # )
    # 
    # observeEvent(main.env$EAL$page, { # on load
    #   req(main.env$EAL$old.page == 5)
    #   
    #   if (dim(main.env$local.rv$custom$coordinates)[1] > 0)
    #     sapply(1:nrow(main.env$local.rv$custom$coordinates), function(.ind) {
    #       
    #       removeUI(
    #         sprintf("#%s-container", session$ns(as.character(-.ind)))
    #       )
    #     })
    # },
    # priority = 1,
    # label = "EAL5: remove custom UI"
    # )
    # 
    # # * Manage input ----
    # observeEvent(input$addui, {
    #   insertGeoCovInput(
    #     session$ns(as.numeric(input$addui)), # from 1 to n, NS-ed
    #     main.env
    #   )
    # },
    # label = "EAL5: get custom"
    # )
    # 
    # 
    
    # Custom server =====
    ### Setup ----
    # TODO !!!
    
    ### Click add ----
    observeEvent(input$add, {
      req(input$add)
      req(main.env$EAL$page == 5)
      devmsg(tag="Geocov", "custom add")
      
      main.env$local.rv$custom$count <<- main.env$local.rv$custom$count+1
      
      ## proper insert
      insertCustomGeoCov(
        session$ns(as.character(main.env$local.rv$custom$count)),
        main.env
      )
    },
    ignoreInit = TRUE)
    
    ### Get leaflet drawing ----
    observeEvent(input$leaflet_draw_new_feature, {
      req(main.env$EAL$page == 5)
      devmsg(tag="Geocov", "leaflet draw")
      
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
            tail(-1)
        }
      )
      main.env$local.rv$custom[[as.character(.nm)]]$points <- .points
      
      insertCustomGeoCov(
        session$ns(as.character(main.env$local.rv$custom$count)),
        main.env
      )
    })
    
    ## Render leaflet ====
    
    #### Output ----
    output$leaflet <- leaflet::renderLeaflet({
      req(main.env$EAL$page == 5)
      devmsg(tag="Geocov", "leaflet render")
      
      ###### Get names ----
      .nms <- names(main.env$local.rv$custom)[
        sapply(
          names(main.env$local.rv$custom),
          function(n) 
            n != "count" &&
            n != "complete" &&
            isContentTruthy(main.env$local.rv$custom[[n]])
        )
      ]
      
      #### Get values ----
      areas <- lapply(.nms, function(id) {
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
      
      #### Render map ----
      .map <- leaflet::leaflet("geocov") |>
        leaflet::addTiles() |>
        leaflet.extras::addDrawToolbar(
          polylineOptions = FALSE,
          circleOptions = FALSE,
          circleMarkerOptions = FALSE
        )
      
      if(isContentTruthy(areas)){
        .nms <- names(areas)
        sapply(names(areas), function(nm) {
          if(areas[[nm]]$type == "marker") {
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
          } else if(areas[[nm]]$type == "rectangle") {
            .map <<- leaflet::addRectangles(
              .map,
              lat1=areas[[nm]]$lat1,
              lat2=areas[[nm]]$lat2,
              lng1=areas[[nm]]$lon1,
              lng2=areas[[nm]]$lon2,
              color=areas[[nm]]$col
            )
          } else if(areas[[nm]]$type == "polygon") {
            .map <<- leaflet::addPolygons(
              .map,
              lng = areas[[nm]]$lon,
              lat = areas[[nm]]$lat,
              color=areas[[nm]]$col
            )
          }
          
        })
      }
      
      .map
    })
    
    # Saves ====
    observe({
      req(main.env$EAL$page == 5)
      invalidateLater(1000)
      req(main.env$local.rv$method %in% c("columns", "custom"))
      devmsg(tag="Geocov", "saves")
      
      # Set full completeness according to selected method
      main.env$EAL$completed <- switch(
        main.env$local.rv$method,
        columns = isTRUE(main.env$local.rv$columns$complete()),
        custom = isTRUE(main.env$local.rv$custom$complete())
      )
    },
    label = "EAL5 module completed"
    )
    
  }) # end of server
}
