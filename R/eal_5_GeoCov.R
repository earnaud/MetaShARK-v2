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
          shinyWidgets::materialSwitch(
            NS(id, "method"),
            tags$h5("Method"),
            inline = TRUE
          ),
          textOutput(
            NS(id, "selected_method"),
            inline = TRUE
          )
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
              column(1, actionButton(NS(id, "addui"), "", icon("plus"))),
              column(11, tags$div(id = "inserthere_eal5"))
            )
          ) 
        )# end of custom
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom shinyWidgets updateMaterialSwitch
#' @importFrom shinyjs toggle
#' @importFrom dplyr %>% 
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
    
    observe({
      req(main.env$EAL$page == 5)
      main.env$EAL$page.load$depend()
      req(main.env$local.rv$method)
      
      shinyWidgets::updateMaterialSwitch(
        session,
        "method",
        switch(
          main.env$local.rv$method,
          columns = FALSE,
          custom = TRUE
        )
      )
    },
    label = "EAL5 set method switch"
    )

    # Method selection ====
    observeEvent(input$method, {
      req(main.env$EAL$page == 5)
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
    label = "EAL4: switch method")
    
    output$selected_method <- renderText(main.env$local.rv$method)
    
    # Variables input ====
    # * Set inputs ----
    # Site description
    output$site <- renderUI({
      req(main.env$EAL$page == 5)
      req(isContentTruthy(main.env$local.rv$columns$choices$sites))
      
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
    
    # * Get input ----
    # Site description
    observeEvent(input$site, {
      req(main.env$EAL$page == 5)
      
      if (!isTruthy(input$site)) {
        main.env$local.rv$columns$site$col <- ""
        main.env$local.rv$columns$site$file <- ""
      } else {
        .tmp <- input$site %>%
          strsplit(., "/", TRUE) %>%
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
      
      if (!isTruthy(input$latitude)) {
        main.env$local.rv$columns$lat$col <- ""
        main.env$local.rv$columns$lat$file <- ""
      } else {
        .tmp <- input$latitude %>%
          strsplit(., "/", TRUE) %>%
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
      
      if (!isTruthy(input$longitude)) {
        main.env$local.rv$columns$lon$col <- ""
        main.env$local.rv$columns$lon$file <- ""
      } else {
        .tmp <- input$longitude %>%
          strsplit(., "/", TRUE) %>%
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
    # * Setup ----
    observeEvent(main.env$EAL$page, { # on load
      req(main.env$EAL$page == 5)
      
      if (dim(main.env$local.rv$custom$coordinates)[1] > 0)
        sapply(1:nrow(main.env$local.rv$custom$coordinates), function(.ind) {
          insertGeoCovInput(
            session$ns(as.character(-.ind)), # from -n to -1, NS-ed
            main.env,
            default = main.env$local.rv$custom$coordinates[.ind]
          )
        })
    },
    priority = -1,
    label = "EAL5: set custom UI"
    )
    
    observeEvent(main.env$EAL$page, { # on load
      req(main.env$EAL$old.page == 5)
      
      if (dim(main.env$local.rv$custom$coordinates)[1] > 0)
        sapply(1:nrow(main.env$local.rv$custom$coordinates), function(.ind) {
          
          removeUI(
            sprintf("#%s-container", session$ns(as.character(-.ind)))
          )
        })
    },
    priority = 1,
    label = "EAL5: remove custom UI"
    )

    # * Manage input ----
    observeEvent(input$addui, {
      insertGeoCovInput(
        session$ns(as.numeric(input$addui)), # from 1 to n, NS-ed
        main.env
      )
    },
    label = "EAL5: get custom"
    )

    # Saves ====
    observe({
      req(main.env$EAL$page == 5)
      invalidateLater(1000)
      req(main.env$local.rv$method %in% c("columns", "custom"))
      
      # Set full completeness according to selected method
      main.env$EAL$completed <- switch(
        main.env$local.rv$method,
        columns = isTRUE(main.env$local.rv$columns$complete()),
        custom = isTRUE(main.env$local.rv$custom$complete())
      )
    },
    label = "EAL5 module completed"
    )

    # Process data (deprecated)
  }) # end of server
}
