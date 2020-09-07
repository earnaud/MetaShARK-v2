#' @import shiny
#' @importFrom shinyjs hidden
#'
#' @noRd
GeoCovUI <- function(id, main.env) {
  return(
    fluidPage(
      fluidRow(
        tags$p("You can fill in geographic coverage through two methods: either 
          by chosing variables from your files, or manually define it."),
        fluidRow(
          column(
            6, tags$h5("Variable selection"),
            tags$p("Prefer storing all of your locations in a single file, under
              3 to 5 columns: one for the site description and one or two others
              for latitude and longitude. Southern latitude and western longitude
              shall be noted with negative values."
            )
          ),
          column(
            6, tags$h5("Manual geographic coverage"),
            HTML("You can detail a more precise number by using the left/right
              (or down/up) arrows of your keyboard. Precision can be given at
              0.01 &#176.") # Items can be removed by using the &#9003 Delete key.")
          )
        ),
        # Method ====
        # - method choice itself
        tags$span(
          shinyWidgets::materialSwitch(
            NS(id, "method"),
            "Method",
            inline = TRUE
          ),
          textOutput(
            NS(id, "selected_method"),
            inline = TRUE
          )
        ),
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
        ),
        # Custom ====
        tags$div(
          id = NS(id, "custom_input"),
          fluidRow(
            column(2, actionButton(NS(id, "addui"), "", icon("plus"))),
            column(10, tags$div(id = "inserthere"))
          )
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
  moduleServer(id, function(input, output, session) {
    # Variable initialization (deprecated)

    # Input management ====
    # * Method selection ----
    observeEvent(input$method, {
      req(input$method)
      
      main.env$local.rv$method = ifelse(
        input$method,
        "custom",
        "columns"
      )
      
      shinyjs::toggle(
        "columns_input", 
        condition = main.env$local.rv$method == "columns"
      )
      
      shinyjs::toggle(
        "custom_input", 
        condition = main.env$local.rv$method == "custom"
      )
    })
    
    output$selected_method <- renderText({
      main.env$local.rv$method
    })
    
    # * Set inputs ----
    # Site description
    output$site <- renderUI({
      req(main.env$EAL$page == 5)
      req(isContentTruthy(main.env$local.rv$columns$choices$sites))
      
      # Site description
      selectInput(
        session$ns("site"),
        "Choose a column for site descriptions",
        choices = c("None" = "", main.env$local.rv$columns$choices$sites)
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
    },
    ignoreInit = TRUE,
    priority = 1,
    ignoreNULL = FALSE,
    label = "EAL5: get longitude"
    )

    # Fill custom ====
    # * Setup ----
    observeEvent(main.env$local.rv$custom$coordinates, {
      if (dim(main.env$local.rv$custom$coordinates)[1] > 0)
        sapply(1:nrow(main.env$local.rv$custom$coordinates), function(.ind) {
          insertGeoCovInput(
            session$ns(as.numeric(-.ind)), # from -n to -1, NS-ed
            main.env,
            default = main.env$local.rv$custom$coordinates[.ind]
          )
        })
    },
    label = "EAL5: set custom"
    )

    # * Manage input ----
    observeEvent(input$addui, {
      shinyjs::show("slider_tips")
      main.env$local.rv$custom <- insertGeoCovInput(
        session$ns(as.numeric(input$addui)), # from 1 to n, NS-ed
        main.env
      )
    },
    label = "EAL5: get custom"
    )

    # Saves ----
    observeEvent(
      {
        main.env$local.rv$columns$site$col
        main.env$local.rv$columns$lat$col
        main.env$local.rv$columns$lon$col
      },
      {
        main.env$local.rv$columns$complete <- 
          isTruthy(main.env$local.rv$columns$site$col) &&
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
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$old.page == 5)
      
      # * Previous ----
      if(main.env$EAL$page < main.env$EAL$old.page) {
        if (!"Categorical Variables" %in% main.env$EAL$history) {
          isolate({main.env$EAL$page <- main.env$EAL$page - 1})
        }
      }
      
      # * Next ----
      if(main.env$EAL$page > main.env$EAL$old.page) {
        message(NS(id, "proceed"))
        
        # Create modal
        choices <- c(
          "Columns selection" = if (main.env$local.rv$columns$complete) "columns" else NULL,
          "Custom edition" = if (main.env$local.rv$custom$complete) "custom" else NULL
        )
        
        req(isTruthy(choices))
        
        nextTabModal <- modalDialog(
          title = "Proceed Geographic Coverage",
          tagList(
            "You are getting ready to proceed. 
            Please confirm your method for templating geographic coverage:",
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
      }
    },
    priority = 1,
    label = "EAL5: process back",
    ignoreInit = TRUE
    )

    observeEvent(input$confirm,
      {
        removeModal()
        main.env$EAL$old.page <- main.env$EAL$page
        main.env$EAL$page <- 6
        main.env$EAL$tag.list <- tagList()

        .method <- input$method

        if (.method == "columns") {
          main.env$local.rv$custom$complete <- FALSE
        }
        if (.method == "custom") {
          main.env$local.rv$columns$complete <- FALSE
        }
        
        saveReactive(main.env, main.env$EAL$old.page)
      },
      label = "EAL5: confirm process data",
      ignoreInit = TRUE
    )
  })
}
