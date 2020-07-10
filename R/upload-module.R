#' @title uploadUI
#'
#' @description UI part for the upload module. Used to build and drop
#' data packages to a chosen metacat.
#'
#' @param id shiny module id
#'
#' @import shiny
#' @importFrom data.table fread
uploadUI <- function(id, main.env) {
  ns <- NS(id)
  dev <- main.env$dev
  
  registeredEndpoints <- data.table::fread(
    isolate(main.env$PATHS$resources)$registeredEndpoints.txt
  )
  dp.list <- list.files(
    "~/dataPackagesOutput/emlAssemblyLine/",
    pattern = "_emldp$",
    full.names = TRUE
  )
  names(dp.list) <- sub("_emldp", "",
    list.files(
      "~/dataPackagesOutput/emlAssemblyLine/",
      pattern = "_emldp$"
    )
  )
  
  # TODO add update module
  
  tagList(
    tabsetPanel(
      id = "upload",
      # Upload -----------------------------------------------------
      tabPanel(
        title = "upload",
        if (dev) actionButton(ns("dev"), "Dev"),
        # select endpoint -----------------------------------------------------
        tags$h3("Select your MetaCat portal"),
        tags$div(
          tags$p(tags$code("dev"), "portals are under construction. No guarantee is given of
            their consistance.", tags$code("prod"), "portals are completely functional.
            Chosing 'Other' will ask you to input some technical information."),
          selectInput(
            ns("endpoint"),
            "Available metacats:",
            c(registeredEndpoints$mn, "Other")
          ),
          uiOutput(ns("actual-endpoint")),
          tagList(
            tags$p("Want your endpoint to be listed? get in touch with the dev team !")
          ),
          class = "leftMargin inputBox"
        ),
        tags$hr(),
        # check authentication token -----------------------------------------------------
        tags$h3("Get your authentication token"),
        tags$div(
          tags$p("The ", tags$b("authentication token"), " is a user-specific characters key.
            It allows the user to authenticate a connection between its current location and
            a distant server, actually the metadata catalog. To upload a data package, 
            the authentication token is required."),
          actionButton(ns("toSettings"), "Go to settings", icon("gear")),
          class = "leftMargin inputBox"
        ),
        
        # files input -----------------------------------------------------
        tags$h3("Select your data package files"),
        tags$div(
          # data package input
          tags$p("You can either select a data package from 
          ~/dataPackagesOutput/emlAssemblyLine/ or pick up the files one by one. 
          Selecting a data package will erase any previous selection."),
          fluidRow(
            column(9,
              # * DP ====
              tags$h4("Select a data package"),
              selectInput(
                ns("DP"),
                "Data package",
                choices = c(
                  None = "",
                  dp.list
                ),
                multiple = FALSE
              ),
              # individual files inputs
              tags$h4("Add or remove files"),
              # * Metadata ====
              fileInput(
                ns("metadata"),
                "EML-valid file (only one allowed)"
              ),
              textOutput(ns("warnings-metadata")),
              # * Data ====
              fileInput(
                ns("data"),
                "Data files described in your EML file"
              ),
              textOutput(ns("warnings-data")),
              # * Scripts ====
              fileInput(
                ns("scripts"),
                "Scripts used to produce or process data"
              ),
              textOutput(ns("warnings-scripts"))
            ),
            column(3,
              tags$h4("Files list"),
              uiOutput(ns("filesList"))
            )
          ),
          class = "leftMargin inputBox"
        ),
        
        # Constraints -----------------------------------------------------
        # div(id="constraints_div",
        #     tags$h4("Add constraints between script and data files"),
        #     actionButton(ns("add_constraint"), "", icon = icon("plus"), width = "40px")
        # ),
        # tags$hr(),
        
        actionButton(
          ns("process"),
          "Process",
          icon = icon("rocket"),
          width = "100%"
        )
      ), # end of upload tab
      # Update -----------------------------------------------------
      tabPanel(
        title = "update",
        tags$div(
          "WIP",
          # 1. solr query 
          # 2. select items to update
          # 3. select files
          class = "inputBox wip"
        )
      ) # end of update tab
    ) # end of tabSetPanel
  ) # end of tagList
}

#' @title upload
#'
#' @describeIn uploadUI
#'
#' @param main.env inner global environment
#'
#' @import shiny
#' @importFrom dplyr filter select %>%
#' @importFrom shinyjs enable disable click
#' @importFrom data.table fread fwrite
#' @importFrom mime guess_type
upload <- function(input, output, session, main.env) {
  ns <- session$ns
  
  registeredEndpoints <- data.table::fread(main.env$PATHS$resources$registeredEndpoints.txt)
  dev <- main.env$dev
  
  # Select endpoint ----
  endpoint <- reactive({
    input$endpoint
  })
  
  memberNode <- reactive({
    if (endpoint() != "Other") {
      registeredEndpoints %>%
        dplyr::filter(mn == endpoint()) %>%
        dplyr::select(URL)
    } else {
      callModule(URL_Input, "actual-endpoint")
    }
  })
  
  output$`actual-endpoint` <- renderUI({
    if (endpoint() != "Other") {
      tags$p(tags$b("Current endpoint:"), memberNode())
    } else {
      URL_Input_UI(ns("actual-endpoint"), "Write the URL of the Member Node")
    }
  })
  
  # Token input -----------------------------------------------------
  observeEvent(input$toSettings, {
    shinyjs::click("appOptions", asis = TRUE)
  }, ignoreInit = TRUE)
  
  observe({
    if (!is.character(options("dataone_token")) ||
        !is.character(options("dataone_test_token")) ||
        (is.null(options("dataone_token")) && is.null(options("dataone_test_token")))
    ) {
      output$token_status <- renderUI({
        tags$div("UNFILLED", class = "danger")
      })
      shinyjs::disable("process")
    }
    else {
      output$token_status <- renderUI({
        tags$div("FILLED", class = "valid")
      })
      shinyjs::enable("process")
    }
  })
  
  # Files input -----------------------------------------------------
  rv <- reactiveValues(
    md = data.frame(stringsAsFactors = FALSE),
    data = data.frame(stringsAsFactors = FALSE),
    scr = data.frame(stringsAsFactors = FALSE)
  )
  
  observeEvent(input$DP, {
    .dir <- gsub("/+", "/", input$DP)
    .id <- basename(.dir) %>% sub("_emldp$", "", .)
    .eml.files <- sprintf("%s/%s/eml", .dir, .id) %>% 
      dir(full.names = TRUE)
    rv$md <- data.frame(
      name = basename(.eml.files),
      size = base::file.size(.eml.files),
      type = mime::guess_type(.eml.files),
      datapath= .eml.files
    )
    
    .data.files <- sprintf("%s/%s/data_objects", .dir, .id) %>% 
      dir(full.names = TRUE)
    rv$data <- data.frame(
      name = basename(.data.files),
      size = base::file.size(.data.files),
      type = mime::guess_type(.data.files),
      datapath = .data.files
    )
  }, 
    ignoreInit = TRUE,
    label = "DPinput"
  )
  
  observeEvent(input$metadata,{
    rv$md <- input$metadata
    showNotification(
      "Only one metadata file allowed",
      type = "message"
    )
  })
  observeEvent(input$data,{
    .add <- input$data
    req(checkTruth(.add))
    browser() # Update list instead of erasing
    rv$data <- rbind(rv$data, .add)
  })
  observeEvent(input$scripts,{
    .add <- input$scripts
    req(checkTruth(.add))
    browser() # Update list instead of erasing
    rv$scr <- rbind(input$scripts, .add)
  })
  
  output$filesList <- renderUI({
    validate(
      need(
        dim(rv$md) > 0 ||
          dim(rv$data) > 0 ||
          dim(rv$scr) > 0,
        "No file selected"
      )
    )
    
    tagList(
      if(dim(rv$md)[1] > 0)
        checkboxGroupInput(
          ns("md-files"),
          label = "EML file",
          choices = rv$md$name
        ),
      if(dim(rv$data)[1] > 0)
        checkboxGroupInput(
          ns("data-files"),
          label = "Data files",
          choices = rv$data$name
        ),
      if(dim(rv$scr)[1] > 0)
        checkboxGroupInput(
          ns("scr-files"),
          label = "Scripts",
          choices = rv$scr$name
        ),
      actionButton(ns("rmv"), "Remove", class = "danger")
    )
  })
  
  observeEvent(input$rmv, {
    .rmv <- input$`md-files`
    if(checkTruth(.rmv)){
      .ind <- match(.rmv, rv$md$name)
      rv$md <- rv$md[-.ind,]
    }
    .rmv <- input$`data-files`
    if(checkTruth(.rmv)){
      .ind <- match(.rmv, rv$data$name)
      rv$data <- rv$data[-.ind,]
    }
    .rmv <- input$`scr-files`
    if(checkTruth(.rmv)){
      .ind <- match(.rmv, rv$scr$name)
      rv$scr <- rv$scr[-.ind,]
    }
  }, ignoreInit = TRUE)
  
  observe({
    if (
      dim(rv$md)[1] != 1 ||
        dim(rv$data)[1] < 1
    ) {
      shinyjs::disable("process")
    } else {
      shinyjs::enable("process")
    }
    
    if (
      dim(rv$scr)[1] == 0 ||
        dim(rv$data)[1] == 0
    ) {
      shinyjs::disable("add_constraint")
    } else {
      shinyjs::enable("add_constraint")
    }
  })
  
  # Process -----------------------------------------------------
  observeEvent(input$process, {
    disable("process")
    
    md.format <- EML::read_eml(as.character(rv$md$datapath))$schemaLocation %>%
      strsplit(split = " ") %>%
      unlist %>%
      utils::head(n = 1)
    
    out <- uploadDP(
      mn = registeredEndpoints %>%
        dplyr::filter(mn == endpoint()) %>%
        dplyr::select(URL) %>%
        as.character,
      cn = registeredEndpoints %>%
        dplyr::filter(mn == endpoint()) %>%
        dplyr::select(cn) %>%
        as.character,
      token = list(
        test = main.env$SETTINGS$metacat.test,
        prod = main.env$SETTINGS$metacat.token
      ),
      eml = list(
        file = rv$md$datapath,
        format = md.format
      ),
      data = list(
        file = rv$data$datapath,
        format = mime::guess_type(rv$data$datapath)
      ),
      scripts = if (dim(rv$scr)[1] > 0) {
        list(
          file = rv$scr$datapath,
          format = mime::guess_type(rv$scr$datapath)
        )
      } else {
        c()
      },
      formats = main.env$FORMAT$dataone.list$MediaType,
      use.doi = FALSE
    )
    
    if (class(out) == "try-error") {
      showNotification(out[1], type = "error")
    } else {
      showNotification(paste("Uploaded DP", out), type = "message")
    }
    
    shinyjs::enable("process")
  })
}
