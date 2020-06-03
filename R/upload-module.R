#' @title uploadUI
#'
#' @description UI part for the upload module. Used to build and drop
#' data packages to a chosen metacat.
#'
#' @param id shiny module id
#'
#' @importFrom shiny NS tagList actionButton tags selectInput
#' uiOutput textOutput icon tabsetPanel tabPanel HTML
#' @importFrom data.table fread
uploadUI <- function(id, dev, globals, server) {
  ns <- NS(id)
  registeredEndpoints <- fread(globals$PATHS$registeredEndpoints.txt)
  # registeredEndpoints <- fread(system.file("resources", "registeredEndpoints.txt", package = "MetaShARK"))

  # TODO use `runjs` from shinyjs to update css : https://stackoverflow.com/questions/46045222/reactive-css-properties-in-r-shiny
  # TODO add update module

  tagList(
    tabsetPanel(
      id = "upload",
      # Upload -----------------------------------------------------
      tabPanel(
        title = "upload",
        if (dev) actionButton(ns("dev"), "Dev"),
        tags$hr(),
        # select endpoint -----------------------------------------------------
        tags$h3("Select your MetaCat portal"),
        tags$div(
          tags$p("'dev' portals are under construction. No guarantee is given of their consistance.
               'stable' portals are completely functional.
               Chosing 'Other' will ask you to input some technical information."),
          selectInput(
            ns("endpoint"),
            "Available metacats:",
            c(registeredEndpoints$mn, "Other")
          ),
          uiOutput(ns("actual-endpoint")),
          "Want to be listed? get in touch with the dev team via Github !",
          class = "leftMargin"
        ),
        tags$hr(),

        # check authentication token -----------------------------------------------------
        tags$h3("Get your authentication token"),
        tags$div(
          tags$p("The ", tags$b("authentication token"), " is a user-specific characters key.
            It allows the user to authenticate a connection between its current location and
            a distant server, actually the metadata catalog. To upload a data package, 
            the authentication token is required."),
          class = "leftMargin"
        ),
        tags$hr(),

        # files input -----------------------------------------------------
        tags$h3("Select your data, script and metadata files"),
        HTML("Either pick <b>individual files</b> (left) or a complete
          <b>EAL data package</b> (right)."),
        # individual files inputs
        tags$div(
          # Metadata
          tags$h4("Metadata (one file required)"),
          multiFilesInputUI(
            ns("metadata"), 
            "Please select an .xml file validating EML schema.",
            server = server
          ),
          textOutput(ns("warnings-metadata")),
          # Data
          tags$h4("Data (at least one file required)"),
          multiFilesInputUI(
            ns("data"), 
            "Please select the data described in the provided metadata.",
            server = server
          ),
          textOutput(ns("warnings-data")),
          # Scripts
          tags$h4("Scripts"),
          multiFilesInputUI(
            ns("scripts"), 
            "Please select the scripts described in the provided metadata.",
            server = server
          ),
          textOutput(ns("warnings-scripts")),
          class = "leftMargin"
        ),
        # DP input
        # column(6,
        #   tags$div(
        #     uiOutput(ns("EAL_dp")),
        #     class = "leftMargin"
        #   )
        # ),
        tags$hr(),

        # Constraints -----------------------------------------------------
        # div(id="constraints_div",
        #     tags$h4("Add constraints between script and data files"),
        #     actionButton(ns("add_constraint"), "", icon = icon("plus"), width = "40px")
        # ),
        # tags$hr(),

        actionButton(ns("process"), "Process",
          icon = icon("rocket"),
          width = "100%"
        )
      ), # end of upload tab
      # Update -----------------------------------------------------
      tabPanel(
        title = "update",
        # 1. solr query -----------------------------------------------------

        # 2. select items to update -----------------------------------------------------

        # 3. select files -----------------------------------------------------
      ) # end of update tab
    ) # end of tabSetPanel
  ) # end of tagList
}

#' @title upload
#'
#' @describeIn uploadUI
#'
#' @param globals inner variable
#'
#' @importFrom shiny observeEvent reactive textInput tags
# observe renderUI reactiveValues callModule showNotification
#' @importFrom dplyr filter select %>%
#' @importFrom shinyjs enable disable
#' @importFrom data.table fread fwrite
#' @importFrom mime guess_type
upload <- function(input, output, session, globals, server) {
  ns <- session$ns

  registeredEndpoints <- fread(globals$PATHS$registeredEndpoints.txt)
  dev <- globals$dev

  # Select endpoint -----------------------------------------------------
  endpoint <- reactive({
    input$endpoint
  })

  output$`actual-endpoint` <- renderUI({
    if (endpoint() == "Other") {
      textInput(ns("actual-endpoint"), "Write the URL of the Member Node",
        placeholder = "https://openstack-192-168-100-67.genouest.org/metacat/d1/mn/v2/"
      )
    } else {
      tags$p(paste("Current endpoint:", registeredEndpoints %>% filter(mn == endpoint()) %>% select(URL)))
    }
  })

  memberNode <- reactive({
    if (endpoint() != "Other") {
      registeredEndpoints %>%
        filter(mn == endpoint()) %>%
        select(URL)
    } else {
      input$`actual-endpoint`
    }
  })

  # Token input -----------------------------------------------------
  observe({
    if (!is.character(options("dataone_token")) ||
      !is.character(options("dataone_test_token")) ||
      (is.null(options("dataone_token")) && is.null(options("dataone_test_token")))
    ) {
      output$token_status <- renderUI({
        tags$div("UNFILLED", class = "danger")
      })
      disable("process")
    }
    else {
      output$token_status <- renderUI({
        tags$div("FILLED", class = "valid")
      })
      enable("process")
    }
  })

  # * Files input -----------------------------------------------------
  rvFiles <- reactiveValues(
    md = callModule(multiFilesInput, "metadata", server = server),
    data = callModule(multiFilesInput, "data", server = server),
    scr = callModule(multiFilesInput, "scripts", server = server)
  )

  observe({
    if (
      dim(rvFiles$md())[1] != 1 ||
        dim(rvFiles$data())[1] < 1
    ) {
      disable("process")
    } else {
      enable("process")
    }

    if (
      dim(rvFiles$scr())[1] == 0 ||
        dim(rvFiles$data())[1] == 0
    ) {
      disable("add_constraint")
    } else {
      enable("add_constraint")
    }
  })

  # * DP input -----------------------------------------------------
  # output$EAL_dp <- renderUI({
  #   # get EAL completed data packages list
  #   dp_list <- sapply(
  #     list.files(
  #       globals$DEFAULT.PATH,
  #       pattern = "_emldp$",
  #       full.names = TRUE
  #     ),
  #     function(dp){
  #       dp_name <- gsub("_emldp$", "", basename(dp))
  #       dp_eml_path <- paste(
  #         dp,
  #         dp_name,
  #         "eml",
  #         sep = "/"
  #       )
  #       if(isTruthy(dir(dp_eml_path)))
  #         return(dp)
  #       else
  #         return(NULL)
  #     }
  #   )
  #
  #   validate(
  #     need(
  #       isTruthy(dp_list),
  #       paste(
  #         "No completed EAL data package at:",
  #         globals$DEFAULT.PATH
  #       )
  #     )
  #   )
  #
  #   # generate select input
  #   selectInput(
  #     session$ns("EAL_dp_select"),
  #     "Select an EAL completed Data package",
  #     basename(dp_list),
  #     multiple = FALSE
  #   )
  # })
  #
  # observeEvent(input$EAL_dp_select, {
  # TODO get all interesting files of the dp
  # })

  # Process -----------------------------------------------------
  observeEvent(input$process, {
    disable("process")

    md_format <- read_eml(rvFiles$md()$datapath)$schemaLocation %>%
      strsplit(split = " ") %>%
      unlist() %>%
      head(n = 1)

    out <- uploadDP(
      mn = as.character(registeredEndpoints %>% filter(mn == endpoint()) %>% select(URL)),
      cn = as.character(registeredEndpoints %>% filter(mn == endpoint()) %>% select(cn)),
      token = list(
        test = globals$TOKEN$DATAONE.TEST.TOKEN,
        prod = globals$TOKEN$DATAONE.TOKEN
      ),
      eml = list(
        file = rvFiles$md()$datapath,
        format = md_format
      ),
      data = list(
        file = rvFiles$data()$datapath,
        format = guess_type(rvFiles$data()$datapath)
      ),
      scripts = if (dim(rvFiles$scr())[1] > 0) {
        list(
          file = rvFiles$scr()$datapath,
          format = guess_type(rvFiles$scr()$datapath)
        )
      } else {
        c()
      },
      formats = globals$FORMAT$DATAONE$MediaType,
      use.doi = FALSE
    )

    if (class(out) == "try-error") {
      showNotification(out[1], type = "error")
    } else {
      showNotification(paste("Uploaded DP", out), type = "message")
    }

    enable("process")
  })
}
