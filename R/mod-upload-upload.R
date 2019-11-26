uploadUI <- function(id, dev) {
  ns <- NS(id)
  registeredEndpoints <- fread("resources/registeredEndpoints.txt")
  
  # TODO use `runjs` from shinyjs to update css : https://stackoverflow.com/questions/46045222/reactive-css-properties-in-r-shiny
  
  tagList(
    if(dev) actionButton(ns("dev"),"Dev"),
    tags$hr(),
    # select endpoint ----
    tags$h3("Select your MetaCat portal"),
    div(
      tags$p("'dev' portals are under construction. No guarantee is given of their consistance.
           'stable' portals are completely functional.
           Chosing 'Other' will ask you to input some technical information."),
      selectInput(ns("endpoint"),"Available metacats:",c(registeredEndpoints$mn, "Other")),
      uiOutput(ns("actual-endpoint")),
      "Want to be listed? get in touch with the dev team via Github !",
      style = "border-left: solid lightgrey; padding: 20px;" # TODO make it work via style.R
    ),
    tags$hr(),
    
    # check authentication token ----
    tags$h3("Get your authentication token"),
    div(
      tags$p("The authentication token must be set in the MetaShARK options."),
      style = "border-left: solid lightgrey; padding: 20px;"
    ),
    tags$hr(),
    
    # files input ----
    tags$h3("Select your data, script and metadata files"),
    div(
      tags$h4("Metadata (one file expected)"),
      multiFIlesInputUI(ns("metadata"), "Please select an .xml file validating EML schema."),
      textOutput(ns("warnings-metadata")),
      tags$h4("Data (at least one file expected)"),
      multiFIlesInputUI(ns("data"), "Please select the data described in the provided metadata."),
      textOutput(ns("warnings-data")),
      tags$h4("Scripts"),
      multiFIlesInputUI(ns("scripts"), "Please select the scripts described in the provided metadata."),
      textOutput(ns("warnings-scripts")),
      style = "border-left: solid lightgrey; padding: 20px;"
    ),
    tags$hr(),
    
    # constraints? ----
    tags$h3("Add constraints between your files"),
    # select Subject
    # select Property (rdf format)
    # select Target
    
    actionButton(ns("process"), "Process", icon = icon("rocket"))
  )
}

#' @importFrom shinyjs enable disable
upload <- function(input, output, session, dev) {
  ns <- session$ns
  registeredEndpoints <- fread("resources/registeredEndpoints.txt")
  
  if(dev)
    observeEvent(input$dev, {
      browser()
    })
  
  # Select endpoint ----
  endpoint = reactive({ input$endpoint })
  
  output$`actual-endpoint` <- renderUI({
    if(endpoint() == "Other")
      textInput(ns("actual-endpoint"), "Write the URL of the Member Node", 
                placeholder = "https://openstack-192-168-100-67.genouest.org/metacat/d1/mn/v2/")
    else
      tags$p(paste("Current endpoint:", registeredEndpoints %>% dplyr::filter(mn == endpoint()) %>% dplyr::select(URL)))
  })
  
  memberNode <- reactive({
    if(endpoint() != "Other")
      registeredEndpoints[match(endpoint(), registeredEndpoints$mn), "URL"]
    else
      input$`actual-endpoint`
  })
  
  # Token input ----
  observe({
    if(!is.character(options("dataone_token")) ||
       !is.character(options("dataone_test_token")) ||
       ( is.null(options("dataone_token")) && is.null(options("dataone_test_token")) )
    ){
      output$token_status <- renderUI({tags$div("UNFILLED", class = "danger")})
      disable("process")
    }
    else{
      output$token_status <- renderUI({tags$div("FILLED", class = "valid")})
      enable("process")
    }
  })
  
  # Files input ----
  rvFiles = reactiveValues(
    md = callModule(multiFIlesInput,"metadata"),
    data = callModule(multiFIlesInput,"data"),
    scr = callModule(multiFIlesInput,"scripts")
  )
  
  observe({
    # browser()
    if(
      dim(rvFiles$md())[1] != 1 ||
      dim(rvFiles$data())[1] < 1
    )
      disable("process")
    else
      enable("process")
  })
  
  # Constraints ----
  
  
  # Process ----
  observeEvent(input$process,{
    disable("process")
    uploadDP(mn = as.character(registeredEndpoints %>% dplyr::filter(mn == endpoint()) %>% dplyr::select(URL)),
             cn = as.character(registeredEndpoints %>% dplyr::filter(mn == endpoint()) %>% dplyr::select(cn)),
             eml = rvFiles$md()$datapath,
             data = rvFiles$data()$datapath)
    enable("process")
  })
  
}