#' @title uploadUI
#' 
#' @description UI part for the upload module. Used to build and drop 
#' data packages to a chosen metacat.
#' 
#' @param id shiny module id
#' 
#' @importFrom shiny NS tagList actionButton tags selectInput
#' uiOutput textOutput icon
#' @importFrom data.table fread
uploadUI <- function(id, dev) {
  ns <- NS(id)
  registeredEndpoints <- fread("resources/registeredEndpoints.txt")
  
  # TODO use `runjs` from shinyjs to update css : https://stackoverflow.com/questions/46045222/reactive-css-properties-in-r-shiny
  
  tagList(
    if(dev) actionButton(ns("dev"),"Dev"),
    tags$hr(),
    # select endpoint ----
    tags$h3("Select your MetaCat portal"),
    tags$div(
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
    tags$div(
      tags$p("The authentication token must be set in the MetaShARK options."),
      style = "border-left: solid lightgrey; padding: 20px;"
    ),
    tags$hr(),
    
    # files input ----
    tags$h3("Select your data, script and metadata files"),
    tags$div(
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
    
    # Constraints ----
    # div(id="constraints_div",
    #     tags$h4("Add constraints between script and data files"),
    #     actionButton(ns("add_constraint"), "", icon = icon("plus"), width = "40px")
    # ),
    # tags$hr(),
    
    actionButton(ns("process"), "Process", icon = icon("rocket"),
                 width = "100%")
  )
}

#' @title upload
#' 
#' @description server part of the upload module
#' 
#' @param input shiny module input
#' @param output shiny module output
#' @param session shiny module session
#' @param dataone.formats list of the dataone supported formats
#' 
#' @importFrom shiny observeEvent reactive textInput tags 
# observe renderUI reactiveValues callModule
#' @importFrom dplyr filter select
#' @importFrom shinyjs enable disable
#' @importFrom data.table fread fwrite
upload <- function(input, output, session, dev,
                   dataone.formats) {
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
      registeredEndpoints %>% dplyr::filter(mn == endpoint()) %>% dplyr::select(URL)
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
    if(
      dim(rvFiles$md())[1] != 1 ||
      dim(rvFiles$data())[1] < 1
    )
      disable("process")
    else
      enable("process")
    
    if(
      dim(rvFiles$scr())[1] == 0 ||
      dim(rvFiles$data())[1] == 0
    )
      disable("add_constraint")
    else
      enable("add_constraint")
    
  })
  
  # Constraints ----
  # workflowFiles <- reactiveValues()
  # 
  # observeEvent(input$add_constraint, {
  #   req(rvFiles$scr(), rvFiles$data())
  #   
  #   insertUI(
  #     selector = "#constraints_div",
  #     where = "afterBegin",
  #     ui = describeWorkflowUI(ns(input$add_constraint), rvFiles$scr(), rvFiles$data())
  #   )
  #   
  #   workflowFiles[[as.character(input$add_constraint)]] <- callModule(describeWorkflow, 
  #                                                                     input$add_constraint)
  # })
  
  # Process ----
  observeEvent(input$process,{
    disable("process")
    uploadDP(mn = as.character(registeredEndpoints %>% dplyr::filter(mn == endpoint()) %>% dplyr::select(URL)),
             cn = as.character(registeredEndpoints %>% dplyr::filter(mn == endpoint()) %>% dplyr::select(cn)),
             eml = rvFiles$md()$datapath,
             data = rvFiles$data()$datapath,
             formats = dataone.formats)
    enable("process")
  })
  
}