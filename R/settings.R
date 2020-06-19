#' @title settingsUI
#'
#' @description UI part of the settings module. Allow the user to change several settings in the app.
#'
#' @importFrom shiny NS tags tagList fluidRow column textAreaInput checkboxInput actionButton
settingsUI <- function(id, dev) {
  ns <- NS(id)
  
  tagList(
    tags$h1("Settings"),
    tags$p("This page is dedicated to define different settings in your session."),
    
    # Sessionning ====
    if(isTRUE(dev)){
      fluidRow(
        tags$h2("Login with ORCID"),
        tags$p("Without login, you can write and read all public data packages
          created on this instance of MetaShARK. By logging in, you will be able
          to write private data packages that will not appear on other users list.
          "),
        # orcidUI(ns("orcid")),
        # TODO POC ORCID
        class = "inputBox wip"
      )
    },
    # Metacat token input ====
    fluidRow(
      column(
        8,
        tags$h2("Metacat settings"),
        textAreaInput(ns("metacat_token"),
          "Authentication token",
          width = "120%",
          value = options("dataone_token")
        ),
        checkboxInput(ns("test_metacat"), "Test MetaCat", value = TRUE),
        actionButton(ns("metacat_save"), "Save"),
        if (dev) {
          textOutput(ns("verbose_token"))
        }
      ),
      column(
        4,
        tags$b("To fetch your authentication token:"),
        tags$ul(
          tags$li("Login into your metacat through user interface."),
          tags$li("Navigate in the upper-right menu corner and click 'My profile'."),
          tags$li("Click the 'settings' tab and the 'Authentication token' panel."),
          tags$li("Copy paste the authentication token into the dedicated area on the left.")
        )
      ),
      class = "inputBox"
    ),
    
    # CEDAR token input ====
    if(isTRUE(dev))
    {
      fluidRow(
        tags$h2("CEDAR token"),
        column(
          8,
          textAreaInput(ns("cedar_token"),
            "Authentication token",
            width = "120%",
            value = options("dataone_token")
          )
        ),
        column(
          4,
          tags$b("To fetch your authentication token:"),
          tags$ul(
            tags$li("Login into your CEDAR profile at: https://cedar.metadatacenter.org/"),
            tags$li("Navigate in the upper-right menu corner and click 'Profile'."),
            tags$li("Paste the content for `key` field before `Usage from REST client`.")
          )
        ),
        class = "inputBox"
      )
    }
  )
}

#' @title settings
#'
#' @description server part of the settings module. Allow the user to change several settings in the app.
#'
#' @importFrom shiny observeEvent updateTextAreaInput showNotification
#' @importFrom shinyjs onclick
#' @importFrom cedarr accessOntology
settings <- function(input, output, session, main.env) {
  # Sessionning ====
  # observeEvent(input$cedar_token, {
  #   browser()
  #   globals$SETTINGS$cedar.token <- orcid_auth(reauth=TRUE)
  # })
  # callModule(orcid, "orcid")
  
  # Metacat token ====
  observeEvent(input$test_metacat, {
    req(input$test_metacat)
    main.env$SETTINGS$metacat.test <- input$test_metacat
  })
  
  observeEvent(input$metacat_save, {
    main.env$SETTINGS$metacat.token <- input$metacat_token
    showNotification(id = "metacat_set", "Dataone token set.", type = "message")
  })
  
  # CEDAR token ====
  # observeEvent(input$cedar_token, {
  #   globals$SETTINGS$TOKEN$CEDAR <- input$cedar_token
  #   req(input$cedar_token)
  #   globals$SEMANTICS$ONTOLOGIES <- accessOntology(input$cedar_token)
  # })
}
