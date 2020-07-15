#' @title settingsUI
#'
#' @description UI part of the settings module. Allow the user to change several settings in the app.
#'
#' @import shiny
settingsUI <- function(id, wip) {
  ns <- NS(id)
  
  tagList(
    tags$h1("Settings"),
    tags$p("This page is dedicated to define different settings in your session."),
    
    # Sessionning ====
    if(isTRUE(wip)){
      fluidRow(
        tags$h2("Login with ORCID"),
        tags$p("Without login, you can write and read all public data packages
          created on this instance of MetaShARK. By logging in, you will be able
          to write private data packages that will not appear on other users list.
          "),
        orcidUI(NS(id, "orcid")),
        # TODO POC ORCID
        class = "inputBox wip"
      )
    },
    # Metacat token input ====
    fluidRow(
      column(
        8,
        tags$h2("Metacat settings"),
        textAreaInput(NS(id, "metacat_token"),
          "Authentication token",
          width = "120%"
        ),
        checkboxInput(NS(id, "test_metacat"), "Test MetaCat", value = TRUE),
        actionButton(NS(id, "metacat_save"), "Save"),
        if (isTRUE(wip)) {
          textOutput(NS(id, "verbose_token"))
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
    if(isTRUE(wip))
    {
      fluidRow(
        tags$h2("CEDAR token"),
        column(
          8,
          textAreaInput(
            NS(id, "cedar_token"),
            "Authentication token",
            width = "120%"
          ),
          actionButton(NS(id, "cedar_save"), "Save")
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
        class = "inputBox wip"
      )
    }
  )
}

#' @title settings
#'
#' @description server part of the settings module. Allow the user to change several settings in the app.
#'
#' @import shiny
#' @importFrom shinyjs onclick
#' @importFrom cedarr accessOntology
settings <- function(id, main.env){
  moduleServer(id, function(input, output, session) {
    # Sessionning ====
    orcid("orcid", main.env)
    
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
    observeEvent({
      input$cedar_token
      input$cedar_save
    }, {
      req(input$cedar_token)
      .SETTINGS$cedar.token <- input$cedar_token
      main.env$SEMANTICS$ontologies <- cedarr::accessOntology(.SETTINGS$cedar.token)
    })
  })
}