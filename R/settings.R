#' @title appOptionsUI
#'
#' @description UI part of the appOptions module. Allow the user to change several settings in the app.
#'
#' @importFrom shiny NS tags tagList fluidRow column textAreaInput checkboxInput actionButton
appOptionsUI <- function(id, dev) {
  ns <- NS(id)

  tagList(
    tags$h1("Settings"),
    tags$p("This page is dedicated to define different settings in your session."),

    # Sessionning ====
    fluidRow(
      tags$h2("Login with ORCID"),
      tags$p("Without login, you can write and read all public data packages 
        created on this instance of MetaShARK. By logging in, you will be able
        to write private data packages that will not appear on other users list.
        "),
      # orcidUI(ns("orcid")),
      class = "inputBox wip"
    ),

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
  )
}

#' @title appOptions
#'
#' @description server part of the appOptions module. Allow the user to change several settings in the app.
#'
#' @importFrom shiny observeEvent updateTextAreaInput showNotification
#' @importFrom shinyjs onclick
#' @importFrom cedarr accessOntology
appOptions <- function(input, output, session, globals) {
  # Sessionning ====
  # observeEvent(input$cedar_token, {
  #   browser()
  #   globals$SETTINGS$TOKEN$CEDAR <- orcid_auth(reauth=TRUE)
  # })
  # callModule(orcid, "orcid")

  # CEDAR token ====
  observeEvent(input$cedar_token, {
    globals$SETTINGS$TOKEN$CEDAR <- input$cedar_token
    req(input$cedar_token)
    globals$SEMANTICS$ONTOLOGIES <- accessOntology(input$cedar_token)
  })

  # Metacat token ====
  observeEvent(input$test_metacat, {
    if (input$test_metacat) {
      updateTextAreaInput(
        session,
        "metacat_token",
        value = globals$SETTINGS$TOKEN$DATAONE.TEST.TOKEN
      )
    }
    else {
      updateTextAreaInput(session,
        "metacat_token",
        value = globals$SETTINGS$TOKEN$DATAONE.TOKEN
      )
    }
  })

  observeEvent(input$metacat_save, {
    # onclick("metacat_save", {
    if (input$test_metacat) {
      globals$SETTINGS$TOKEN$DATAONE.TEST.TOKEN <- input$metacat_token
    } else {
      globals$SETTINGS$TOKEN$DATAONE.TOKEN <- input$metacat_token
    }
    showNotification(id = "metacat_set", "Dataone token set.", type = "message")
  })
}
