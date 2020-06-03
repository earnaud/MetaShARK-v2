#' @title appOptionsUI
#'
#' @description UI part of the appOptions module. Allow the user to change several settings in the app.
#'
#' @importFrom shiny NS tags tagList fluidRow column textAreaInput checkboxInput actionButton
appOptionsUI <- function(id, settingsVar, dev) {
  ns <- NS(id)

  tagList(
    tags$h1("Settings"),
    tags$p("This page is dedicated to define different settings in your session."),
    # Metacat token input ====
    fluidRow(
      column(8,
        tags$h2("Metacat settings"),
        textAreaInput(ns("auth_token"),
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
      column(4,
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
    # Additional settings ====
    fluidRow(
      
    )
  )
}

#' @title appOptions
#'
#' @description server part of the appOptions module. Allow the user to change several settings in the app.
#'
#' @importFrom shiny observeEvent updateTextAreaInput showNotification
#' @importFrom shinyjs onclick
appOptions <- function(input, output, session, globals, server) {
  observeEvent(input$test_metacat, {
    if (input$test_metacat) {
      updateTextAreaInput(
        session,
        "auth_token",
        value = globals$TOKEN$DATAONE.TEST.TOKEN
      )
    }
    else {
      updateTextAreaInput(session,
        "auth_token",
        value = globals$TOKEN$DATAONE.TOKEN
      )
    }
  })

  # observeEvent(input$metacat_save, {
  onclick("metacat_save", {
    if (input$test_metacat) {
      globals$TOKEN$DATAONE.TEST.TOKEN <- input$auth_token
    } else {
      globals$TOKEN$DATAONE.TOKEN <- input$auth_token
    }
    showNotification("Dataone token set.", type = "message")
  })
}
