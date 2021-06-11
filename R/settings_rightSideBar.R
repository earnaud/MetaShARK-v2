#' @import shiny
#' @importFrom shinydashboardPlus rightSidebar rightSidebarTabContent
#' @importFrom shinyWidgets prettySwitch
#' 
#' @noRd
rightSidebarSettings <- function(id) {
  shinydashboardPlus::rightSidebar(

    # #### MAX 5 PANELS !!!! due to AdminLTE2 ### #

    # Sessionning ====
    shinydashboardPlus::rightSidebarTabContent(
      id = 1,
      title = "Login with ORCID",
      active = TRUE,
      icon = "globe",
      tags$div(
        wipRow(
          collapsibleUI(
            id = NS(id, "orcid-help"),
            label = "Interest of ORCID",
            ... = tags$p(
              "Without login, you can write and read all", tags$b("public"),
              "data packages created on this instance of MetaShARK. By logging in,
              you will also be able to edit", tags$b("private"), "data packages
              that will not be visible to other users."
            )
          ),
          orcidUI(NS(id, "orcid"))
          # TODO POC ORCID
        )
      )
    ),

    # Metacat token input ====
    shinydashboardPlus::rightSidebarTabContent(
      id = 2,
      title = "Metacat settings",
      icon = "desktop",
      tags$div(
        textAreaInput(
          NS(id, "metacat_token"),
          "Authentication token"
        ),
        tags$span(
          shinyWidgets::prettySwitch(
            NS(id, "test_metacat"),
            "Test MetaCat",
            value = TRUE
          ),
          actionButton(NS(id, "metacat_save"), "Save"),
          style = "display: inline-block;"
        ),
        textOutput(NS(id, "verbose_token")),
        collapsibleUI(
          NS(id, "help-metacat"),
          "How to get your token",
          ... = tagList(
            tags$b("To fetch your authentication token:"),
            tags$ul(
              tags$li("Login into your metacat through user interface."),
              tags$li("Navigate in the upper-right menu corner and click 'My profile'."),
              tags$li("Click the 'settings' tab and the 'Authentication token' panel."),
              tags$li("Copy paste the authentication token into the dedicated area on the left.")
            )
          )
        ),
        class = "inputBox"
      )
    ),

    # CEDAR token input ====
    shinydashboardPlus::rightSidebarTabContent(
      id = 3,
      title = "CEDAR token",
      icon = "tree",
      tags$div(
        wipRow(
          textAreaInput(
            NS(id, "cedar_token"),
            "Authentication token",
            width = "120%"
          ),
          tags$span(
            actionButton(NS(id, "cedar_save"), "Save")
          ),
          collapsibleUI(
            NS(id, "help-cedar"),
            "How to get your token",
            tagList(
              tags$b("To fetch your authentication token:"),
              tags$ul(
                tags$li("Login into your CEDAR profile at: https://cedar.metadatacenter.org/"),
                tags$li("Navigate in the upper-right menu corner and click 'Profile'."),
                tags$li("Paste the content for `key` field before `Usage from REST client`.")
              )
            )
          )
        ) # end of wipRow
      )
    )
  )
}

#' @title settings
#'
#' @description server part of the settings module. Allow the user to change several settings in the app.
#'
#' @import shiny
# @importFrom cedarr accessOntology
#' 
#' @noRd
settings <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Sessionning ====
    collapsible("help-orcid")
    orcid("orcid", main.env)

    # Metacat token ====
    collapsible("help-metacat")

    observeEvent(input$test_metacat, {
      req(input$test_metacat)
      main.env$SETTINGS$metacat.test <- input$test_metacat
    })

    observeEvent(input$metacat_save, {
      main.env$SETTINGS$metacat.token <- input$metacat_token
      showNotification(id = "metacat_set", "Dataone token set.", type = "message")
    })

    # CEDAR token ====
    collapsible("help-cedar")

    observeEvent(
      {
        input$cedar_token
        input$cedar_save
      },
      {
        req(input$cedar_token)
        .SETTINGS$cedar.token <- input$cedar_token
        # main.env$SEMANTICS$ontologies <- cedarr::accessOntology(.SETTINGS$cedar.token)
      }
    )
  })
}
