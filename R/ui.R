#' @importFrom golem get_golem_options
#' @import shiny
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar 
#' sidebarMenu menuItem dashboardBody tabItems tabItem
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shinycssloaders withSpinner
#'
#' @noRd
appUI <- function() {
  # get app arguments
  main.env <- getShinyOption("main.env")
  dev <- main.env$dev
  wip <- main.env$wip

  # prepare variable
  .menu.width <- "250px"

  # ui
  fluidPage(
    includeCSS(system.file("app/www/styles.css", package = "MetaShARK")),
    shinyjs::useShinyjs(),
    # List the first level UI elements here
    shinydashboard::dashboardPage(
      title = "MetaShARK",
      shinydashboard::dashboardHeader(
        tags$li(
          class = "dropdown", 
          actionLink("settings", "", icon("gear"))
        ),
        title = tags$img(
          src = "media/ms_logo_small.png",
          width = "200px", 
          height = "50px"
        ),
        titleWidth = .menu.width
      ),
      ## Menus ----
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "side_menu",
          shinydashboard::menuItem("Welcome",
            tabName = "welcome",
            icon = icon("home")
          ),
          shinydashboard::menuItem("Fill in EML",
            tabName = "fill",
            icon = icon("file-import")
          ),
          shinydashboard::menuItem("Upload EML",
            tabName = "upload",
            icon = icon("file-export")
          ),
          shinydashboard::menuItem("EML Documentation",
            tabName = "documentation",
            icon = icon("glasses")
          ),
          shinydashboard::menuItem("About MetaShARK",
            tabName = "about",
            icon = icon("beer")
          ),
          shinyjs::hidden( # Ghost tab for options
            menuItem("settings",
              tabName = "settings",
              icon = icon("gear")
            )
          ),
          if (isolate(main.env$dev))
            actionButton("dev", "DEV CHECK")
        ),
        width = .menu.width
      ), # end sidebar
      ## Content ----
      shinydashboard::dashboardBody(
        tags$script(HTML("$('body').addClass('fixed');")),
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "welcome",
            welcomeUI("welcome", wip=wip)
          ),
          shinydashboard::tabItem(
            tabName = "fill",
            fillUI("fill", main.env)
          ),
          shinydashboard::tabItem(
            tabName = "upload",
            uploadUI("upload", main.env)
          ),
          shinydashboard::tabItem(
            tabName = "documentation",
            docUI("documentation")
          ),
          shinydashboard::tabItem(
            tabName = "about",
            aboutUI("about")
          ),
          shinydashboard::tabItem(
            tabName = "settings",
            settingsUI("settings", wip=wip)
          )
        )
      )
    ) # end dashboard
  ) # end fluidPage
}
