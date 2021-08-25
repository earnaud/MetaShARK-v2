#' @import shiny
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem dashboardBody tabItems tabItem
#' @importFrom shinyjs useShinyjs inlineCSS hidden
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom htmltools includeCSS
#' @importFrom shinydashboardPlus loadingState dashboardPage dashboardHeader
#' @importFrom shinybusy add_busy_spinner
#' 
#' @noRd
ui <- function() {
  # body
  tagList(
    # Enables packages support
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    # Add style
    shinyjs::inlineCSS("
      #loading-content {
        position: absolute;
        background: #000000;
        opacity: 0.9;
        z-index: 100;
        left: 0;
        right: 0;
        height: 100%;
        text-align: center;
        color: #FFFFFF;
      }
      
      .logo {
        padding-left: 5px !important;
        padding-top: 5px !important;
      }
    "),
    htmltools::includeCSS(
      system.file("app/www/styles.css", package = "MetaShARK")
    ),
    # App business spinner
    shinybusy::add_busy_spinner(
      spin = "spring",
      position = "bottom-left",
      color = "#3c8dbc"
    ),
    # Loading message
    div(
      id = "loading-content",
      h2("Loading..."),
      shinydashboardPlus::loadingState(),
      tags$img(
        src = "media/sea_shark.png", 
        width = "473px",
        height = "235px"
      )
    ),
    shinyjs::hidden(
      div(
        id = "app-content",
        shinydashboardPlus::dashboardPage(
          title = "MetaShARK",
          ## Header ====
          header = shinydashboardPlus::dashboardHeader(
            title = tagList(
              span(
                class = "logo-lg",
                tags$img(
                  src = "media/metashark-logo-v4.png", 
                  width = "240px",
                  height = "40px"
                )
              ),
              tags$img(
                src = "media/hex-MetaShARK_squared.png",
                width = "40px",
                height = "40px"
              )
            ),
            titleWidth = "250px"
          ),
          ## Menus ====
          ## * Tools ----
          sidebar = shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(
              id = "sidemenu",
              shinydashboard::menuItem(
                "Welcome",
                tabName = "welcome",
                icon = icon("home")
              ),
              shinydashboard::menuItem(
                "Fill in EML",
                tabName = "fill",
                icon = icon("file-import")
              ),
              shinydashboard::menuItem(
                "Upload EML",
                tabName = "upload",
                icon = icon("file-export")
              ),
              shinydashboard::menuItem(
                "EML Documentation",
                tabName = "documentation",
                icon = icon("glasses")
              ),
              shinydashboard::menuItem(
                "About MetaShARK",
                tabName = "about",
                icon = icon("beer")
              ),
              tagList(
                tags$hr(),
                shinyjs::hidden(
                  actionButton("dev", "DEV CHECK")
                ),
                shinyjs::hidden(
                  actionButton("test_end", "END TEST")
                ),
                tags$p(uiOutput("version"))
              )
            ),
            width = "250px"
          ), # end sidebar
          # * Settings ----
          controlbar = rightSidebarSettings(
            "settings"
          ),
          ## Content ====
          body = shinydashboard::dashboardBody(
            tags$script(HTML("$('body').addClass('fixed');")),
            shinydashboard::tabItems(
              shinydashboard::tabItem(
                tabName = "welcome",
                welcomeUI("welcome")
              ),
              shinydashboard::tabItem(
                tabName = "fill",
                fillUI("fill")
              ),
              shinydashboard::tabItem(
                tabName = "upload",
                uploadUI("upload")
              ),
              shinydashboard::tabItem(
                tabName = "documentation",
                docUI("documentation")
              ),
              shinydashboard::tabItem(
                tabName = "about",
                aboutUI("about")
              )
            )
          ) # end body
        ) # end of dashboardPage
      ) # end of div
    ) # end of hidden
  )
}
