#' @import shiny
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#' sidebarMenu menuItem dashboardBody tabItems tabItem
#' @importFrom shinyjs useShinyjs hidden
#'
#' @noRd
appUI <- function() {
  # get app arguments
  main.env <- get("main.env", .GlobalEnv)

  # prepare variable
  .menu.width <- "250px"

  tagList(
    shinyjs::useShinyjs(),
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
    # tags$link(
    #   crossorigin="anonymous",
    #   href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css",
    #   integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T",
    #   rel="stylesheet"
    # ),
    htmltools::includeCSS(
      system.file("app/www/styles.css", package = "MetaShARK")
    ),
    # Loading message
    div(
      id = "loading-content",
      h2("Loading..."),
      shinydashboardPlus::loadingState(),
      tags$img(
        src = "media/sea_shark.png",
        width = "50%",
        height = "25%"
      )
    ),
    shinyjs::hidden(
      div(
        id = "app-content",
        shinydashboardPlus::dashboardPagePlus(
          title = "MetaShARK",
          ## Header ====
          header = shinydashboardPlus::dashboardHeaderPlus(
            title = tagList(
              span(
                class = "logo-lg",
                tags$img(
                  src = "media/metashark-logo-v4.png",
                  width = "240px",
                  height = "40px"
                )
              ),
              img(
                src = "media/hex-MetaShARK_squared.png",
                width = "40px",
                height = "40px"
              )
            ),
            titleWidth = "250px",
            enable_rightsidebar = TRUE,
            rightSidebarIcon = "gears"
          ),
          ## Menus ====
          ## * Tools ----
          sidebar = shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(
              id = "side_menu",
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
              # shinyjs::hidden( # Ghost tab for options
              #   shinydashboard::menuItem(
              #     "settings",
              #     tabName = "settings"
              #   )
              # ),
              if (main.env$dev) {
                tagList(
                  tags$hr(),
                  actionButton(
                    "dev", "DEV CHECK"
                  )
                )
              }
            ),
            width = "250px"
          ), # end sidebar
          # * Settings ----
          rightsidebar = rightSidebarSettings(
            "settings",
            wip = main.env$wip
          ),
          ## Content ====
          body = shinydashboard::dashboardBody(
            # tags$script(HTML("$('body').addClass('fixed');")),
            shinydashboard::tabItems(
              shinydashboard::tabItem(
                tabName = "welcome",
                welcomeUI("welcome", wip = main.env$wip)
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
              )
              # ,
              # shinydashboard::tabItem(
              #   tabName = "settings",
              #   settingsUI("settings", wip = main.env$wip)
              # )
            )
          ) # end body
        ) # end of dashboardPage
      ) # end of div
    ) # end of hidden
  )
}
