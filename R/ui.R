#' @importFrom golem get_golem_options
#' @import shiny
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar 
#' sidebarMenu menuItem dashboardBody tabItems tabItem
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shinycssloaders withSpinner
#'
#' @noRd
.app_ui <- function() {
  
  # get app arguments
  .app.args <- golem::get_golem_options()
  dev <- .app.args$dev
  main.env <- .app.args$main.env
  wip <- .app.args$wip

  # prepare variable
  .menu.width <- "250px"

  # action
  tagList(
    includeCSS(system.file("app/www/styles.css", package = "MetaShARK")),
    shinyjs::useShinyjs(),
    # List the first level UI elements here
    dashboardPage(
      title = "MetaShARK",
      dashboardHeader(
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
      dashboardSidebar(
        sidebarMenu(
          id = "side_menu",
          menuItem("Welcome",
            tabName = "welcome",
            icon = icon("home")
          ),
          menuItem("Fill in EML",
            tabName = "fill",
            icon = icon("file-import")
          ),
          menuItem("Upload EML",
            tabName = "upload",
            icon = icon("file-export")
          ),
          menuItem("EML Documentation",
            tabName = "documentation",
            icon = icon("glasses")
          ),
          menuItem("About MetaShARK",
            tabName = "about",
            icon = icon("beer")
          ),
          shinyjs::hidden( # Ghost tab for options
            menuItem("settings",
              tabName = "settings",
              icon = icon("gear")
            )
          ),
          if (isolate(main.env$dev)) {
            actionButton(
              "dev", "DEV CHECK"
            )
          }
        ),
        width = .menu.width
      ), # end sidebar
      ## Content ----
      dashboardBody(
        tags$script(HTML("$('body').addClass('fixed');")),
        tabItems(
          tabItem(
            tabName = "welcome",
            welcomeUI("welcome", wip=wip)
          ),
          tabItem(
            tabName = "fill",
            fillUI("fill", main.env)
          ),
          tabItem(
            tabName = "upload",
            uploadUI("upload", main.env)
          ),
          tabItem(
            tabName = "documentation",
            docUI("documentation")
          ),
          tabItem(
            tabName = "about",
            aboutUI("about")
          ),
          tabItem(
            tabName = "settings",
            settingsUI("settings", wip)
          )
        )
      )
    ) # end dashboard
  ) # end taglist
}
