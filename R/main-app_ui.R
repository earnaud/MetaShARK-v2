#' @title .app_ui
#'
#' @description UI part of the mainapp's  script
#'
#' @importFrom golem get_golem_options
#' @importFrom shiny tagList tags actionLink icon span imageOutput actionButton HTML
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem dashboardBody tabItems tabItem
#' @importFrom shinyjs useShinyjs hidden
.app_ui <- function() {
  appArgs <- get_golem_options()
  dev <- appArgs$dev
  server <- appArgs$server

  # prepare variable
  menuWidth <- "250px"
  if (!is.logical(appArgs$dev) || is.null(appArgs$dev)) appArgs$dev <- FALSE
  globals <- .globalScript(appArgs$dev, reactive = FALSE)

  # action
  tagList(
    includeCSS(system.file("app/www/styles.css", package = "MetaShARK")),
    # List the first level UI elements here
    dashboardPage(
      title = "MetaShARK",
      dashboardHeader(
        tags$li(class = "dropdown", actionLink("appOptions", "", icon("gear"))),
        tags$li(
          class = "dropdown",
          if (!isTRUE(appArgs$server)) {
            actionLink("close", "", icon("power-off"))
          } else {
            NULL
          }
        ),
        title = span(imageOutput("logo", inline = TRUE)),
        titleWidth = menuWidth
      ),
      ## Menus -----------------------------------------------------
      dashboardSidebar(
        useShinyjs(),
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
          hidden( # Ghost tab for options
            menuItem("appOptions",
              tabName = "appOptions",
              icon = icon("gear")
            )
          )
        ),
        if (appArgs$dev) actionButton("check", "Dev Check"),
        width = menuWidth
      ), # end sidebar
      ## Content -----------------------------------------------------
      dashboardBody(
        tags$script(HTML("$('body').addClass('fixed');")),
        tabItems(
          tabItem(
            tabName = "welcome",
            welcomeUI("welcome")
          ),
          tabItem(
            tabName = "fill",
            fillUI("fill", appArgs$dev)
          ),
          tabItem(
            tabName = "upload",
            uploadUI("upload", appArgs$dev, globals)
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
            tabName = "appOptions",
            appOptionsUI("appOptions", appArgs$dev)
          )
        )
      ) # end body
    ) # end dashboard
  ) # end taglist
}
