#' @import shiny shinyjs shinydashboard shinyTree shinyFiles
#' @import RefManageR
#' @importFrom golem get_golem_options
.app_ui <- function() {

  # prepare variable
  menuWidth = "250px"
  dev = golem::get_golem_options(which = 'dev')
  if(!is.logical(dev) || is.null(dev)) dev = FALSE

  # action
  tagList(
    # Leave this function for adding external resources
    .golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      title = "MetaShARK",
      dashboardHeader(
        tags$li(class = "dropdown", actionLink("close", "", icon("power-off"))),
        # title = span(imageOutput("logo", inline = TRUE), "MetaShARK"),
        title = "MetaShARK",
        titleWidth = menuWidth
      ),
      ## Menus ----
      dashboardSidebar(
        useShinyjs(),
        sidebarMenu(id = "side_menu",
          menuItem("Welcome", tabName = "welcome",
                   icon = icon("home")),
          menuItem("Fill in EML", tabName = "fill",
                   icon = icon("file-import")),
          menuItem("EML Documentation", tabName = "documentation",
                   icon = icon("glasses")),
          menuItem("About MetaShARK", tabName = "about",
                   icon = icon("beer"))
        ),
        if(dev) actionButton("check","Dev Check"),
        width = menuWidth
      ), # end sidebar
      ## Content ----
      dashboardBody(
        tags$script(HTML("$('body').addClass('fixed');")),
        tabItems(
          tabItem(tabName = "welcome",
                  welcomeUI("welcome")),
          tabItem(tabName = "fill",
                  fillUI("fill", dev)),
          tabItem(tabName = "documentation",
                  docUI("documentation")),
          tabItem(tabName = "about",
                  aboutUI("about"))
        )
      ) # end body

    ) # end dashboard

  ) # end taglist

}

#' @import shiny
.golem_add_external_resources <- function(){

  addResourcePath(
    'www', system.file('app/www', package = 'MetaShARK')
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css")
  )
}
