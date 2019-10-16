#' @import shiny shinyjs shinydashboard RefManageR
.app_ui <- function() {
  
  # prepare variable
  menuWidth = "250px"
  
  # action
  tagList(
    # Leave this function for adding external resources
    .golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      title = "MetaShARK",
      dashboardHeader(
        title = span(imageOutput("metashark-logo", inline = TRUE), "MetaShARK"),
        titleWidth = menuWidth
      ),
      ## Menus ----
      dashboardSidebar(
        useShinyjs(),
        sidebarMenu(
          menuItem("Welcome", tabName = "welcome", 
                   icon = icon("home")),
          menuItem("Fill in EML", tabName = "fill",
                   icon = icon("file-import")),
          menuItem("EML Documentation", tabName = "documentation",
                   icon = icon("glasses")),
          menuItem("About MetaShARK", tabName = "about",
                   icon = icon("beer"))
          ,actionButton("check","Dev Check")
        ),
        width = menuWidth
      ), # end sidebar
      ## Content ----
      dashboardBody(
        tabItems(
          tabItem(tabName = "welcome",
                  welcomeUI("welcome")),
          tabItem(tabName = "fill",
                  # fillUI(IM.fill[1], IM = IM.fill)),
                  fluidPage(2)),
          tabItem(tabName = "documentation",
                  # docUI(IM.doc[1], IM = IM.doc)),
                  fluidPage(3)),
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
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
