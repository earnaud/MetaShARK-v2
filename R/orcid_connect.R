#' @importFrom shiny NS uiOutput
orcidUI <- function(id, globals) {
  ns <- NS(id)
  
  uiOutput(ns("infos"))
}

#' @importFrom shiny renderUI tagList actionButton icon tags 
#' @importFrom shinyjs onclick
orcid <- function(input, output, session) {
  ns <- session$ns
  
  globals = reactiveValues(
    SESSION = reactiveValues(
      LOGGED = FALSE,
      ORCID.TOKEN = character()
    )
  )
  
  observeEvent(globals$SESSION$LOGGED, {
    output$infos <- renderUI({
      if(isFALSE(globals$SESSION$LOGGED))
        tagList(
          actionButton(
            ns("connect"), 
            "Connect with ORCID", 
            icon = icon("orcid")
          )
        )
      else
        tagList(
          tags$h3("You name here"),
          tags$p("WIP: here will be your infos."),
          actionButton(ns("disconnect"), "Logout")
        )
    })
  })
  
  # observeEvent(input$connect, {
  onclick("connect", {
    globals$SESSION$ORCID.TOKEN <- rorcid::orcid_auth(
      reauth = TRUE
    )
    res <- httr::GET(
      "https://cn.dataone.org/portal/"
    )
    
    browser()
    stop("still working")
    globals$SESSION$LOGGED <- TRUE
    message(globals$SESSION$LOGGED)
  })
  
  onclick("disconnect", {
    globals$SESSION$LOGGED <- FALSE
  })
}
