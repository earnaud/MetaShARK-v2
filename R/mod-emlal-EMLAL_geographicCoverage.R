#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @importFrom shiny NS fluidPage fluidRow column tagList tags actionButton
geocovUI <- function(id, title, dev, data.files) {
  ns <- NS(id)

  browser()
  
  return(
    fluidPage(
      # Features UI ----
      column(
        10,
        fluidRow(
          tags$h4(title),
          # tags$p("help"),
          bsCollapse(
            id = ns("method"),
            bsCollapsePanel(
              title = "Use dataset's geographic variables",
              value = 1,
              tagList(
                selectInput("site", "Choose a column for sites:", c("(None selected)", lapply(data.files, colnames))),
                selectInput("latitude", "Choose a column for latitude:", c("(None selected)", lapply(data.files, colnames))),
                selectInput("longitude", "Choose a column for longitude:", c("(None selected)", lapply(data.files, colnames)))
              )
            ),
            bsCollapsePanel(
              title = "Fill geographic template",
              value = 2,
              span(
                actionButton("addui", "", icon("plus")),
                hidden(actionButton("removeui", "", icon("minus")))
              )
            )
          )
        )
      ), # end of column1
      # Navigation UI ----
      column(
        2,
        navSidebar(ns("nav"),
          ... = tagList(
            if (dev) actionButton(ns("check"), "Dev Check")
          )
        )
      ) # end of column2
    ) # end of fluidPage
  ) # end of return
}

#' @title Geographic coverage
#'
#' @description server part for the Geographic Coverage module
#'
#' @importFrom shiny observeEvent callModule
#' @importFrom shinyBS updateCollapse
geocov <- function(input, output, session, savevar, globals) {
  ns <- session$ns

  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }
  # Variable initialization
  rv = reactiveValues(
    sites = data.frame(
      id = character(),
      site = character(),
      lat = numeric(),
      lon = numeric()
    ),
    uis <- reactiveValues(),
    servers <- reactiveValues()
  )

  observeEvent(TRUE, {
    disable("removeui")
  }, once = TRUE)
  
  # Navigation buttons ----
  observeEvent(input$addui, {
    rv$sites <- rbind(
      rv$sites,
      c(paste0("site_",input$addui), character(""), 0, 0)
    )
    
    enable("removeui")
  })
  output$sites_ui <- renderUI({
    req(rv$sites)
    
    tagList(
      lapply(rv$sites$id, function(id){
        div(id = paste0("ui", id),
          checkboxInput(paste0("check-",id)),
          textInput(paste0("site-",id), "Site name or id"),
          numericInput(paste0("lat-",id), "Latitude", 0, -90, 90, 1),
          numericInput(paste0("lon-",id), "Longitude", 0, -180, 180, 1)
        )
      })
    )
  })
  observeEvent(rv$sites, {
    
  })
  
  observeEvent(input$removeui, {
    
  })
  
  # NSB
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    nextTab, "nav",
    globals, "geocov"
  )
  callModule(
    prevTab, "nav",
    globals
  )

  # Method ----
  observeEvent(input$method, {
    
  })
  
  # Process data ----
  
  observeEvent(input[["nav-prevTab"]], {
    if(tail(globals$EMLAL$HISTORY, 1) == "customUnits")
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-1
    else if(tail(globals$EMLAL$HISTORY, 1) == "template")
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE-2
  })
  observeEvent(input[["nav-nextTab"]], {
    
  })
  
  # Output ----
  return(savevar)
}
