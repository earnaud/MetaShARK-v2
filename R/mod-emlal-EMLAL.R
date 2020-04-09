#' @title EMLALUI
#'
#' @description UI part of the EMLAL module. Allow the user to use a front-end shiny interface to the EML Assembly Line package, from
#' Environmental Data Initiative. ARAR
#'
#' @importFrom shiny NS fluidPage column HTML tags imageOutput uiOutput
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
EMLALUI <- function(id, dev = FALSE) {
  ns <- NS(id)
  
  fluidPage(
    style = "padding-top:2.5%;",
    box(
      collapsible = TRUE,
      width = 12,
      title = "About EML Assembly Line",
      column(4,
        tags$h3("Authorship"),
        HTML(
          "<p>The `EML Assembly Line` package used in this module
          and its children is the intellectual property of the
          Environment Data Initiative (EDI). You can find further
          details on their
          <a href=https://github.com/EDIorg/EMLassemblyline>git repository</a>.
          </p>"
        ),
        tags$div(
          imageOutput(
            ns("edi-logo"),
            width = "100px",
            height = "100px"
          ),
          class = "logo"
        )
      ),
      column(
        8,
        tags$h3("Usage"),
        HTML(
          "<p><b>EMLassemblyline</b> is a metadata builder for scientists
          and data managers who need to easily create high quality
          <b>EML</b> metadata for data publication. It emphasizes
          auto-extraction of metadata, appends value added content,
          and accepts user supplied inputs through template files
          thereby minimizing user effort while maximizing the potential
          of future data discovery and reuse. EMLassemblyline requires
          no familiarity with EML, is great for managing 10-100s of
          data packages, accepts all data formats, and supports complex
          and fully reproducible science workflows. Furthermore, it
          incorporates <a href=\"https://environmentaldatainitiative.files.wordpress.com/2017/11/emlbestpractices-v3.pdf\">EML best practices</a>,
          is based on a simple file organization scheme, and is not tied to a specific data repository.</p>
          <i>(preface by Colin Smith, EDI)</i>
        "
        )
      ) # end usage
    ),
    box(
      title = span(
        div("EML Assembly Line", style = "padding-right: 15px"),
        uiOutput(ns("chain")),
        style = "display: inline-flex"
      ),
      width = 12,
      fluidRow(
        uiOutput(ns("currentUI")) %>%
          withSpinner(color = "#599cd4")
      )
    ) # end variable UI
  ) # end fluidPage
}

#' @title EMLAL
#'
#' @description server part of the EMLAL module. Allow the user to use a front-end shiny interface to the EML Assembly Line package, from
#' Environmental Data Initiative.
#'
#' @importFrom shiny observeEvent renderUI renderImage HTML callModule imageOutput actionLink icon
#' @importFrom shinyBS tipify
EMLAL <- function(input, output, session,
  savevar, globals, server) {
  ns <- session$ns
  
  output$`edi-logo` <- renderImage(
    {
      list(
        src = system.file("media/EDI-logo.png", package = "MetaShARK"),
        contentType = "image/png",
        width = "100%",
        height = "100%"
      )
    },
    deleteFile = FALSE
  )
  
  # NSB -----------------------------------------------------
  # names of EMLAL steps
  steps <- c("SelectDP", "Data Files", "Attributes", "Categorical Variables", "Geographic Coverage", "Taxonomic Coverage", "Personnel", "Miscellaneous", "Make EML")
  
  NSB <- navSidebar("nav", globals, savevar)
  
  # Output -----------------------------------------------------
  iteration <- 0 # varying namespace
  observeEvent(globals$EMLAL$NAVIGATE, {
    iteration <<- iteration + 1
    
    if(globals$EMLAL$CURRENT == "Data Files")
      unlink(globals$EMLAL$TEMP)
    globals$EMLAL$CURRENT <- steps[globals$EMLAL$NAVIGATE]
    if(globals$EMLAL$CURRENT == "Data Files" && 
        !dir.exists(globals$TEMP.PATH))
      dir.create(globals$TEMP.PATH)
    
    if(isFALSE(globals$EMLAL$COMPLETE_CURRENT))
      globals$EMLAL$COMPLETE_CURRENT <- TRUE # trigger
    globals$EMLAL$COMPLETE_CURRENT <- FALSE
    NSB$tagList <- tagList()
    
    # Edition changed path -> remove excedent history
    if (!globals$EMLAL$CURRENT %in% globals$EMLAL$HISTORY) { # new
      globals$EMLAL$HISTORY <- c(globals$EMLAL$HISTORY, globals$EMLAL$CURRENT)
    }
    
    # Savevar modification
    savevar$emlal$step <- globals$EMLAL$NAVIGATE
    savevar$emlal$history <- globals$EMLAL$HISTORY
    
    # * Chain -----------------------------------------------------
    output$chain <- renderUI({
      validate(
        need(globals$EMLAL$NAVIGATE > 1, "")
      )
      
      return(
        tags$span(
          tagList(
            lapply(seq(globals$EMLAL$HISTORY)[-1], function(ind) {
              step_name <- globals$EMLAL$HISTORY[ind]
              
              if (step_name != "Taxonomic Coverage") {
                style <- "color: dodgerblue;"
                description <- paste(step_name, "(mandatory)")
              } else {
                style <- "color: lightseagreen;"
                description <- paste(step_name, "(facultative)")
              }
              
              return(
                actionLink(
                  ns(paste0("chain_", step_name)),
                  "",
                  if(step_name == globals$EMLAL$CURRENT) 
                    icon("map-marker") 
                  else 
                    icon("circle"),
                  style = style
                ) %>% tipify(
                  title = description,
                  placement = "bottom",
                  trigger = "hover"
                )
              ) # end of return
            }),
            paste0(
              "Step ", globals$EMLAL$NAVIGATE,
              "/", length(steps),
              ": ", globals$EMLAL$CURRENT
            )
          ),
          style = "position: right"
        )
      )
    })
    
    observe({
      validate(
        need(
          exists("globals") && isTruthy(names(input)), 
          "Not initialized"
        ), 
        need(
          isTruthy(globals$EMLAL$HISTORY),
          "No history available"
        ),
        need(
          any(sapply(
            globals$EMLAL$HISTORY, 
            grepl,
            x = names(input)
          ) %>% unlist) && 
            length(globals$EMLAL$HISTORY) > 1,
          "No history available"
        )
      )
      
      sapply(seq(globals$EMLAL$HISTORY)[-1], function(ind) {
        step_name <- globals$EMLAL$HISTORY[ind]
        
        id <- paste0("chain_", step_name)
        
        observeEvent(input[[id]], {
          req(input[[id]] &&
              ind != globals$EMLAL$NAVIGATE)
          globals$EMLAL$NAVIGATE <- ind
          NSB$NEXT <- NSB$NEXT+1
        })
      })
    })
    
    # * UI -----------------------------------------------------
    output$currentUI <- renderUI({
      .ui <- switch(globals$EMLAL$NAVIGATE,
        SelectDPUI(
          id = ns(iteration),
          dev = globals$dev,
          server = server
        ),
        DataFilesUI(
          id = ns(iteration),
          title = globals$EMLAL$CURRENT,
          dev = globals$dev,
          server = server
        ),
        AttributesUI(
          id = ns(iteration),
          title = globals$EMLAL$CURRENT,
          dev = globals$dev
        ),
        CatVarsUI(
          id = ns(iteration),
          title = globals$EMLAL$CURRENT,
          dev = globals$dev
        ),
        GeoCovUI(
          id = ns(iteration),
          title = globals$EMLAL$CURRENT,
          dev = globals$dev,
          data.files = savevar$emlal$DataFiles$datapath,
          coordPattern = globals$PATTERNS$LATLON
        ),
        TaxCovUI(
          id = ns(iteration),
          title = globals$EMLAL$CURRENT,
          dev = globals$dev,
          data.files = savevar$emlal$DataFiles$datapath,
          taxa.authorities = globals$FORMAT$AUTHORITIES
        ),
        PersonnelUI(
          id = ns(iteration),
          title = globals$EMLAL$CURRENT,
          dev = globals$dev
        ),
        MiscUI(
          id = ns(iteration),
          title = globals$EMLAL$CURRENT,
          dev = globals$dev,
          savevar = savevar,
          server = server
        ),
        MakeEMLUI(
          id = ns(iteration),
          title = globals$EMLAL$CURRENT,
          dev = globals$dev
        ),
        tags$h2("WIP")
      )
      
      return(
        if (globals$EMLAL$NAVIGATE > 1) {
          # NSB modifications
          .nsb <- if (globals$EMLAL$CURRENT == "Data Files")
            navSidebarUI(ns("nav"), .prev=FALSE)
          else if(globals$EMLAL$CURRENT == "Make EML")
            navSidebarUI(ns("nav"), .next=FALSE)
          else
            navSidebarUI(ns("nav"))
          
          tagList(
            column(10, .ui),
            column(2, .nsb)
          )
        } else {
          tagList(
            column(12, .ui)
          )
        }
      )
    })
    
    # * Server -----------------------------------------------------
    savevar <- switch(globals$EMLAL$NAVIGATE,
      callModule(
        SelectDP, iteration,
        savevar, globals,
        server = server
      ),
      callModule(
        DataFiles, iteration,
        savevar, globals,
        server = server,
        NSB = NSB
      ),
      callModule(
        Attributes, iteration,
        savevar, globals,
        NSB = NSB
      ),
      callModule(
        CatVars, iteration,
        savevar, globals,
        NSB = NSB
      ),
      callModule(
        GeoCov, iteration,
        savevar, globals,
        NSB = NSB
      ),
      callModule(
        TaxCov, iteration,
        savevar, globals,
        NSB = NSB
      ),
      callModule(
        Personnel, iteration,
        savevar, globals,
        NSB = NSB
      ),
      callModule(
        Misc, iteration,
        savevar, globals,
        NSB = NSB,
        server = server
      ),
      callModule(
        MakeEML, iteration,
        savevar, globals
      )
    )
  })
  
  # Save variable
  return(savevar)
}
