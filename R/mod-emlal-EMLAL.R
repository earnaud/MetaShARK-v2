#' @title EMLALUI
#'
#' @description UI part of the EMLAL module. Allow the user to use a front-end shiny interface to the EML Assembly Line package, from
#' Environmental Data Initiative. ARAR
#'
#' @importFrom shiny NS fluidPage HTML tags imageOutput uiOutput
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
          imageOutput(ns("edi-logo"), # from main.R
            width = "100px", height = "100px"
          ),
          class = "logo"
        )
      ),
      column(8,
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
        ")
      ) # end usage
    ),
    box(
      title = "EML Assembly Line",
      width = 12,
      uiOutput(ns("currentUI"))  %>% withSpinner(color="#599cd4")
    ) # end variable UI
  ) # end fluidPage
}

#' @title EMLAL
#'
#' @description server part of the EMLAL module. Allow the user to use a front-end shiny interface to the EML Assembly Line package, from
#' Environmental Data Initiative.
#'
#' @importFrom shiny observeEvent renderUI HTML callModule
EMLAL <- function(input, output, session,
  savevar, globals) {
  ns <- session$ns
  
  output$`edi-logo` <- renderImage({
    list(
      src = "inst/app/www/EDI-logo.png",
      contentType = "image/png",
      width = "100%",
      height = "100%"
    )
  }, deleteFile = FALSE)
  
  # names of EMLAL steps
  steps <- c("SelectDP", "DataFiles", "Attributes","CustomUnits",
    "CatVars", "GeoCov", "TaxCov", "Misc", "MakeEML")
  
  iteration <- 0 # varying namespace
  # Output ----
  observeEvent(globals$EMLAL$NAVIGATE, {
    iteration <<- iteration + 1
    # UI ----
    output$currentUI <- renderUI({
      switch(globals$EMLAL$NAVIGATE,
        SelectDPUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        DataFilesUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        AttributesUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        CustomUnitsUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        CatVarsUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        GeoCovUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev,
          data.files = savevar$emlal$DataFiles$dp_data_files$datapath,
          coordPattern = globals$PATTERNS$LATLON
        ),
        TaxCovUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev,
          data.files = savevar$emlal$DataFiles$dp_data_files$datapath,
          taxa.authorities = globals$FORMAT$AUTHORITIES
        ),
        PersonnelUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        MiscUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev,
          savevar = savevar
        ),
        MakeEMLUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        {
          tagList(
            imageOutput(NS(iteration)("WIP")),
            imageOutput("WIP")
          )
        }
      )
    })
    
    # Server ----
    savevar <- switch(globals$EMLAL$NAVIGATE,
      callModule(
        SelectDP, iteration,
        savevar, globals
      ),
      callModule(
        DataFiles, iteration,
        savevar, globals
      ),
      callModule(
        Attributes, iteration,
        savevar, globals
      ),
      callModule(
        CustomUnits, iteration,
        savevar, globals
      ),
      callModule(
        CatVars, iteration,
        savevar, globals
      ),
      callModule(
        GeoCov, iteration,
        savevar, globals
      ),
      callModule(
        TaxCov, iteration,
        savevar, globals
      ),
      callModule(
        Personnel, iteration,
        savevar, globals
      ),
      callModule(
        Misc, iteration,
        savevar, globals
      ),
      callModule(
        MakeEML, iteration,
        savevar, globals
      ),
      callModule(
        function(input, output, server, savevar) {
          output$WIP <- renderImage("media/working.png")
          return(savevar)
        },
        iteration
      )
    )
  })
  
  # Save variable
  return(savevar)
}
