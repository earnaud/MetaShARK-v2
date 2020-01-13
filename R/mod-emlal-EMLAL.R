#' @title EMLALUI
#'
#' @description UI part of the EMLAL module. Allow the user to use a front-end shiny interface to the EML Assembly Line package, from
#' Environmental Data Initiative.
#'
#' @importFrom shiny NS fluidPage HTML tags imageOutput uiOutput
#' @importFrom shinydashboard box
EMLALUI <- function(id, dev = FALSE) {
  ns <- NS(id)

  fluidPage(
    style = "padding-top:2.5%;",
    box(
      width = 4,
      title = "Authorship",
      HTML(
        "<p>The `EML Assembly Line` package used in this module
          and its children is the intellectual property of the
          Environment Data Initiative (EDI). You can find further
          details on their
          <a href=https://github.com/EDIorg/EMLassemblyline>git repository</a>.
          </p>"
      ),
      tags$div(
        imageOutput("edi-logo", # from main.R
          width = "100px", height = "100px"
        ),
        class = "logo"
      )
    ), # end authorship
    box(
      width = 8,
      title = "How to use",
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
      <i>(preface by Colin Smith, EDI)</i>"
      )
    ), # end how-to-use
    box(
      title = "EML Assembly Line",
      width = 12,
      uiOutput(ns("currentUI"))
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

  # names of EMLAL steps
  steps <- paste0(c(
    "select", "files", "template", "customUnits",
    "catvars", "geocov", "taxcov", "make", "publish"
  ), "-tab")

  iteration <- 0 # varying namespace
  # Output ----
  observeEvent(globals$EMLAL$NAVIGATE, {
    iteration <<- iteration + 1
    # UI
    output$currentUI <- renderUI({
      switch(globals$EMLAL$NAVIGATE,
        selectDPUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        DPfilesUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        templateDPUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        customUnitsUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        catvarsUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        geocovUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev,
          data.files = savevar$emlal$DPfiles$dp_data_files$datapath,
          coordPattern = globals$PATTERNS$LATLON
        ),
        taxcovUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE],
          dev = globals$dev
        ),
        tagList(
          imageOutput(ns("WIP"))
        )
      )
    })
    # Server
    savevar <- switch(globals$EMLAL$NAVIGATE,
      callModule(
        selectDP, iteration,
        savevar, globals
      ),
      callModule(
        DPfiles, iteration,
        savevar, globals
      ),
      callModule(
        templateDP, iteration,
        savevar, globals
      ),
      callModule(
        customUnits, iteration,
        savevar, globals
      ),
      callModule(
        catvars, iteration,
        savevar, globals
      ),
      callModule(
        geocov, iteration,
        savevar, globals
      ),
      callModule(
        taxcov, iteration,
        savevar, globals
      ),
      callModule(
        function(input, output, server, savevar) {
          output$WIP <- renderImage("media/working.png")
          return(savevar)
        },
        iteration,
        savevar
      )
    )
  })

  # Save variable
  return(savevar)
}
