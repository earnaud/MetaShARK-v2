# EMLAL.R

# Derived Id Modules from IM.EMLAL by pasting the step number (https://ediorg.github.io/EMLassemblyline/articles/overview.html)

### UI ###
EMLALUI <- function(id) {
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
      div(
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

### SERVER ###
EMLAL <- function(input, output, session,
                  savevar, globals) {
  ns <- session$ns

  # variable initialization ----
  # submodules sourcing
  source("R/modules/fill/EMLAL/EMLAL_selectDP.R")
  source("R/modules/fill/EMLAL/EMLAL_DPfiles.R")
  source("R/modules/fill/EMLAL/EMLAL_templateDP.R")
  source("R/modules/fill/EMLAL/EMLAL_functions.R")

  # names of EMLAL steps
  steps <- paste0(c(
    "select", "files", "template", "customUnits",
    "catvars", "edit", "make", "publish"
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
          title = steps[globals$EMLAL$NAVIGATE]
        ),
        DPfilesUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE]
        ),
        templateDPUI(
          id = ns(iteration),
          title = steps[globals$EMLAL$NAVIGATE]
        )
        # `4` = customUnitsUI(id =  ns(iteration),
        #                     title = steps[globals$EMLAL$NAVIGATE]),
        # `5` = catvarsUI(id = ns(iteration),
        #                 title = steps[globals$EMLAL$NAVIGATE])
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
      )
    )
  })

  # Save variable
  return(savevar)
}
