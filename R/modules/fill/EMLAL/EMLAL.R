# EMLAL.R

# Derived Id Modules from IM.EMLAL by pasting the step number (https://ediorg.github.io/EMLassemblyline/articles/overview.html)

### UI ###
EMLALUI <- function(id, IM){
  ns <- NS(id)
  
  steps = list(`Organize data package` = 1,
               `Create metadata templates` = 2,
               `Edit metadata templates` = 3,
               `Make EML` = 4)
  step = steps[[1]]
  
  fluidPage(
    style="padding-top:2.5%;",
    box(width = 4,
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
          style = "display: block; 
                   margin-left: auto;
                   margin-right: auto;
                   width: 100px;"
        )
    ), # end authorship
    box(width = 8,
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
EMLAL <- function(input, output, session, IM, globalRV){
  ns <- session$ns
  
  # variable initialization
  steps = paste0(c("select","create","template","customUnits",
                   "catvars", "edit","make","publish"), "-tab")
  
  # Output
  output$currentUI <- renderUI({
    switch(globalRV$navigate,
           `1` = selectDPUI(id = IM.EMLAL[2+globalRV$navigate],
                            IM = IM.EMLAL,
                            title = steps[globalRV$navigate]),
           `2` = createDPUI(id = IM.EMLAL[2+globalRV$navigate],
                            IM = IM.EMLAL,
                            title = steps[globalRV$navigate]),
           `3` = templateDPUI(id = IM.EMLAL[2+globalRV$navigate],
                              IM = IM.EMLAL,
                              title = steps[globalRV$navigate]),
           `4` = customUnitsUI(id = IM.EMLAL[2+globalRV$navigate],
                               IM = IM.EMLAL,
                               title = steps[globalRV$navigate]),
           `5` = catvarsUI(id = IM.EMLAL[2+globalRV$navigate],
                           IM = IM.EMLAL,
                           title = steps[globalRV$navigate])
                  )
  })
  
}