# UI ====

#' @import shiny
#' 
#' @noRd
tabPage <- function(id, title, ui, navTagList = NULL) {
  tabPanelBody(
    value = title,
    if (is.null(navTagList)) {
      fluidRow(
        column(12, ui)
      )
    } else {
      fluidRow(
        column(10, ui),
        column(2, navTagList)
      )
    }
  )
}

#' @import shiny
#'
#' @noRd
pagesUI <- function(id, parent.id) {
  steps <- base::get("ui.steps", envir = .GlobalEnv)
  # rm("ui.steps", envir = .GlobalEnv)
  .nb <- length(steps)
  .ui.args <- vector("list", .nb)
  
  # Wizard UI: a hidden tabSetPanel
  sapply(
    seq_along(steps),
    function(i) {
      page <- steps[i]
      
      .ui.args[[i]] <<- tabPage(
        id = id, # namespace extension
        title = page,
        ui = do.call(
          what = switch(i,
                        "SelectDPUI",
                        "DataFilesUI",
                        "AttributesUI",
                        "CatVarsUI",
                        "GeoCovUI",
                        "TaxCovUI",
                        "PersonnelUI",
                        "MiscUI",
                        "MakeEMLUI"
          ),
          args = list(
            id = NS(parent.id, page)
          )
        ),
        navTagList = if (i > 1)
          tagList(
            if (i != 2) prevTabButton(id, i),
            if (i != .nb) nextTabButton(id, i),
            tags$br(),
            # if(i > 2) # unused on Data Files selection
            #   actionButton(
            #     NS(id, "open_annotation"),
            #     "Annotation",
            #     icon = icon("project-diagram"),
            #     width = "100%"
            #   ),
            tags$hr(),
            uiOutput(NS(id, paste0(page, "-tag_list")))
          )
        else
          NULL
      )
    }
  )
  
  .ui.args$id <- NS(id, "wizard")
  .ui.args$type <- "hidden"
  
  do.call("tabsetPanel", .ui.args)
}

# Server ====

#' @import shiny
#' @importFrom shinyjs toggleState
#'
#' @noRd
pagesServer <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    steps <- isolate(main.env$VALUES$steps)
    
    changePage <- function(from, to, input, main.env) {
      observeEvent(input[[paste(from, to, sep = "_")]], {
        
        main.env$EAL$old.page <- main.env$EAL$page
        
        # Two times computing required for ifelse clause following
        .tmp <- main.env$EAL$page + to - from
        main.env$EAL$page <- .tmp + if(
          # want to reach categorical variables
          # but no catvar is to be found ..
          .tmp == 4 && isFALSE(main.env$save.variable$Attributes$use.catvars)
        ) {
          # .. avoid step ..
          switch(
            as.character(main.env$EAL$old.page),
            "3" = 1,
            "5" = -1
          )
        } else {
          # .. else let step be reached
          0
        }
        
      },
      label = paste("changePage", from, to)
      )
    }
    
    completeToggle <- function(from, to, main.env) {
      observe({
        shinyjs::toggleState(
          paste(from, to, sep = "_"),
          condition = isTRUE(main.env$EAL$completed)
        )
      })
    }
    
    # * Servers ====
    ids <- seq_along(steps)
    # Generate observers
    # Previous page
    lapply(ids[-1], function(i) 
      changePage(i, i-1, input, main.env)
    )
    # Next page
    lapply(ids[-length(steps)], function(i) {
      changePage(i, i+1, input, main.env)
      completeToggle(i, i+1, main.env)
    })
    
    # * Side UI ====
    # Fully functional? 
    sapply(isolate(main.env$VALUES$steps), function(page) {
      output[[paste0(page, "-tag_list")]] <- renderUI(main.env$EAL$tag.list)
    })
    
    # * Annotation ====
    # annotation_modal <- annotationsUI(session$ns("annotations"))
    # annotations("annotations")
    # observeEvent(input$open_annotation, {
    #   # only available on Attributes
    #   req(main.env$EAL$page > 2) # Nothing to describe in data files 
    #   # on first time, template
    #   if(!isContentTruthy(main.env$save.variable$Annotations$annot.table)){
    #     # template
    #     EMLassemblyline::template_annotations(
    #       path = main.env$save.variable$SelectDP$dp.path,
    #       data.path = main.env$save.variable$SelectDP$dp.data.path,
    #       data.table = main.env$save.variable$DataFiles$name,
    #       eml.path = ifelse(
    #         dir.exists(sprintf("%s/eml", main.env$save.variable$SelectDP$dp.path)),
    #         sprintf("%s/eml", main.env$save.variable$SelectDP$dp.path),
    #         NULL # EAL default
    #       ),
    #       eml = ifelse(
    #         dir.exists(sprintf("%s/eml", main.env$save.variable$SelectDP$dp.path)),
    #         dir(sprintf("%s/eml", main.env$save.variable$SelectDP$dp.path))[1],
    #         NULL # EAL default
    #       )
    #     )
    #     # read templated
    #     main.env$local.annotations$annotation.table <- readDataTable(
    #       sprintf(
    #         "%s/annotations.txt",
    #         main.env$save.variable$SelectDP$dp.metadata.path
    #       )
    #     )
    #     # build tree for modal popup
    #     main.env$local.annotations$tree.content <- buildAnnotationTree(main.env$local.annotations$annotation.table)
    #   }
    #   # show modal
    #   showModal(annotation_modal)
    # },
    # label = "open annotation")
    
    # * Chain ====
    # TODO fun things to use: bsButton() bsTooltip()
  })
}

# * Next ====

#' @import shiny
#'
#' @noRd
nextTabButton <- function(id, i) {
  actionButton(
    NS(id, paste(i, i + 1, sep = "_")),
    "Next",
    icon = icon("arrow-right"),
    width = "100%"
  )
}

# * Previous ====

#' @import shiny
#'
#' @noRd
prevTabButton <- function(id, i) {
  actionButton(
    NS(id, paste(i, i - 1, sep = "_")),
    "Previous",
    icon = icon("arrow-left"),
    width = "100%"
  )
}