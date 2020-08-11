# UI ====

#' @noRd
#'
#' @import shiny
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
pagesUI <- function(id, parent.id, main.env) {
  steps <- isolate(main.env$VALUES$steps)
  .nb <- length(steps)
  .ui.args <- vector("list", .nb)
  
  # Wizard UI: a hidden tabSetPanel
  sapply(
    seq_along(steps),
    function(i, main.env) {
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
            id = NS(parent.id, page),
            main.env = main.env
          )
        ),
        navTagList = if (i > 1) {
          tagList(
            if (i != 2) prevTabButton(id, i),
            if (i != .nb) nextTabButton(id, i),
            uiOutput(NS(i, "tag_list"))
          )
        } else {
          NULL
        }
      )
    },
    main.env = main.env
  )
  
  .ui.args$id <- NS(id, "wizard")
  .ui.args$type <- "hidden"
  
  do.call("tabsetPanel", .ui.args)
}

# Server ====

#' Wizard pages server
#'
#' @noRd
pagesServer <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    steps <- isolate(main.env$VALUES$steps)
    
    changePage <- function(from, to, input, main.env) {
      observeEvent(input[[paste(from, to, sep = "_")]], {
        main.env$EAL$page <- main.env$EAL$page + to - from
        if (to > from) {
          main.env$EAL$.next <- main.env$EAL$.next + 1
        }
        if (from > to) {
          main.env$EAL$.prev <- main.env$EAL$.prev + 1
        }
      })
    }
    
    completeToggle <- function(from, to, main.env) {
      observeEvent(main.env$EAL$completed, {
        if(isTRUE(main.env$EAL$completed))
          enable(paste(from, to, sep = "_"))
        else
          disable(paste(from, to, sep = "_"))
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
    lapply(ids, function(i) {
      output[[NS(i, "tag_list")]] <- renderUI(main.env$EAL$tag.list)
    })
    
    # * Chain ====
    # TODO fun things to use: bsButton() bsTooltip()
    # output$chain <- renderUI({
    #   validate(
    #     need(main.env$EAL$page > 1, "")
    #   )
    #
    #   return(
    #     tags$span(
    #       tagList(
    #         lapply(seq(main.env$EAL$history)[-1], function(ind) {
    #           .step.name <- main.env$EAL$history[ind]
    #
    #           if (.step.name != "Taxonomic Coverage") {
    #             .style <- "color: dodgerblue;"
    #             .description <- paste(.step.name, "(mandatory)")
    #           } else {
    #             .style <- "color: lightseagreen;"
    #             .description <- paste(.step.name, "(facultative)")
    #           }
    #
    #           return(
    #             actionLink(
    #               ns(paste0("chain_", .step.name)),
    #               "",
    #               if (.step.name == main.env$EAL$current) {
    #                 icon("map-marker")
    #               } else {
    #                 icon("circle")
    #               },
    #               style = .style
    #             ) %>% shinyBS::tipify(
    #               title = .description
    #               # , placement = "bottom"
    #               # , trigger = "hover"
    #             )
    #           ) # end of return
    #         }),
    #         paste0(
    #           "Step ", main.env$EAL$page,
    #           "/", length(steps),
    #           ": ", main.env$EAL$current
    #         )
    #       ),
    #       style = "position: right"
    #     )
    #   )
    # })
    #
    # observe({
    #   validate(
    #     need(
    #       exists("main.env") && isTruthy(names(input)),
    #       "Not initialized"
    #     ),
    #     need(
    #       isTruthy(main.env$EAL$history),
    #       "No history available"
    #     ),
    #     need(
    #       any(sapply(
    #         main.env$EAL$history,
    #         grepl,
    #         x = names(input)
    #       ) %>% unlist()) &&
    #         length(main.env$EAL$history) > 1,
    #       "No history available"
    #     )
    #   )
    #
    #   sapply(seq(main.env$EAL$history)[-1], function(.ind) {
    #     id <- paste0("chain_", main.env$EAL$history[.ind])
    #
    #     observeEvent(input[[id]], {
    #       req(input[[id]] && .ind != main.env$EAL$page)
    #       main.env$EAL$page <- .ind
    #       saveReactive() # Set this up correctly
    #
    #       # trigger changes
    #       if(.ind > main.env$EAL$page)
    #         EAL$.next <- EAL$.next + 1
    #       if(.ind < EAL$page)
    #         EAL$.prev <- EAL$.prev + 1
    #     })
    #   })
  })
}

# * Next ====

#' @noRd
#'
#' @import shiny
nextTabButton <- function(id, i) {
  actionButton(
    NS(id, paste(i, i + 1, sep = "_")),
    "Next",
    icon = icon("arrow-right"),
    width = "100%"
  )
}

# * Previous ====

#' @noRd
#'
#' @import shiny
prevTabButton <- function(id, i) {
  actionButton(
    NS(id, paste(i, i - 1, sep = "_")),
    "Previous",
    icon = icon("arrow-left"),
    width = "100%"
  )
}
