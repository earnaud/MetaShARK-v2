# UI ====

#' @import shiny
#'
#' @noRd
tabPage <- function(id, title, ui, nav_tag_list = NULL) {
  tabPanelBody(
    value = title,
    if (is.null(nav_tag_list)) {
      fluidRow(
        column(12, ui)
      )
    } else {
      fluidRow(
        column(10, ui),
        column(2, nav_tag_list)
      )
    }
  )
}

#' @import shiny
#'
#' @noRd
pagesUI <- function(id, parent_id) {
  steps <- c(
    "SelectDP",
    "Data_Files",
    "Attributes",
    "Categorical_Variables",
    "Geographic_Coverage",
    "Taxonomic_Coverage",
    "Personnel",
    "Miscellaneous",
    "Make_EML"
  )
  .nb <- length(steps)
  .ui_args <- vector("list", .nb)

  # Wizard UI: a hidden tabSetPanel
  sapply(
    seq_along(steps),
    function(i) {
      page <- steps[i]

      .ui_args[[i]] <<- tabPage(
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
            id = NS(parent_id, page)
          )
        ),
        nav_tag_list = if (i > 1) {
          tagList(
            if (i != 2) prevTabButton(id, i),
            if (i != .nb) nextTabButton(id, i),
            tags$br(),
            tags$hr(),
            uiOutput(NS(id, paste0(page, "-tag_list")))
          )
        } else {
          NULL
        }
      )
    }
  )

  .ui_args$id <- NS(id, "wizard")
  .ui_args$type <- "hidden"

  do.call("tabsetPanel", .ui_args)
}

# Server ====

#' @import shiny
#' @importFrom shinyjs toggleState
#'
#' @noRd
pagesServer <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    steps <- isolate(main_env$VALUES$steps)

    changePage <- function(from, to, input, main_env) {
      observeEvent(input[[paste(from, to, sep = "_")]], {
          main_env$EAL$old_page <- main_env$EAL$page

          # Two times computing required for ifelse clause following
          .tmp <- main_env$EAL$page + to - from
          # Do catvars?
          if (.tmp == 4) {
            .use_catvars <- if ("Attributes" %in% main_env$EAL$history) {
              any(sapply(
                main_env$save_variable$Attributes$content,
                function(table) any(table$class == "categorical")
              ))
            } else if (main_env$EAL$old_page == 3) {
              main_env$local_rv$use_catvars()
            } else {
              FALSE
            }

            .tmp <- .tmp + if (isFALSE(.use_catvars)) {
              # tell the user if going from Attributes to GeoCov
              if (main_env$EAL$old_page == 3) {
                showNotification(
                  "Skipped categorical variables (not required)"
                )
              }
              # .. no, avoid step ..
              switch(as.character(main_env$EAL$old_page),
                "3" = 1,
                "5" = -1
              )
            } else if (isTRUE(.use_catvars)) {
              # let it goooo !! ♪♪
              0
            }
          }
          main_env$EAL$page <- .tmp
        },
        label = paste("changePage", from, to)
      )
    }

    completeToggle <- function(from, to, main_env) {
      observe({
        shinyjs::toggleState(
          paste(from, to, sep = "_"),
          condition = isTRUE(main_env$EAL$completed)
        )
      })
    }

    ## Servers ====
    ids <- seq_along(steps)
    # Generate observers
    # Previous page
    lapply(ids[-1], function(i) {
      changePage(i, i - 1, input, main_env)
    })
    # Next page
    lapply(ids[-length(steps)], function(i) {
      changePage(i, i + 1, input, main_env)
      completeToggle(i, i + 1, main_env)
    })

    ## Side UI ====
    # Fully functional?
    sapply(isolate(main_env$VALUES$steps), function(page) {
      output[[paste0(page, "-tag_list")]] <- renderUI(main_env$EAL$tag_list)
    })

    ## Chain ====
    # TODO fun things to use: bsButton() bsTooltip()
  })
}

## Next ====

#' @import shiny
#'
#' @noRd
nextTabButton <- function(id, i) {
  shinyWidgets::actionBttn(
    NS(id, paste(i, i + 1, sep = "_")),
    "Next",
    icon("arrow-right"),
    block = TRUE
  )
}

## Previous ====

#' @import shiny
#'
#' @noRd
prevTabButton <- function(id, i) {
  shinyWidgets::actionBttn(
    NS(id, paste(i, i - 1, sep = "_")),
    "Previous",
    icon("arrow-left"),
    block = TRUE
  )
}
