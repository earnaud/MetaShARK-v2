#' @title Navigation sidebar
#'
#' @description UI part for shiny navigation sidebar module
#'
#' @param id shiny id
#' @param class css class style. Default is set to "navSidebar":
#' .navSidebar {
#'   width: 100%;
#'   text-align: center;
#'   padding: 0;
#' }
#' @param .prev logical. Do you want a "Previous" actionButton? (prevTabButton)
#' @param .next logical. Do you want a "Next" actionButton? (nextTabButton)
#' @param ... UI tags set at the bottom of the navigation side bar
#'
#' @importFrom shiny NS HTML tags
navSidebar <- function(id, class = "navSidebar",
                       .prev = TRUE, .next = TRUE,
                       disableNext = FALSE,
                       disablePrev = FALSE,
                       ...) {
  ns <- NS(id)

  # variable initialization
  preBut <- if (.prev) prevTabButton(id) else HTML(NULL)
  nexBut <- if (.next) nextTabButton(id) else HTML(NULL)
  arguments <- list(...)
  div(
    verticalLayout(
      tags$h4("Navigation"),
      quitButton(id),
      saveButton(id),
      if (disablePrev) disabled(preBut) else preBut,
      if (disableNext) disabled(nexBut) else nexBut,
      arguments
    ),
    class = class
  )
}

#' @describeIn navSidebar Quit button.
#'
#' @importFrom shiny NS actionButton icon
quitButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("quit"), "Quit",
    icon = icon("sign-out-alt"),
    width = "100%"
  )
}

#' @describeIn navSidebar Save button.
#'
#' @importFrom shiny NS actionButton icon
saveButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("save"), "Save",
    icon = icon("save", class = "regular"),
    width = "100%"
  )
}

#' @describeIn navSidebar Next button.
#'
#' @importFrom shiny NS actionButton icon
nextTabButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("nextTab"), "Next",
    icon = icon("arrow-right"),
    width = "100%"
  )
}

#' @describeIn navSidebar Prev button.
#'
#' @importFrom shiny NS actionButton icon
prevTabButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("prevTab"), "Previous",
    icon = icon("arrow-left"),
    width = "100%"
  )
}

## Associated server functions ----

#' @title Navigation server
#'
#' @description server part for shiny navigation sidebar module (see [navSidebar()]).
#' The functions are very specific and thus are not exported.
#'
#' @importFrom shiny modalDialog tagList modalButton actionButton icon observeEvent req showModal removeModal
onQuit <- function(input, output, session,
                   globals, savevar) {
  ns <- session$ns

  # modal dialog for quitting data description
  quitModal <- modalDialog(
    title = "You are leaving data description",
    "Are you sure to leave? Some of your metadata have maybe not been saved.",
    footer = tagList(
      modalButton("Cancel"),
      actionButton(
        ns("save_quit_button"), 
        "Save & Quit"
      ),
      actionButton(
        ns("quit_button"), 
        "Quit",
        icon("times-circle"),
        class = "redButton"
      )
    )
  )

  # show modal on 'quit' button clicked
  observeEvent(input$quit, {
      req(input$quit)
      showModal(quitModal)
    }, priority = -1)

  # calls saveRDS method and quits
  observeEvent(input$save_quit_button, {
    req(input$quit)
    removeModal()
    file.remove(
      list.files(
        savevar$emlal$SelectDP$dp_data_path,
        pattern = "preview___"
      )
    )
    saveReactive(savevar)
    globals$EMLAL$NAVIGATE <- 1
  })

  # quits simply
  observeEvent(input$quit_button, {
    req(input$quit)
    removeModal()
    file.remove(
      list.files(
        savevar$emlal$SelectDP$dp_data_path,
        pattern = "preview___"
      )
    )
    globals$EMLAL$NAVIGATE <- 1
  })
}

#' @describeIn onQuit
#'
#' @importFrom shiny observeEvent
onSave <- function(input, output, session,
                   savevar) {
  observeEvent(input$save, {
    req(input$save)
    saveReactive(savevar)
  }, priority = -1)
}

#' @describeIn onQuit
#'
#' @importFrom shiny observeEvent
nextTab <- function(input, output, session,
                    globals, previous) {
  observeEvent(input$nextTab,
    {
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + 1
      globals$EMLAL$HISTORY <- c(globals$EMLAL$HISTORY, previous)
    },
    priority = -1
  )
}
#' @describeIn onQuit
#'
#' @importFrom shiny observeEvent
prevTab <- function(input, output, session,
                    globals) {
  observeEvent(input$prevTab,
    {
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE - 1
      globals$EMLAL$HISTORY <- head(globals$EMLAL$HISTORY, -1)
    },
    priority = -1
  )
}

# Misc ----

# R to JS boolean
r2js.boolean <- function(condition) {
  if (is.character(condition)) condition <- as.logical(condition)
  return(tolower(as.character(condition)))
}

#' @importFrom shiny is.reactive
printReactiveValues <- function(values) {
  sapply(
    names(values),
    function(nn) {
      if (is.reactive(values[[nn]])) {
        values[[nn]]()
      } else {
        values[[nn]]
      }
    }
  )
}
