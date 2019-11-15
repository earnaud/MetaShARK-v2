# EMLAL_functions.R

## Navigation UI ----

#' @title quitButton
#'
#' @description button with a 'quit' styling
#'
#' @importFrom shiny NS actionButton icon
quitButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("quit"), "Quit",
    icon = icon("sign-out-alt")
  )
}

#' @title saveButton
#'
#' @description button with a 'save' styling
#'
#' @importFrom shiny NS actionButton icon
saveButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("save"), "Save",
    icon = icon("save", class = "regular")
  )
}

#' @title nextTab
#'
#' @description button with a 'next' styling
#'
#' @importFrom shiny NS actionButton icon
nextTabButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("nextTab"), "Next",
    icon = icon("arrow-right")
  )
}

#' @title prevTab
#'
#' @description button with a 'previous' styling
#'
#' @importFrom shiny NS actionButton icon
prevTabButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("prevTab"), "Previous",
    icon = icon("arrow-left")
  )
}

#' @title navSideBar
#'
#' @description container thought to embed Shiny tags allowing navigation
#' in a shiny app.
#'
#' @param .prev logical. Shall 'previous' button appear?
#' @param .next logical. Shall 'next' button appear?
#'
#' @return A shiny tagList.
#'
#' @importFrom shiny NS actionButton icon HTML
navSidebar <- function(id, class = "navSidebar",
                       .prev = TRUE, .next = TRUE,
                       ...) {
  ns <- NS(id)

  # variable initialization
  preBut <- if (.prev) prevTabButton(id) else HTML(NULL)
  nexBut <- if (.next) nextTabButton(id) else HTML(NULL)
  arguments <- list(...)
  div(h4("Navigation"),
    quitButton(id),
    saveButton(id),
    preBut,
    nexBut,
    arguments,
    class = class
  )
}

## Associated server functions ----

#' @title onQuit
#'
#' @description server part of the quitButton UI.
#'
#' @param globals global variable containing application settings
#' @param toSave R objects to save
#' @param path directory location where the file will be saved
#' @param filename name of the file to save
#'
#' @importFrom shiny modalDialog tagList modalButton actionButton
#' observeEvent req showModal removeModal
onQuit <- function(input, output, session,
                   globals, toSave, path, filename) {
  ns <- session$ns

  # modal dialog for quitting data description
  quitModal <- modalDialog(
    title = "You are leaving data description",
    "Are you sure to leave? Some of your metadata have maybe not been saved.",
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("save_quit_button"), "Save & Quit"),
      actionButton(ns("quit_button"), "Quit", icon("times-circle"),
        class = "redButton"
      )
    )
  )

  # show modal on 'quit' button clicked
  observeEvent(input$quit,
    {
      req(input$quit)
      showModal(quitModal)
    },
    priority = -1
  )

  # calls saveRDS method and quits
  observeEvent(input$save_quit_button, {
    req(input$quit)
    removeModal()
    saveReactive(toSave, path, filename)
    globals$EMLAL$NAVIGATE <- 1
  })

  # quits simply
  observeEvent(input$quit_button, {
    req(input$quit)
    removeModal()
    globals$EMLAL$NAVIGATE <- 1
  })
}

#' @title onSave
#'
#' @description server side of the 'saveButton' UI
#'
#' @param toSave R objects to save
#' @param path directory location where the file will be saved
#' @param filename name of the file to save
#'
#' @importFrom shiny observeEvent
onSave <- function(input, output, session,
                   toSave, path, filename) {
  ns <- session$ns

  observeEvent(input$save,
    {
      saveReactive(toSave, path, filename)
    },
    priority = -1
  )
}

#' @title nextTab
#'
#' @description server side of the 'nextTabButton' UI. Allows to
#' increment a specific variable contained in globals by 1.
#'
#' @param globals global variable containing application settings
#' @param previous character. Name of the recently quit page.
#'
#' @importFrom shiny observeEvent
nextTab <- function(input, output, session,
                    globals, previous) {
  observeEvent(input$nextTab,
    {
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + 1
      globals$EMLAL$PREVIOUS[1] <- previous
    },
    priority = -1
  )
}

#' @title prevTab
#'
#' @description server side of the 'prevTabButton' UI. Allows to
#' decrement a specific variable contained in globals by 1.
#'
#' @param globals global variable containing application settings
#' @param previous character. Name of the recently quit page.
#'
#' @importFrom shiny observeEvent
prevTab <- function(input, output, session,
                    globals, previous) {
  observeEvent(input$prevTab,
    {
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE - 1
      globals$EMLAL$PREVIOUS[1] <- previous
    },
    priority = -1
  )
}

# Files management ----
#' @title createDPFolder
#'
#' @description if a folder is about to be created with same name and
#' location an existing folder owns, the existing folder is deleted
#' and a message appear through a shiny modal.
#' _(the 'DP' prefixing refers to 'data package')_
#' _(this function is not used in MetaShARK)_
#'
#' @param DP.location the directory path where to create the folder
#' @param DP.name the name to set on the folder
#'
#' @importFrom shiny showModal modalDialog span modalButton
createDPFolder <- function(DP.location, DP.name) {
  if (dir.exists(paste0(DP.location, DP.name))) {
    unlink(paste0(DP.location, DP.name), recursive = TRUE)
    showModal(modalDialog(
      title = "Information: directory deleted",
      span(paste0(DP.location, DP.name), "has been deleted and replaced by a new empty data package."),
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  }

  template_directories(
    path = DP.location,
    dir.name = DP.name
  )
}

# EAL Templates ----

# very local function
saveInput <- function(RV) {
  # save attributes
  RV$attributesTable[RV$current_attribute, ] <- printReactiveValues(
    RV$attributes
  )[names(RV$attributesTable)]

  return(RV)
}

# build a unique id from file, attribute and colname - attribute_tables
buildInputID <- function(filename, attribute, colname) {
  paste(filename, attribute, colname, sep = "_")
}

# Misc ----

# R to JS boolean
r2js.boolean <- function(condition) {
  if (is.character(condition)) condition <- as.logical(condition)
  return(tolower(as.character(condition)))
}

#' @title printReactiveValues
#'
#' @description clean way to display reactiveValues. This only works
#' with one-level-deep reactiveValues
#'
#' @param values reactiveValues. The reactiveValues object to return
#'
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

passcat <- function(i) {
  i <<- i + 1
  cat(i, "\n")
}
