#' @title navSidebarUI
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
#' @import shiny
navSidebarUI <- function(id, class = "navSidebar",
                         .prev = TRUE, .next = TRUE) {
  ns <- NS(id)

  tags$div(
    id = "navsidebar",
    verticalLayout(
      tags$h4("Navigation", class = "text-title"),
      quitButton(id),
      saveButton(id),
      if (isTRUE(.prev)) prevTabButton(id) else NULL,
      if (isTRUE(.next)) nextTabButton(id) else NULL,
      uiOutput(ns("NSB_custom_ui")),
      tags$hr(),
      actionButton(ns("EAL_help"), "Help")
    ),
    class = class
  )
}

#' @describeIn navSidebarUI
#'
#' @import shiny
quitButton <- function(id) {
  ns <- NS(id)

  actionButton(
    ns("quit"),
    "Quit",
    icon = icon("sign-out-alt"),
    width = "100%"
  )
}

#' @describeIn navSidebarUI
#'
#' @import shiny
saveButton <- function(id) {
  ns <- NS(id)

  actionButton(
    ns("save"),
    "Save",
    icon = icon("save", class = "regular"),
    width = "100%"
  )
}

#' @describeIn navSidebarUI
#'
#' @import shiny
nextTabButton <- function(id) {
  ns <- NS(id)

  actionButton(
    ns("nextTab"),
    "Next",
    icon = icon("arrow-right"),
    width = "100%"
  )
}

#' @describeIn navSidebarUI
#'
#' @import shiny
prevTabButton <- function(id) {
  ns <- NS(id)

  actionButton(
    ns("prevTab"),
    "Previous",
    icon = icon("arrow-left"),
    width = "100%"
  )
}

## Associated server functions -----------------------------------------------------

#' @title navSidebar
#'
#' @description server part for shiny navigation sidebar module (see [navSidebar()]).
#' The functions are very specific and thus are not exported.
#'
#' @param id shiny server id
#' @param main.env MetaShARK main.env variable
#' @param savevar MetaShARK savevar variable
#'
#' @import shiny
navSidebar <- function(id, main.env, savevar) {
  NSB <- reactiveValues(
    SAVE = 0,
    NEXT = 0,
    PREV = 0,
    tag.list = tagList(),
    help = modalDialog()
  )

  NSB <- callModule(onQuit, id, main.env, savevar, NSB)
  NSB <- callModule(onSave, id, savevar, NSB)
  NSB <- callModule(prevTab, id, main.env, NSB)
  NSB <- callModule(nextTab, id, main.env, savevar, NSB)

  # Custom on-the-fly server
  callModule(
    function(input, output, session, x = NSB) {
      output$NSB_custom_ui <- renderUI({
        x$tag.list
      })

      observeEvent(input$EAL_help, {
        showModal(x$help)
      })
    },
    id
  )

  return(NSB)
}

#' @describeIn navSidebar
#'
#' @import shiny
#' @importFrom shinyjs onclick disable enable
onQuit <- function(input, output, session,
                   main.env, savevar, NSB) {
  ns <- session$ns

  # modal dialog for quitting data description
  quitModal <- modalDialog(
    title = "You are leaving data description",
    "Are you sure to leave? Some of your metadata have maybe not been saved.",
    easyClose = FALSE,
    footer = tagList(
      actionButton(ns("cancel"), "Cancel"),
      actionButton(ns("save_quit_button"), "Save & Quit"),
      actionButton(
        ns("quit_button"),
        "Quit",
        icon("times-circle"),
        class = "redButton"
      )
    )
  )

  # show modal on 'quit' button clicked
  observeEvent(input$quit,
    {
      endisableNSB(input, disable)
      req(input$quit)
      showModal(quitModal)
    },
    label = "NSB quit"
  )

  # quits simply
  observeEvent(input$cancel,
    {
      req(input$quit)
      req(input$cancel)
      endisableNSB(input, enable)
      removeModal()
    },
    label = "NSB cancel"
  )

  # calls saveRDS method and quits
  observeEvent(input$save_quit_button,
    {
      req(input$quit)
      req(input$save_quit_button)
      endisableNSB(input, enable)
      removeModal()

      NSB$tagList <- tagList()
      NSB$SAVE <- NSB$SAVE + 1
      saveReactive(savevar)
      main.env$EAL$history <- "SelectDP"
      main.env$EAL$navigate <- 1

      file.remove(
        list.files(
          savevar$emlal$SelectDP$dp.data.path,
          pattern = "preview_"
        )
      )
    },
    label = "NSB save+quit"
  )

  # quits simply
  observeEvent(input$quit_button,
    {
      req(input$quit)
      req(input$quit_button)
      endisableNSB(input, enable)
      removeModal()

      NSB$tag.list <- tagList()
      main.env$EAL$history <- "SelectDP"
      main.env$EAL$navigate <- 1

      file.remove(
        list.files(
          savevar$emlal$SelectDP$dp.data.path,
          pattern = "preview___"
        )
      )
    },
    label = "NSB +quit"
  )

  return(NSB)
}

#' @describeIn navSidebar
#'
#' @import shiny
#' @importFrom shinyjs onclick disable enable
onSave <- function(input, output, session, savevar, NSB) {
  observeEvent(input$save,
    {
      req(input$save)
      NSB$SAVE <- NSB$SAVE + 1
    },
    label = "NSB save"
  )

  return(NSB)
}

#' @describeIn navSidebar
#'
#' @import shiny
#' @importFrom shinyjs onclick enable disable
nextTab <- function(input, output, session,
                    main.env, savevar, NSB) {

  observe(
    {
      if (isFALSE(main.env$EAL$current[2])) {
        disable("nextTab")
      } else if (isTRUE(main.env$EAL$current[2])) {
        enable("nextTab")
      }
    },
    label = "NSB cplt"
  )

  observeEvent(input$nextTab,
    {
      if(!isTRUE(main.env$EAL$current[2]))
        showNotification(
          "Check input information: some are not valid.", 
          type = "warning"
        )
      req(isTRUE(main.env$EAL$current[2]))
      endisableNSB(input, disable)
      if (!main.env$EAL$current[1] %in% c("Geographic Coverage", "Taxonomic Coverage")) {
        main.env$EAL$navigate <- main.env$EAL$navigate + 1
        NSB$tag.list <- tagList()

        # Savevar modification
        savevar$emlal$step <- main.env$EAL$navigate
      }
      NSB$NEXT <- NSB$NEXT + 1
      endisableNSB(input, enable)
    },
    label = "NSB next"
  )

  return(NSB)
}
#' @describeIn navSidebar
#'
#' @import shiny 
#' @importFrom shinyjs onclick enable disable
prevTab <- function(input, output, session, main.env, NSB) {
  observeEvent(input$prevTab,
    {
      endisableNSB(input, disable)
      main.env$EAL$navigate <- main.env$EAL$navigate - 1
      NSB$tag.list <- tagList()
      NSB$PREV <- NSB$PREV + 1
      endisableNSB(input, enable)
    },
    label = "NSB prev"
  )

  return(NSB)
}

#' @importFrom shinyjs enable disable
#' 
#' @noRd
endisableNSB <- function(input, todo) {
  .n <- names(input)
  .n <- .n[which(.n %in% c("save", "quit", "nextTab", "prevTab"))]
  sapply(.n, function(ui, action = todo) {
    action(ui)
  })
}