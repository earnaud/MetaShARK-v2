#' insertModuleBox
#' 
#' Convenience function to insert a container able to auto-destroy.
#' 
#' @param id namespaced id for the UI (previous modules namespaces will be 
#' removed for server)
#' @param name name to display for the current container. If set to NULL (default), 
#' `id` will be used. If set to FALSE, no name will be displayed.
#' @param selector A string that is accepted by jQuery's selector (i.e. the 
#' string s to be placed in a $(s) jQuery call). (See `shiny::insertUI`)
#' @param moduleUI,moduleUI.args UI for the module to embed and its potential
#' arguments provided as a list (empty list by default).
#' @param module,module.args server for the module to embed and its potential
#' arguments provided as a list (empty list by default).
#' 
#' FIXME finish this module
#' @import shiny.grid
insertModuleBox <- function(
  id, name = NULL, selector, 
  moduleUI, moduleUI.args = list(), 
  module, module.args = list()
) {
  # Checks
  stopifnot(is.character(id))
  stopifnot(is.character(selector) && isFALSE("^#" %grep% selector))
  stopifnot(is.function(moduleUI) || is.character(moduleUI))
  stopifnot(is.list(moduleUI.args))
  stopifnot(is.function(module) || is.character(module))
  stopifnot(is.list(module.args))
  
  # Add row
  .id <- unns(id)
  # create the UI -- no NS !!!
  new.ui <- tags$div(
    id = NS(id, "container"),
    class = "inputBox",
    shiny.grid::gridPanel(
      class = "topInputRow",
      areas = c("collapse collapse remove"),
      columns = c("50px 1fr 50px"),
      
      # Collapse
      tags$div(
        class = "collapse",
        actionLink(NS(id, "collapse"), "", icon("chevron-right"))
      ),
      # Show name
      tags$div(
        class = "name",
        name,
        style="margin-top: 20px; padding: 6px; height: 40px;"
      ),
      # Remove UI
      tags$div(
        class = "remove",
        actionButton(NS(id, "remove"), "", icon("trash"), class = "redButton")
      )
    ), # end of header
    shinyjs::hidden(
      tags$div(
        id = NS(id, "content"),
        class = "contentRow",
        tagList(
          # User module itself
          do.call(
            moduleUI,
            id,
            moduleUI.args
          )
        )
      )
    ) # end of content
  )
  
  # insert the UI
  insertUI(selector = selector, ui = new.ui, immediate = TRUE)
  
  # create the server
  do.call(module, c(.id, module.args))
}

moduleBox <- function(id, ...) {
moduleServer(id, function(input, output, session) {
  if(!grepl("^_", .id)) {
    # add a new row to local table
    main.env$local.rv$data.files[nrow(main.env$local.rv$data.files)+1,] <- 
      c(rep("", ncol(main.env$local.rv$data.files)-1), id = .id)
  }
  
  })
}