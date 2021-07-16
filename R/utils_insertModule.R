#' Insert module
#' 
#' Use this function to insert a module and its associated server somewhere in
#' your UI. It is inserted with a way to remove it (contained in a div). This 
#' function only manages the graphical part. Server modules are still functional
#' and related data can still exist. 
#' 
#' @param id The id to use for the modules. If you call `insertModule` from another
#' module, make sure `id` is namespaced. 
#' @param selector A JS selector targetting the element after which the UI shall
#' be inserted (see `shiny::insertUI`).
#' @param moduleUI,moduleUI.args The module UI definition and its arguments OTHER
#' THAN `inputID`. Prepare them to be input in a `do.call` call.
#' @param module,module.args The module server definition and its arguments OTHER
#' THAN `inputID`. Prepare them to be input in a `do.call` call.
#' @param ui.class,ui.style CSS class and style to be applied to the module UI
#' container.
#' 
#' @export
insertModule <- function(
  id, selector,
  moduleUI, moduleUI.args = list(), 
  module, module.args = list(),
  ui.class = NULL,
  ui.style = "border: 1px solid lightgrey; border-radius: 10px; padding: 3px;
	margin: 3px;"
) {
  stopifnot(is.function(moduleUI))
  stopifnot(is.list(moduleUI.args))
  stopifnot(is.function(module))
  stopifnot(is.list(module.args))
  stopifnot(is.character(selector) && nzchar(selector))
  
  # Add row
  .id <- strsplit(id, "-") |> unlist() |> tail(1)
  
  # create the UI
  new.ui <- tags$div(
    # targettable container in namespace
    id = NS(id, "container"),
    class = ui.class,
    style = ui.style,
    tags$span(
      # class = "content",
      style="margin: 0 5px 0; display: inline-flex; width: 100%;",
      do.call(moduleUI, c(id, moduleUI.args)),
      # removal button
      tags$div(
        style="height: min-content; padding: 7px;",
        actionButton(
          NS(id, "remove_module"), 
          "",
          icon = icon("trash"),
          class = "redButton"
        )
      ),
      class = "remove"
    )
  )
  
  # insert the created UI
  insertUI(selector = selector, ui = new.ui, immediate = TRUE)
  
  # create the server
  do.call(module, c(.id, module.args))
  do.call(
    args = list(id = .id),
    function(id) {
      moduleServer(id, function(input, output, session) {
        message(sprintf("Looking for %s removal", session$ns("container")))
        
        observeEvent(input$remove_module, {
          message(sprintf("Removing %s", session$ns("container")))
          removeUI(
            selector = sprintf("#%s", session$ns("container")),
            immediate = TRUE,
            session = session
          )
        },
        label = sprintf("Remove %s", id))
        
      })
    }
  )
  
}