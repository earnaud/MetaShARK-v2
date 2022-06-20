#' Insert module
#'
#' Use this function to insert a module and its associated server somewhere in
#' your UI. It is inserted with a way to remove it (contained in a div). This
#' function only manages the graphical part. Server modules are still functional
#' and related data can still exist.
#'
#' @param id The id to use for the modules. If you call `insertModule` from
#' another module, make sure `id` is namespaced.
#' @param selector A JS selector targetting the element after which the UI shall
#' be inserted (see `shiny::insertUI`).
#' @param moduleUI,moduleUI_args The module UI definition and its arguments
#' OTHER THAN `inputID`. Prepare them to be input in a `do.call` call.
#' @param module,module_args The module server definition and its arguments
#' OTHER THAN `inputID`. Prepare them to be input in a `do.call` call.
#' @param container_class,container_style CSS class and style to be applied to
#' the module UI
#' container.
#'
#' @export
insertModule <- function(
    id,
    selector,
    moduleUI,
    moduleUI_args = list(),
    module,
    module_args = list(),
    container_class = NULL,
    container_style = "border: 1px solid lightgrey; border-radius: 10px;
  padding: 3px; margin: 3px;") {
  # Validity checks ====
  stopifnot(is.function(moduleUI))
  stopifnot(is.list(moduleUI_args))
  stopifnot(is.function(module))
  stopifnot(is.list(module_args))
  stopifnot(is.character(selector) && nzchar(selector))

  # Set variables ====
  # Add row
  .id <- unns(id)

  # create the UI
  new_ui <- tags$div(
    # targettable container in namespace
    id = NS(id, "container"),
    class = container_class,
    style = container_style,
    tags$span(
      # class = "content",
      style = "margin: 0 5px 0; display: inline-flex; width: 100%;",
      do.call(moduleUI, c(id, moduleUI_args)),
      # removal button
      tags$div(
        style = "height: min-content; padding: 7px;",
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

  # Insert UI ====
  insertUI(selector = selector, ui = new_ui, immediate = TRUE)

  # create the server
  do.call(module, c(.id, module_args))
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
          label = sprintf("Remove %s", id)
        )
      })
    }
  )
}
