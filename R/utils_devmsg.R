#' @noRd
#' @importFrom shiny in_devmode
devmsg <- function(fmt, ..., tag = "dev", devmode = shiny::in_devmode()) {
  text <- sprintf(paste0("[%s] ", fmt), tag, ...)
  message(text)
}