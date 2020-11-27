#' @noRd
devmsg <- function(fmt, ..., tag = "dev") {
  text <- sprintf(paste0("[%s] ", fmt), tag, ...)
  message(text)
}