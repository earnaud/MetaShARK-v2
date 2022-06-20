#' @noRd
devmsg <- function(fmt, ..., tag = "dev", devmode = shiny::in_devmode()) {
  text <- sprintf(paste0("[%s] ", fmt), tag, ...)

  # Output
  message(text)
}

#' @noRd
fillLine <- function(char, symbol = "=") {
  symbol <- substr(symbol, 1, 1)
  paste(char, paste0(rep(symbol, 80 - nchar(char)), collapse = ""))
}

hoursDifftime <- function(t1, t2) {
  difftime(t1, t2, units = "secs") |>
    ceiling() |>
    lubridate::seconds_to_period()
}
