#' @noRd
devmsg <- function(fmt, ..., tag = "dev", devmode = shiny::in_devmode(), timer.env = NULL) {
  text <- sprintf(paste0("[%s] ", fmt), tag, ...)
  if(!is.null(timer.env)){
    timer <- base::get("VALUES", envir = timer.env)$last.timer
    time.string <- difftime(Sys.time(), timer, units = "secs") |>
      round(1) |>
      lubridate::seconds_to_period() |>
      tolower()
    text <- paste0(text, ", at: ", time.string)
    assign("VALUES$last.timer", Sys.time(), envir = timer.env, immediate = TRUE)
  }
    
  # Output
  message(text)
}

#' @noRd
fillLine <- function(char, symbol = "=") {
  symbol <- substr(symbol, 1, 1)
  paste(char, paste0(rep(symbol, 80-nchar(char)), collapse = "") )
}

hoursDifftime <- function(t1, t2) {
  difftime(t1, t2, units = "secs") |>
    ceiling() |>
    lubridate::seconds_to_period()
}