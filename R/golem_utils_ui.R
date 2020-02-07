#' @title with_red_star
#'
#' @description Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#'
#' @examples
#' with_red_star("Enter your name here")
#' @importFrom htmltools tags HTML
with_red_star <- function(text) {
  tags$span(
    HTML(
      paste0(
        text,
        tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
}
