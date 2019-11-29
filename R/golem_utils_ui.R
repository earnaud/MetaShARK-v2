# Hide or display a tag
# @param tag the tag
# @return a tag
# @examples
# ## Hide
# a <- shiny::tags$p(src = "plop", "pouet")
# undisplay(a)
# b <- shiny::actionButton("go_filter", "go")
# undisplay(b)

#' @importFrom htmltools tagList
undisplay <- function(tag) {
  # if not already hidden
  if (!is.null(tag$attribs$style) && !grepl("display:\\s+none", tag$attribs$style)) {
    tag$attribs$style <- paste("display: none;", tag$attribs$style)
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' @importFrom htmltools tagList
display <- function(tag) {
  if (!is.null(tag$attribs$style) && grepl("display:\\s+none", tag$attribs$style)) {
    tag$attribs$style <- gsub("(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*", "", tag$attribs$style)
  }
  tag
}

#' @title jq_hide
#' 
#' @description Hide an elements by calling jquery hide on it
#' 
#' @importFrom htmltools tags
jq_hide <- function(id) {
  tags$script(sprintf("$('#%s').hide()", id))
}

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
#'
#' @importFrom htmltools tags HTML
with_red_star <- function(text) {
  htmltools::tags$span(
    HTML(
      paste0(
        text,
        htmltools::tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
}
