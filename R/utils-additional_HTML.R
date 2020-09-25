# Additional HTML

#' @title centered
#' 
#' @description Returns the content styled as centered text.
#' 
#' @param ... any HTML tag.
#' 
#' @return A div with `style = "text-align: center"`
#' 
#' @importFrom shiny tags
centered <- function(...) {
  tags$div(..., style = "text-align: center")
}

#' @title withRedStar
#'
#' @description Add a red star at the end of the text
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#'
#' @examples
#' withRedStar("Enter your name here")
#' @import shiny
withRedStar <- function(text) {
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

#' @title wipRow
#' 
#' @description styles an inputRow to give it WIP appearance
#' 
#' @param ... content to style
#' 
#' @return styled content as WIP
#' 
#' @importFrom shiny tags
wipRow <- function(...) {
  tags$div(
    tags$div(style = "height: 10px", class = "topInputRow wip"),
    ...,
    class = "inputBox"
  )
}

unns <- function(id) {
  strsplit(id, "-") %>% unlist %>% tail(., 1)
}

#' @title checkFeedback
#' 
#' relies on {shinyFeedback} to automate feedback on one input.
checkFeedback <- function(
  input, id, condition = NULL, silent = FALSE, type = c("danger", "warning"), text = NULL
) {
  type = type[1]
  
  shinyFeedback::hideFeedback(id)
  
  if(is.null(condition)) 
    condition <- isTruthy(input[[id]])
  
  if(condition) {
    shinyFeedback::showFeedbackSuccess(id)
  } else if(isFALSE(silent)) {
    if(type == "danger")
      shinyFeedback::showFeedbackDanger(id, text = text)
    if(type == "warning")
      shinyFeedback::showFeedbackWarning(id, text = text)
  }
}

# Clear server-side of a shiny module
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}
