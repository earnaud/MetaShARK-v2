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
#' 
#' @export
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
#' 
#' @import shiny
#' 
#' @export
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
#' 
#' @export
wipRow <- function(..., id = NULL) {
  tags$div(
    id = id,
    tags$div(style = "height: 10px", class = "topInputRow wip"),
    tags$hr(),
    ...,
    class = "inputBox wipbox"
  )
}

unns <- function(id) {
  strsplit(id, "-") |> unlist() |> tail(1)
}

#' @title checkFeedback
#' 
#' @description relies on {shinyFeedback} to automate feedback on one input.
#' 
#' @param id character. An ID string that corresponds with the ID used to call 
#' the module's UI function.
#' @param condition logical. Determines if the feedback is positive or not.
#' @param silent logical. If TRUE, the only feedback occurs when `condition` is
#' met. If FALSE, also displays feedbacks for other cases.
#' @param type character. Either "danger" or "warning", sets the type of 
#' feedback in case `condition` is not met.
#' @param text character. What message to display in case of unmet condition.
#' 
#' @import shinyFeedback
#' 
#' @export
checkFeedback <- function(
  input, id, condition = NULL, silent = FALSE, type = c("danger", "warning"), text = NULL
) {
  if(isFALSE(is.character(type) && type %in% c("danger", "warning")))
    type = "danger"
  else 
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
  
  return(condition)
}

#' Clear backstage shiny observers
#'
#' Clear server-side of a shiny module
#' 
#' @param id character. An ID string that corresponds with the ID used to call 
#' the module's UI function.*
#' @param .input internal. Shiny server `input` variable passed to servers.
#' 
#' @details 
#' Freely teached from a community soluce on
#' [appsilon](https://appsilon.com/how-to-safely-remove-a-dynamic-shiny-module/).
#' 
#' @export
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}
