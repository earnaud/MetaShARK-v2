# UI ====

#' @import shiny
#' @importFrom dplyr %>% select filter
#' @importFrom shinyBS bsCollapsePanel
#'
#' @noRd
CatVarsInputUI <- function(id, attribute, table.name, main.env) {
  # Shortcuts
  # .file.name <- main.env$local.rv$current$file
  .tables <- main.env$local.rv$cv.tables
  
  codes <- .tables[[table.name]] %>%
    dplyr::filter(attributeName == attribute) %>%
    dplyr::select(code)
  
  shinyBS::bsCollapsePanel(
    title = attribute,
    # value = attribute,
    ... = tagList(
      lapply(unlist(codes), function(.code) {
        # Correct value for NAs
        if (is.na(.code) || .code == "") {
          browser()
          .code <- "NA"
        }
        
        # proper UI
        textAreaInput(
          inputId = NS(id, gsub("[ [:punct:]]", "", .code)),
          label = ifelse(is.na(.code), "NA", .code),
          value = .tables[[table.name]] %>%
            dplyr::filter(attributeName == attribute & code == .code) %>%
            dplyr::select(definition) %>%
            unique()
        )
      })
    ) # end of "tagapply" -- text areas
  ) # end of bsCollapsePanel
}

# Server ====

#' @import shiny
#' @importFrom dplyr %>% select filter
#'
#' @noRd
CatVarsInput <- function(id, attribute, table.name, main.env) {
  moduleServer(id, function(input, output, session){
    # Shortcuts
    # .file.name <- main.env$local.rv$current$file
    .tables <- main.env$local.rv$cv.tables
    
    codes <- .tables[[table.name]] %>%
      dplyr::filter(attributeName == attribute) %>%
      dplyr::select(code)
    
    sapply(unlist(codes), function(.code) {
      # Correct value for NAs
      if (is.na(.code)) {
        .code <- "NA"
      }
      
      # Set input id
      input.id <- gsub("[ [:punct:]]", "", .code)
      
      observeEvent(input[[input.id]], {
        # validity check
        .valid <- isTruthy(input[[input.id]])
        shinyFeedback::hideFeedback(input.id)
        
        if(isTRUE(.valid)) {
          shinyFeedback::showFeedbackSuccess(input.id)
          # set value
          main.env$local.rv$cv.tables[[table.name]] %>%
            dplyr::filter(attributeName == attribute, code == .code) %>%
            dplyr::mutate(definition = input[[input.id]])
        }
        else {
          shinyFeedback::showFeedbackDanger(
            input.id,
            text = "Invalid description provided."
          )
        }
        
        main.env$local.rv$completed[[table.name]][[attribute]][.code] <<- .valid
        main.env$local.rv$trigger$trigger()
      },
      label = paste("EAL4:", attribute, .code)
      )
    })
  })
}