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
        .value <- .tables[[table.name]] %>%
          dplyr::filter(attributeName == attribute & identical(code, .code)) %>%
          dplyr::select(definition) %>%
          unique() %>% 
          unlist()
        if (is.na(.code) || .code == "") {
          .code <- "NA"
        }
        if(length(.value) == 0){
          .value <- sprintf("No description provided for: %s", .code)
        }
        if(length(.code) == 0 || .code == "NA")
          devmsg("%s:%s = %s", table.name, attribute, .code)
        
        # proper UI
        textAreaInput(
          inputId = NS(id, gsub("[ [:punct:]]", "", .code)),
          label = ifelse(is.na(.code), "NA", .code),
          value = .value
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
    .tables <- main.env$local.rv$cv.tables
    
    codes <- .tables[[table.name]] %>%
      dplyr::filter(attributeName == attribute) %>%
      dplyr::select(code)
    
    sapply(unlist(codes), function(.code) {
      # Correct value for NAs
      if (is.na(.code) || .code == "") {
        .code <- "NA"
      }
      
      # Set input id
      input.id <- gsub("[ [:punct:]]", "", .code)
      
      observeEvent(input[[input.id]], {
        # validity check
        .valid <- isTruthy(input[[input.id]])
        if(.code == "NA")
          devmsg(.valid, tag = paste("catvar:", .code))
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