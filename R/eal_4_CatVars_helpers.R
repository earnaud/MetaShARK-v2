# * Write UI ====

#' @import shiny
#' @importFrom dplyr %>% select filter
#' @importFrom shinyBS bsCollapsePanel
#'
#' @noRd
CatVarsInputUI <- function(id, attribute, main.env) {
  # Shortcuts
  .file.name <- main.env$local.rv$current$file
  .tables <- main.env$local.rv$cv.tables
  
  codes <- .tables[[.file.name]] %>%
    dplyr::filter(attributeName == attribute) %>%
    dplyr::select(code)
  
  shinyBS::bsCollapsePanel(
    title = attribute,
    # value = attribute,
    ... = tagList(
      lapply(unlist(codes), function(.code) {
        # Correct value for NAs
        if (is.na(.code)) {
          .code <- "NA"
        }
        
        # proper UI
        textAreaInput(
          inputId = NS(id, gsub("[ [:punct:]]", "", .code)),
          label = .code,
          value = .tables[[.file.name]] %>%
            dplyr::filter(attributeName == attribute & code == .code) %>%
            dplyr::select(definition) %>%
            unique()
        )
      })
    ) # end of "tagapply" -- text areas
  ) # end of bsCollapsePanel
}

# * Write server ====

#' @import shiny
#' @importFrom dplyr %>% select filter
#'
#' @noRd
CatVarsInput <- function(id, attribute, main.env) {
  # id and attribute are the same, split for legibility purposes
  moduleServer(id, function(input, output, session){
    # Shortcuts
    .file.name <- main.env$local.rv$current$file
    .tables <- main.env$local.rv$cv.tables
    
    codes <- .tables[[.file.name]] %>%
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
        # Get value
        .row.index <- intersect(
          which(.tables[[.file.name]]$attributeName == attribute),
          which(.tables[[.file.name]]$code == .code)
        )
        
        if(isTruthy(input[[input.id]]))
          main.env$local.rv$cv.tables[[.file.name]][.row.index, "definition"] <- input[[input.id]]
      },
      label = paste("EAL4", attribute, .code, sep = ">")
      )
    })
  })
}