#' @title about
#'
#' @description server part of the about module.
#' 
#' @import shiny
about <- function(input, output, session) {
  ns <- session$ns

  .bibliography <- reactiveValues(
    actors = system.file("resources/bibliography-actors.bib", package = "MetaShARK"),
    informatics = system.file("resources/bibliography-informatics.bib", package = "MetaShARK"),
    ecology = system.file("resources/bibliography-ecology.bib", package = "MetaShARK"),
    misc = system.file("resources/bibliography-misc.bib", package = "MetaShARK")
  )

  # use function
  output$actors <- renderBibliography(.bibliography$actors)
  output$informatics <- renderBibliography(.bibliography$informatics)
  output$ecology <- renderBibliography(.bibliography$ecology)
}
