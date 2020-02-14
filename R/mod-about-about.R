#' @title about
#'
#' @description server part of the about module.
about <- function(input, output, session, IM) {
  ns <- session$ns

  # use function
  output$actors <- renderBibliography(bibliography$actors)
  output$informatics <- renderBibliography(bibliography$informatics)
  output$ecology <- renderBibliography(bibliography$ecology)
}
