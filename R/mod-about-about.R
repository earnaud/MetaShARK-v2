#' @title about - server
#'
#' @description this is the function in charge of the server part for 'About'
#' module
about <- function(input, output, session) {
  ns <- session$ns

  # use function
  output$actors <- renderBibliography(bibliography$actors)
  output$informatics <- renderBibliography(bibliography$informatics)
  output$ecology <- renderBibliography(bibliography$ecology)
}
