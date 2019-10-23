#' @import RefManageR
about <- function(input, output, session, IM, bibliography){
  ns <- session$ns
  
  library("RefManageR")
  # source("R/modules/about/about_functions.R")
  
  # Bibliography - approximatively the same as 2019 master memoir
  bibliography <- list()
  bibliography$actors <- ReadBib("R/mod-about-actors.bib")
  message("* Actors bibliography successfully loaded !")
  bibliography$informatics <- ReadBib("R/mod-about-informatics.bib")
  message("* Informatics bibliography successfully loaded !")
  bibliography$ecology <- ReadBib("R/mod-about-ecology.bib")
  message("* Ecology bibliography successfully loaded !")
  
  # use function
  output$actors <- renderBibliography(bibliography$actors)
  output$informatics <- renderBibliography(bibliography$informatics)
  output$ecology <- renderBibliography(bibliography$ecology)
  
}
