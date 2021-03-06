#' @param id character. An ID string that corresponds with the ID used to call 
#' the module's UI function.
#' 
#' @import shiny
#' 
#' @noRd
aboutUI <- function(id) {
  fluidPage(
    tags$h1("About MetaShARK dev team"),
    HTML("MetaShARK is developped within the french Museum National 
          d'Histoire Naturelle / UMS Patrimoine Naturel / Pole 
          National de Biodiversite. Its development team is currently
          composed of <a href='https://fr.linkedin.com/in/elie-arnaud-440132151?trk=people-guest_profile-result-card_result-card_full-click'>Elie Arnaud</a>
          (lead developer) and <a href ='https://fr.linkedin.com/in/yvan-le-bras-aa2b3738?trk=people-guest_profile-result-card_result-card_full-click'>Yvan Le Bras</a> (team director)."),

    tags$h1("Thanks"),
    HTML("MetaShARK could not be built without the help of those people: </br>
        <ul>
          <li>Colin Smith (EDI, US)</li> who collaborates with us since March 
          2019, and currently provides us the <a href='https://github.com/EDIorg/EMLassemblyline'>EML Assembly Line</a> tool.
        </ul>"),

    tags$h1("References"),
    # Parties
    tags$h2("LTER and EML Actors"),
    tags$p("The following people and parties have worked upstream of
      MetaShARK dev team to provide the fundamentals united in
      the present tool."),
    uiOutput(NS(id, "actors")),
    # Informatics
    tags$h2("Computer material"),
    tags$p("The following references will lead the user to the source
      of tools and methods reused in MetaShARK."),
    uiOutput(NS(id, "informatics")),
    # Science litterature
    tags$h2("Literature"),
    tags$p("MetaShARK is based on the following papers and articles
      concerning mainly ecological metadata."),
    uiOutput(NS(id, "ecology"))
  )
}

#' @param id character. An ID string that corresponds with the ID used to call 
#' the module's UI function.
#' 
#' @import shiny
#'
#' @noRd
about <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Render bibliography for actors
    output$actors <- renderBibliography(
      system.file(
        "resources/bibliography-actors.bib",
        package = "MetaShARK"
      )
    )
    
    # Render bibliography for informatic part
    output$informatics <- renderBibliography(
      system.file(
        "resources/bibliography-informatics.bib",
        package = "MetaShARK"
      )
    )
    
    # Render bibliography for ecology part
    output$ecology <- renderBibliography(
      system.file(
        "resources/bibliography-ecology.bib",
        package = "MetaShARK"
      )
    )
  })
}
