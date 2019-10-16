# about.R

### UI ###
aboutUI <- function(id, IM){
  ns <- NS(id)
  
  fluidPage(
    h1("About MetaShARK dev team"),
    HTML("MetaShARK is developped within the french Museum National 
          d'Histoire Naturelle / UMS Patrimoine Naturel / Pole 
          National de Biodiversité. Its development team is currently
          composed of <a href='https://fr.linkedin.com/in/elie-arnaud-440132151?trk=people-guest_profile-result-card_result-card_full-click'>Elie Arnaud</a>
          (lead developer) and <a href ='https://fr.linkedin.com/in/yvan-le-bras-aa2b3738?trk=people-guest_profile-result-card_result-card_full-click'>Yvan Le Bras</a> (team director)."),
    h1("Thanks"),
    HTML("MetaShARK could not be built without the help of those people: </br>
        <ul>
          <li>Colin Smith (EDI, US)</li> who collaborates with us since March 2019, and currently provides us the <a href='https://github.com/EDIorg/EMLassemblyline'>EML Assembly Line</a> tool.
        </ul>"),
    h1("References"),
    h2("LTER and EML Actors"),
    p("The following people and parties have worked upstream of
      MetaShARK dev team to provide the fundamentals united in
      the present tool."), 
    uiOutput(ns("actors")),
    h2("Computer material"),
    p("The following references will lead the user to the source
      of tools and methods reused in MetaShARK."),
    uiOutput(ns("informatics")),
    h2("Literature"),
    p("MetaShARK is based on the following papers and articles
      concerning mainly ecological metadata."),
    uiOutput(ns("ecology"))
  )
}



### SERVER ###
about <- function(input, output, session, IM, bibliography){
  ns <- session$ns
  
  # use function
  output$actors <- renderBibliography(bibliography$actors)
  output$informatics <- renderBibliography(bibliography$informatics)
  output$ecology <- renderBibliography(bibliography$ecology)
  
}