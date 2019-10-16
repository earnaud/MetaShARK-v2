# fill.R

### UI ###
welcomeUI <- function(id, IM){
  ns <- NS(id)
  
  fluidPage(
    headerPanel("Welcome in MetaShARK"),
    mainPanel(
      HTML("
        <div id='Welcome'>
          <p><b>MetaShARK</b> (<b>Meta</b>data <b>Sh</b>iny <b>A</b>utomated <b>R</b>esources and <b>K</b>nowledge) is a tool designed for ecology
          data description tasks. The tool relies on ecology metadata standards, but currently its main standard is the <a href='#EML'>Ecological 
          Metadata Language</a>. Its vocation is to allow any ecologist to fill in metadata for its dataset to permit the understanding, resusability
          and reproducibility of his work. But as metadata is becoming more and more complex, this tool is trying to get as <i>user-friendly</i> as
          possible.</p>
        </div>
        
        <div id='EML'>
          <h2>Ecological Metadata Language</h2>
          <p>The Ecological Metadata Language has been developped since 1997 from the <a href='https://www.researchgate.net/publication/220040725_Non-geospatial_metadata_for_the_ecological_sciences'>
          work of Michener et al.</a>. Since then, it has been developped by the <b>NCEAS</b> (National Center for Ecological Analysis and Synthesis)
          and is mainly accessible through the <b>KNB</b> (Knowledge Network for Biodiversity) and its <a href='https://github.com/NCEAS/eml'>git</a>.
          However, MetaShARK offers a <a href='#shiny-tab-documentation'>documentation section</a> dedicated to EML. </p>
        </div>
      ")
    )
  )
  
}

### SERVER ###
welcome <- function(input, output, session, IM){
  ns <- session$ns
  
  output$icon <- renderImage({ list(src = "resources/pictures/MetaShARK_icon2.png",
                                    contentType = "image/png",
                                    width = "240px",
                                    height = "120px") }, deleteFile = FALSE)
}