#' @title welcome
#'
#' @description Welcome page of the MetaShARK app.
#'
#' @importFrom shiny fluidPage headerPanel mainPanel HTML
welcomeUI <- function(id) {
  fluidPage(
    headerPanel("Welcome in MetaShARK"),
    mainPanel(
      HTML("
        <div id='Welcome'>
          <p><b>MetaShARK</b> (<b>Meta</b>data <b>Sh</b>iny <b>A</b>utomated 
          <b>R</b>esources and <b>K</b>nowledge) is a tool designed for ecology
          data description tasks. The tool relies on ecology metadata standards,
          and mainly the <a href='#EML'>Ecological 
          Metadata Language</a>. Its vocation is to allow any ecologist to fill 
          in metadata for its dataset to permit the understanding, resusability
          and reproducibility of his work. But as metadata is becoming more and
          more complex, this tool is trying to get as <i>user-friendly</i> as
          possible.</p>
        </div>
        
        <div id='EML'>
          <h2>Ecological Metadata Language</h2>
          <p>The Ecological Metadata Language has been developped since 1997
          from the <a href='https://www.researchgate.net/publication/220040725_Non-geospatial_metadata_for_the_ecological_sciences'>
          work of Michener et al.</a>. Since then, it has been developped by the 
          <b>NCEAS</b> (National Center for Ecological Analysis and Synthesis)
          and is mainly accessible through the <b>KNB</b> (Knowledge Network for
          Biodiversity) and its <a href='https://github.com/NCEAS/eml'>git</a>.
          However, MetaShARK offers a <a href='#shiny-tab-documentation'>documentation
          section</a> dedicated to EML. </p>
        </div>
        
        <div id='CatnShark'>
          <h2>MetaCat and MetaShARK</h2>
          <p>The application you are currently using is a front-end
          tool for any user who wants to contribute to a DataOne node
          repository, also known as <b>metadata catalog</b> or <b>MetaCat</b>.
          To contribute to a MetaCat, you need to login to the metacat
          in which you want to upload your data package. Then, it will 
          be possible for MetaShARK to gather the needed informations, as
          you will see while using the app.</p>
        </div>
      ")
    )
  )
}
