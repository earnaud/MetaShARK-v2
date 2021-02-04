#' @import shiny
#' 
#' @noRd
welcomeUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    mainPanel(
      width = 12,
      # MetaShARK
      tags$h1("Welcome in MetaShARK"),
      fluidRow(
        wipRow(
          tags$p("DISCLAIMER: some features are still in development. Some parts
            with this color code are not meant to be fully functional.")
        ),
        column(
          4,
          tags$h3("MetaShARK"),
          tags$p(
            tags$b("MetaShARK"), HTML("(<b>Meta</b>data 
            <b>Sh</b>iny <b>A</b>utomated <b>R</b>esources and 
            <b>K</b>nowledge) is a tool designed for ecology
            data description tasks. The tool relies on ecology metadata standards,
            and mainly the <a href='https://eml.ecoinformatics.org/'>Ecological 
            Metadata Language</a>. Its vocation is to allow any ecologist to fill 
            in metadata for its dataset to permit the understanding, resusability
            and reproducibility of his work. But as metadata is becoming more and
            more complex, this tool is trying to get as <i>user-friendly</i> as
            possible.")
          )
        ),
        column(
          4,
          tags$h3("EML"),
          tags$p(
            "The", tags$b("Ecological Metadata Language"), HTML("has been 
            developped since 1997 from the work of <a href='https://www.researchgate.net/publication/220040725_Non-geospatial_metadata_for_the_ecological_sciences'>
            Michener et al.</a>. Since then, it has been developped by the 
            <b>NCEAS</b> (National Center for Ecological Analysis and Synthesis) 
            and is mainly accessible through its <a href='https://github.com/NCEAS/eml'>github 
            repository</a>. However, MetaShARK offers a <a href='#shiny-tab-documentation'>documentation
            section</a> dedicated to EML.")
          )
        ),
        column(
          4,
          tags$h3("MetaCat & MetaShark"),
          tags$p(
            HTML("The application you are currently using is a front-end
            tool for any user who wants to contribute to a DataOne node
            repository, also known as <b>metadata catalog</b> or <b>MetaCat</b>.
            To contribute to a MetaCat, you need to login to the metacat
            in which you want to upload your data package. Then, it will 
            be possible for MetaShARK to gather the needed informations, as
            you will see while using the app.")
          )
        )
      ),
      # EAL
      tags$h1("About EML Assembly Line"),
      fluidRow(
        column(
          4,
          tags$h3("Authorship"),
          tags$p(
            HTML("The `EML Assembly Line` package used in this module and its
            children is the intellectual property of the Environment Data 
            Initiative (EDI). You can find further details on their 
            <a href=https://github.com/EDIorg/EMLassemblyline>git repository</a>.")
          ),
          tags$img(
            src = "media/edi_logo_small.png",
            width = "100px",
            height = "100px"
          )
        ),
        column(
          8,
          tags$h3("Usage"),
          HTML(
            "<p><b>EMLassemblyline</b> is a metadata builder for scientists
            and data managers who need to easily create high quality
            <b>EML</b> metadata for data publication. It emphasizes
            auto-extraction of metadata, appends value added content,
            and accepts user supplied inputs through template files
            thereby minimizing user effort while maximizing the potential
            of future data discovery and reuse. EMLassemblyline requires
            no familiarity with EML, is great for managing 10-100s of
            data packages, accepts all data formats, and supports complex
            and fully reproducible science workflows. Furthermore, it
            incorporates <a href=\"https://environmentaldatainitiative.files.wordpress.com/2017/11/emlbestpractices-v3.pdf\">EML best practices</a>,
            is based on a simple file organization scheme, and is not tied to a specific data repository.</p>
            <i>(preface by Colin Smith, EDI)</i>"
          )
        ) # end about
      )
    )
  )
}
