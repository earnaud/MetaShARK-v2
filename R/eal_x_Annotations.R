library(shiny)
library(shinyTree)

annotations = data.table::fread("/home/pndb-elie/R/x86_64-pc-linux-gnu-library/4.1/EMLassemblyline/examples/pkg_260/metadata_templates_overflow/annotations.txt", stringsAsFactors = F, data.table = F)

Annotations_UI <- fluidPage(
  column(
    3,
    shinyTree("tree")
  ),
  column(
    9,
    fluidRow(
      h2("Selected node"),
      actionButton("add_annotation", "", icon("plus"))
    ),
    fluidRow(h2("CEDAR browser"))
  )
)

Annotations <- function(input, output, session) {
  
}

shinyApp(ui, server)