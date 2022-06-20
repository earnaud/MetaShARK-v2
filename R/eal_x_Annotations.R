library(shiny)
library(shinyTree)

annotations <- data.table::fread(system.file("examples/pkg_260/metadata_templates_overflow/annotations.txt", package = "EMLassemblyline"), stringsAsFactors = F, data.table = F)

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
