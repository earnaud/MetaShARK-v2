library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  uiOutput("edit_files")
)

server <- function(input, output, session) {
  output$edit_files <- renderUI({
    files <- dir("~/Documents/Test data/EAL samples/", full.names = TRUE)
    tables <- setNames(
      lapply(files, data.table::fread, data.table = FALSE, stringsAsFactors = FALSE),
      basename(files)
    )
    
    do.call(
      tabsetPanel,
      args = c(
        id = session$ns("tabset"),
        lapply(
          basename(files),
          tables = tables,
          # file UI - tab content
          function(table.name, tables) {
            table <- tables[[table.name]]
            browser()
            tabPanel(
              title = table.name,
              value = table.name,
              
              do.call(
                shinyBS::bsCollapse,
                args = c(
                  id = session$ns(table.name),
                  multiple = FALSE,
                  lapply(
                    seq(nrow(table)),
                    id = session$ns(table.name),
                    table.name = table.name,
                    table = table,
                    # row UI - collapsePanel
                    function(row.id, id, table.name, table) {
                      row = table[row.id,]
                      
                      shinyBS::bsCollapsePanel(
                        title = row$attributeName,
                        value = row$attributeName,
                        ... = fluidRow(
                          column(
                            9,
                            lapply(
                              names(row),
                              id = NS(id, row$attributeName),
                              table.name = table.name,
                              table = table,
                              row.id = row.id,
                              # col UI - single Input
                              function(col.id, row.id, id, table.name, table) {
                                value = table[row.id, col.id]
                                
                                switch(
                                  col.id,
                                  # attributeDefinition ----
                                  attributeDefinition = textAreaInput(
                                    NS(id, col.id),
                                    value = value,
                                    "Describe the attribute"
                                  ),
                                  # class ----
                                  class = selectInput(
                                    NS(id, col.id),
                                    "Dectected class (change if misdetected)",
                                    choices = c("numeric", "character", "Date", "categorical"),
                                    selected = value
                                  ),
                                  # unit ----
                                  unit = {
                                    tmp <- selectInput(
                                      NS(id, col.id),
                                      "Select an unit",
                                      choices = setUnitList(main.env),
                                      selected = if(isTruthy(value) && 
                                                    !grepl("!.*!", value) &&
                                                    value != "custom")
                                        value
                                    )
                                    if (isTruthy(value)) {
                                      tmp
                                    } else {
                                      shinyjs::hidden(tmp)
                                    }
                                  },
                                  # dateTimeFormatString ----
                                  dateTimeFormatString = {
                                    tmp <- selectInput( # TODO better hour format
                                      NS(id, col.id),
                                      "Select a date format",
                                      unique(c(value, "DD-MM-YYYY")),
                                      selected = if (isTruthy(value) && !grepl("!Ad.*ere!", value)) value
                                    )
                                    if (isTruthy(value)) {
                                      tmp
                                    } else {
                                      shinyjs::hidden(tmp)
                                    }
                                  },
                                  # missingValueCode ----
                                  missingValueCode = textInput(
                                    NS(id, col.id),
                                    "Code for missing value (max 1 word)",
                                    value = value
                                  ),
                                  # missingValueCodeExplanation ----
                                  missingValueCodeExplanation = textAreaInput(
                                    NS(id, col.id),
                                    "Explain Missing Values",
                                    value = value
                                  ),
                                  NULL
                                )
                              }
                            )
                          )
                        )
                      )
                    }
                  )
                )
              )
            )
          }
        )
      )
    )
  })
}



shinyApp(ui, server)