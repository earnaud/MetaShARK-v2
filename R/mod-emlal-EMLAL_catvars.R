#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @importFrom shiny NS fluidPage column fluidRow actionButton tags tagList
catvarsUI <- function(id, title, dev) {
  ns <- NS(id)

  return(
    fluidPage(
      # Features UI ----
      column(
        10,
        fluidRow(
          tags$h4("Categorical variables"),
          fluidRow(
            tagList(
              actionButton(ns("file_prev"),
                "",
                icon("chevron-left"),
                width = "12%"
              ),
              uiOutput(ns("current_file"),
                inline = TRUE
              ),
              actionButton(ns("file_next"),
                "",
                icon("chevron-right"),
                width = "12%"
              )
            ),
            style = "padding: 5px;"
          ),
          # content form
          uiOutput(ns("edit_catvar"))
        )
      ), # end of column1
      # Navigation UI ----
      column(
        2,
        navSidebar(ns("nav"),
          ... = tagList(
            if (dev) actionButton(ns("check"), "Dev Check")
          )
        )
      ) # end of column2
    ) # end of fluidPage
  ) # end of return
}

#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @importFrom shiny observeEvent callModule tags tagList reactiveValues renderUI textAreaInput
#' @importFrom dplyr %>% filter select mutate
#' @importFrom shinyBS bsCollapse bsCollapsePanel
catvars <- function(input, output, session, savevar, globals) {
  ns <- session$ns

  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }

  rv <- reactiveValues(
    catvarFiles = c(),
    currentIndex = integer(),
    catvars = reactiveValues()
  )

  # Initialization
  observeEvent(TRUE,
    {
      # list catvar files
      rv$catvarFiles$full <- list.files(
        paste(
          savevar$emlal$selectDP$dp_path,
          savevar$emlal$selectDP$dp_name,
          "metadata_templates",
          sep = "/"
        ),
        "catvar",
        full.names = TRUE
      )
      rv$catvarFiles$short <- list.files(
        paste(
          savevar$emlal$selectDP$dp_path,
          savevar$emlal$selectDP$dp_name,
          "metadata_templates",
          sep = "/"
        ),
        "catvar"
      )
      rv$currentIndex <- 1
    },
    once = TRUE
  )

  observeEvent(rv$currentIndex, {
    rv$catvars <- fread(rv$catvarFiles$full[rv$currentIndex])
  })

  # Navigation buttons ----
  # ** files
  observeEvent(input$file_prev,{
    req(rv$currentIndex)
    if (rv$current_index > 1) {
      rv$current_index <- rv$current_index - 1
    }
  },
  priority = -1
  )
  observeEvent(input$file_next, {
    req(rv$currentIndex, rv$catvarFiles$short)
    if (rv$current_index < length(rv$catvarFiles$short)) {
      rv$current_index <- rv$current_index + 1
    }
  },
  priority = -1
  )
  output$current_file <- renderUI(tags$div(rv$catvarFiles$short[rv$currentIndex],
      style = paste0(
        "display: inline-block;
        font-size:20pt;
        text-align:center;
        width:66%;
        background: linear-gradient(90deg, #3c8dbc ",
        round(100 * rv$currentIndex / length(rv$catvarFiles$short)),
        "%, white ",
        round(100 * rv$currentIndex / length(rv$catvarFiles$short)),
        "%);"
      )
    ))
  
  
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    nextTab, "nav",
    globals, "catvars"
  )
  callModule(
    prevTab, "nav",
    globals
  )

  # Procedurals ----
  # /UI ----
  output$edit_catvar <- renderUI({
    tagList(
      lapply(unique(rv$catvars$attributeName), function(attribute) {
        # get code for each attribute
        codes <- rv$catvars %>%
          dplyr::filter(attributeName == attribute) %>%
          dplyr::select(code)
        # collapse box for each attribute
        shinyBS::bsCollapse(
          id = paste0(
            rv$catvarFiles$short[rv$currentIndex],
            attribute,
            collapse = "-"
          ),
          shinyBS::bsCollapsePanel(
            title = attribute,
            value = attribute,
            tagList(
              lapply(sapply(codes, as.list), function(cod) {
                cat(attribute, "--", cod, "\n")
                # get definition for each code
                saved_value <- rv$catvars %>%
                  filter(
                    attributeName == attribute,
                    code == cod
                  ) %>%
                  select(definition)
                if(is.na(saved_value))
                  saved_value <- paste("Value:", cod, "for attribute:", attribute)
                
                # input for each code
                textAreaInput(ns(cod), cod, value = saved_value)
              }) # end of "tagapply"
            )
          ) # end of bsCollapsePanel
        )
      }) # end of "tagapply"
    )
  }) # end of renderUI

  # /Server ----

  # Process data ----

  observeEvent(input[["nav-prevTab"]], {
    if (tail(globals$EMLAL$HISTORY, 1) != "customUnits") {
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE - 1
    }
  })
  observeEvent(input[["nav-nextTab"]], {

  })

  # Output ----
  return(savevar)
}
