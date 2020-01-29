#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @importFrom shiny NS fluidPage column fluidRow actionButton tags tagList
CatVarsUI <- function(id, title, dev) {
  ns <- NS(id)

  return(
    fluidPage(
      # Features UI ----
      h4(title),
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
CatVars <- function(input, output, session, savevar, globals) {
  ns <- session$ns

  # DEV ----
  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }

  # Initialization ----
  rv <- reactiveValues(
    catvarFiles = c(),
    currentIndex = 0,
    CatVars = reactiveValues(),
    codes = reactiveValues()
  )

  observeEvent(TRUE,
    {
      # list catvar files
      rv$catvarFiles$full <- list.files(
        paste(
          savevar$emlal$SelectDP$dp_path,
          savevar$emlal$SelectDP$dp_name,
          "metadata_templates",
          sep = "/"
        ),
        "catvar",
        full.names = TRUE
      )
      rv$catvarFiles$short <- list.files(
        paste(
          savevar$emlal$SelectDP$dp_path,
          savevar$emlal$SelectDP$dp_name,
          "metadata_templates",
          sep = "/"
        ),
        "catvar"
      )
      rv$currentIndex <- 1
      sapply(rv$catvarFiles$full, function(file_name) {
        savevar$emlal$CatVars[[basename(file_name)]] <- fread(file_name)
        if (all(is.na(savevar$emlal$CatVars[[basename(file_name)]]$definition))) {
          savevar$emlal$CatVars[[basename(file_name)]] <- savevar$emlal$CatVars[[basename(file_name)]] %>%
            mutate(definition = replace(.$definition, TRUE, paste("Value:", .$code, "for attribute:", .$attributeName)))
        }
      })
    },
    once = TRUE
  )

  observeEvent(rv$currentIndex, {
    # update rv$CatVars
    rv$CatVars <- fread(rv$catvarFiles$full[rv$currentIndex])
    if (all(is.na(rv$CatVars$definition))) {
      rv$CatVars <- savevar$emlal$CatVars[[ rv$catvarFiles$short[rv$currentIndex] ]]
    }
  }) # end of observeEvent

  # Navigation buttons ----
  # ** files
  observeEvent(input$file_prev,
    {
      req(rv$currentIndex)
      if (rv$currentIndex > 1) {
        rv$currentIndex <- rv$currentIndex - 1
      }
    },
    priority = -1
  )
  observeEvent(input$file_next,
    {
      req(rv$currentIndex, rv$catvarFiles$short)
      if (rv$currentIndex < length(rv$catvarFiles$short)) {
        rv$currentIndex <- rv$currentIndex + 1
      }
    },
    priority = -1
  )
  output$current_file <- renderUI({
    tags$div(
      rv$catvarFiles$short[rv$currentIndex],
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
    )
  })

  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar,
    savevar$emlal$SelectDP$dp_path,
    savevar$emlal$SelectDP$dp_name
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar,
    savevar$emlal$SelectDP$dp_path,
    savevar$emlal$SelectDP$dp_name
  )
  callModule(
    nextTab, "nav",
    globals, "CatVars"
  )
  callModule(
    prevTab, "nav",
    globals
  )

  # Procedurals ----
  # /UI ----
  output$edit_catvar <- renderUI({
    req(rv$CatVars)
    CatVars <- isolate(rv$CatVars)
    do.call(
      bsCollapse,
      c(
        id = rv$catvarFiles$short[rv$currentIndex],
        ... = lapply(unique(CatVars$attributeName), function(attribute) {
          # get code for each attribute
          codes <- CatVars %>%
            dplyr::filter(attributeName == attribute) %>%
            dplyr::select(code)
          # collapse box
          shinyBS::bsCollapsePanel(
            title = attribute,
            value = attribute,
            tagList(
              lapply(sapply(codes, as.list), function(cod) {
                # input for each code
                textAreaInput(ns(cod), cod, value = CatVars %>% filter(attributeName == attribute, code == cod) %>% select(definition))
              })
            ) # end of "tagapply" -- text areas
          ) # end of bsCollapsePanel
        }) # end of "tagapply" -- collapse boxes
      ) # end of args
    ) # end of do.call
  }) # end of renderUI

  # /Server ----
  observeEvent(rv$currentIndex, {
    # req(any(names(rv$codes)) %in% names(input))
    # validate(
    #   need(all(rv$CatVars$code %in% names(input)), "codes' inputs are not ready yet")
    # )
    sapply(rv$CatVars$code, function(cod) {
      rv$codes[[cod]] <- eventReactive(input[[cod]],
        {
          # get input value
          enter <- input[[cod]]

          # check obtained value
          if (is.list(enter)) {
            enter <- unlist(enter)
          }
          return(enter)
        },
        ignoreNULL = TRUE
      ) # end eventReactive
    }) # end sapply
  }) # end observeEvent

  # Saves ----
  observeEvent(sapply(names(rv$codes), function(i) input[[i]]),
    {
      validate(
        need(all(rv$CatVars$code %in% names(input)), "codes' inputs are not ready yet"),
        need(all(rv$CatVars$code %in% names(rv$codes)), "codes' inputs are not ready yet")
      )
      rv$CatVars$definition <- printReactiveValues(rv$codes)[rv$CatVars$code]
    }
  )

  # Process data ----
  observeEvent(input[["nav-prevTab"]], {
    if (tail(globals$EMLAL$HISTORY, 1) != "CustomUnits") {
      globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE - 1
    }
  })
  observeEvent(input[["nav-nextTab"]], {
    # write catvar templates
    sapply(rv$catvarFiles$full, function(file_name) {
      fwrite(
        savevar$emlal$CatVars[[basename(file_name)]],
        file_name
      )
    })
  })
  
  # Output ----
  return(savevar)
}
