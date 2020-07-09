#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @import shiny
CatVarsUI <- function(id, title, dev) {
  ns <- NS(id)

  return(
    fluidPage(
      fluidRow(
        # Navigation
        fluidRow(
          column(
            1,
            actionButton(
              ns("file_prev"),
              "",
              icon("chevron-left")
            )
          ),
          column(
            10,
            uiOutput(
              ns("current_file"),
              inline = TRUE
            )
          ),
          column(
            1,
            actionButton(
              ns("file_next"),
              "",
              icon("chevron-right")
            )
          ),
          style = "padding: 5px;"
        ),
        # content form
        uiOutput(ns("edit_catvar"))
      )
    ) # end of fluidPage
  ) # end of return
}

#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @import shiny
#' @importFrom dplyr %>% filter select mutate
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom shinyjs onclick
CatVars <- function(input, output, session,
                    savevar, main.env, NSB) {
  ns <- session$ns
  
  if (main.env$DEV) {
shinyjs::onclick("dev",
      {
        req(main.env$EAL$navigate == 4)
        browser()
      },
      asis = TRUE
    )
  }

  # variables initialization -----------------------------------------------------
  rv <- reactiveValues(
    catvar.files = list.files(
      savevar$emlal$SelectDP$dp.metadata.path,
      pattern = "catvar",
      full.names = TRUE
    ),
    current.index = 1
  )

  # update current file
  observeEvent(rv$current.index,
    {
      rv$current.file <- basename(rv$catvar.files[rv$current.index])
    },
    priority = 1,
    ignoreInit = FALSE
  )

  # Set each reactivevalues per file
  sapply(rv$catvar.files, function(file.path) {
    file.name <- basename(file.path)
    rv[[file.name]] <- reactiveValues()

    # * Init data frame ====
    rv[[file.name]]$CatVars <- fread(
      file.path,
      data.table = FALSE, stringsAsFactors = FALSE,
      na.strings = "NA"
    ) %>%
      dplyr::mutate(
        definition = if (definition == "NA" || is.na(definition)) {
          paste("Value:", code, "for attribute:", attributeName)
        } else {
          definition
        }
      )

    # * Write UI ====
    .content <- lapply(
      unique(rv[[file.name]]$CatVars$attributeName), 
      function(attribute) {
        # get codes aka values for `attribute` in catvar_*.txt
        codes <- rv[[file.name]]$CatVars %>%
          dplyr::filter(attributeName == attribute) %>%
          dplyr::select(code)
        
        bsCollapsePanel(
          title = attribute,
          # value = attribute,
          ... = tagList(
            lapply(unlist(codes), function(cod) {
              if(is.na(cod))
                cod <- "NA"
              
              input.id <- paste(
                attribute,
                cod %>%
                  gsub("[ [:punct:]]", "", .),
                sep = "-"
              )
              
              textAreaInput(
                ns(input.id),
                cod,
                value = rv[[file.name]]$CatVars %>%
                  dplyr::filter(attributeName == attribute, code == cod) %>%
                  dplyr::select(definition) %>% 
                  unique
              )
            })
          ) # end of "tagapply" -- text areas
        ) # end of bsCollapsePanel
      }
    ) # end of "tagapply" -- collapse boxes
    
    rv[[file.name]]$UI <- do.call(
      bsCollapse,
      c(
        .content,
        id = file.name,
        multiple = FALSE
      )
    )

    # * Write server ====
    rv[[file.name]]$obs <- sapply(
      seq(dim(rv[[file.name]]$CatVars)[1]), 
      function(row) {
        input.id <- paste(
          rv[[file.name]]$CatVars$attributeName[row],
          rv[[file.name]]$CatVars$code[row] %>%
            gsub("[ [:punct:]]", "", .),
          sep = "-"
        )
        
        return(
          observeEvent(input[[input.id]],
            {
              req(input[[input.id]])
              rv[[file.name]]$CatVars[row, "definition"] <- input[[input.id]]
            },
            suspended = TRUE
          )
        )
      }
    )
  })

  # Navigation buttons -----------------------------------------------------
  # Previous file
  observeEvent(input$file_prev, {
    req(rv$current.index, rv$catvar.files)
    savevar$emlal$CatVars[[rv$current.file]] <- rv[[rv$current.file]]$CatVars
    if (rv$current.index > 1) {
      rv$current.index <- rv$current.index - 1
    }
  })

  # Next file
  observeEvent(input$file_next, {
    req(rv$current.index, rv$catvar.files)
    savevar$emlal$CatVars[[rv$current.file]] <- rv[[rv$current.file]]$CatVars
    if (rv$current.index < length(rv$catvar.files)) {
      rv$current.index <- rv$current.index + 1
    }
  })

  # Current file
  output$current_file <- renderUI({
    tags$div(
      rv$current.file,
      class = "ellipsis",
      style = paste0(
        "display: inline-block;
        font-size:14pt;
        text-align:center;
        width:100%;
        background: linear-gradient(90deg, #3c8dbc ",
        round(100 * rv$current.index / length(rv$catvar.files)),
        "%, white ",
        round(100 * rv$current.index / length(rv$catvar.files)),
        "%);"
      )
    )
  })

  # Set UI -----------------------------------------------------
  output$edit_catvar <- renderUI({
    validate(
      need(rv[[rv$current.file]]$UI, "No UI set.")
    )
    rv[[rv$current.file]]$UI
  }) # end of renderUI

  # Set Server -----------------------------------------------------

  # Suspend observers
  observeEvent(rv$current.index,
    {
      req(rv$current.file)
      sapply(rv[[rv$current.file]]$obs, function(obs) {
        obs$suspend()
      })
    },
    priority = 2
  )

  # Run observers
  observeEvent(rv$current.file,
    {
      req(rv$current.file)
      sapply(rv[[rv$current.file]]$obs, function(obs) {
        obs$resume()
      })
    },
    priority = 0
  )

  # Saves -----------------------------------------------------
  observe({
    main.env$EAL$current[2] <- all(
      sapply(basename(rv$catvar.files), function(file.name) {
        all(sapply(rv[[file.name]]$CatVars$definition, isTruthy))
      })
    )
  })

  observeEvent(NSB$SAVE, {
    req(tail(main.env$EAL$history, 1) == "Categorical Variables")

    savevar <- saveReactive(
      savevar,
      rv = list(CatVars = rv)
    )
  })

  # Process data -----------------------------------------------------
  observeEvent(NSB$NEXT,
    {
      req(main.env$EAL$current[2] == "Categorical Variables")

      savevar <- saveReactive(
        savevar,
        rv = list(CatVars = rv)
      )
    },
    priority = 1,
    ignoreInit = TRUE
  )

  # Output -----------------------------------------------------
  return(savevar)
}
