#' @import shiny
CatVarsUI <- function(id, main.env) {
  ns <- NS(id)

  return(
    fluidPage(
      fluidRow(
        # Navigation
        fluidRow(
          column(
            1,
            actionButton(
              NS(id, "file_prev"),
              "",
              icon("chevron-left")
            )
          ),
          column(
            10,
            uiOutput(
              NS(id, "current_file"),
              inline = TRUE
            )
          ),
          column(
            1,
            actionButton(
              NS(id, "file_next"),
              "",
              icon("chevron-right")
            )
          ),
          style = "padding: 5px;"
        ),
        # content form
        uiOutput(NS(id, "edit_catvar"))
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom dplyr %>% filter select mutate
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom shinyjs onclick
#'
#' @noRd
CatVars <- function(id, full.id, main.env) {
  moduleServer(id, function(input, output, session) {
    # variables initialization ----
    observe({
      req(main.env$EAL$page == 4)
      main.env$EAL$page.load$depend()
      
      # read metadata folder path
      .md.path <- isolate(main.env$save.variable$SelectDP$dp.metadata.path)
      req(isContentTruthy(.md.path))
      main.env$local.rv$catvar.files <- list.files(
        .md.path,
        pattern = "catvar",
        full.names = TRUE
      )
      # update current file
      req(isContentTruthy(main.env$local.rv$catvar.files))
      main.env$local.rv$current.index <- as.numeric(isContentTruthy(main.env$local.rv$catvar.files))
      main.env$local.rv$current.file <- basename(main.env$local.rv$catvar.files[main.env$local.rv$current.index])
    },
    label = "EAL4: set local var"
    )

    # Set each reactivevalues per file
    observeEvent(main.env$local.rv$catvar.files, {
      req(main.env$EAL$page == 4)
      req(isTruthy(main.env$local.rv$catvar.files))

      sapply(main.env$local.rv$catvar.files, function(file.path) {
        file.name <- basename(file.path)
        main.env$local.rv[[file.name]] <- reactiveValues()

        # * Init data frame ====
        main.env$local.rv[[file.name]]$CatVars <- fread(
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
          unique(main.env$local.rv[[file.name]]$CatVars$attributeName),
          function(attribute) {
            # get codes aka values for `attribute` in catvar_*.txt
            codes <- main.env$local.rv[[file.name]]$CatVars %>%
              dplyr::filter(attributeName == attribute) %>%
              dplyr::select(code)

            shinyBS::bsCollapsePanel(
              title = attribute,
              # value = attribute,
              ... = tagList(
                lapply(unlist(codes), function(cod) {
                  if (is.na(cod)) {
                    cod <- "NA"
                  }

                  input.id <- paste(
                    attribute,
                    cod %>%
                      gsub("[ [:punct:]]", "", .),
                    sep = "-"
                  )

                  textAreaInput(
                    NS(id, input.id),
                    cod,
                    value = main.env$local.rv[[file.name]]$CatVars %>%
                      dplyr::filter(attributeName == attribute, code == cod) %>%
                      dplyr::select(definition) %>%
                      unique()
                  )
                })
              ) # end of "tagapply" -- text areas
            ) # end of bsCollapsePanel
          }
        ) # end of "tagapply" -- collapse boxes

        main.env$local.rv[[file.name]]$UI <- do.call(
          shinyBS::bsCollapse,
          c(
            .content,
            id = file.name,
            multiple = FALSE
          )
        )

        # * Write server ====
        main.env$local.rv[[file.name]]$obs <- sapply(
          seq(dim(main.env$local.rv[[file.name]]$CatVars)[1]),
          function(row) {
            input.id <- paste(
              main.env$local.rv[[file.name]]$CatVars$attributeName[row],
              main.env$local.rv[[file.name]]$CatVars$code[row] %>%
                gsub("[ [:punct:]]", "", .),
              sep = "-"
            )

            return(
              observeEvent(input[[input.id]],
                {
                  req(input[[input.id]])
                  main.env$local.rv[[file.name]]$CatVars[row, "definition"] <- input[[input.id]]
                },
                suspended = TRUE
              )
            )
          }
        )
      })
    }, 
    label = "EAL4: set rv per file"
    )

    # Navigation buttons ----
    # Previous file
    observeEvent(input$file_prev, {
      req(main.env$EAL$page == 4)
      req(main.env$local.rv$current.index, main.env$local.rv$catvar.files)
      main.env$save.variable$CatVars[[main.env$local.rv$current.file]] <- main.env$local.rv[[main.env$local.rv$current.file]]$CatVars
      if (main.env$local.rv$current.index > 1) {
        main.env$local.rv$current.index <- main.env$local.rv$current.index - 1
      }
    },
    label = "EAL4: previous file"
    )

    # Next file
    observeEvent(input$file_next, {
      req(main.env$EAL$page == 4)
      req(main.env$local.rv$current.index, main.env$local.rv$catvar.files)
      main.env$save.variable$CatVars[[main.env$local.rv$current.file]] <- main.env$local.rv[[main.env$local.rv$current.file]]$CatVars
      if (main.env$local.rv$current.index < length(main.env$local.rv$catvar.files)) {
        main.env$local.rv$current.index <- main.env$local.rv$current.index + 1
      }
    },
    label = "EAL4: next file"
    )

    # Current file
    output$current_file <- renderUI({
      req(main.env$EAL$page == 4)
      tags$div(
        main.env$local.rv$current.file,
        class = "ellipsis",
        style = paste0(
          "display: inline-block;
        font-size:14pt;
        text-align:center;
        width:100%;
        background: linear-gradient(90deg, #3c8dbc ",
          round(100 * main.env$local.rv$current.index / length(main.env$local.rv$catvar.files)),
          "%, white ",
          round(100 * main.env$local.rv$current.index / length(main.env$local.rv$catvar.files)),
          "%);"
        )
      )
    })

    # Set UI ----
    output$edit_catvar <- renderUI({
      req(main.env$EAL$page == 4)
      validate(
        need(main.env$local.rv[[main.env$local.rv$current.file]]$UI, "No UI set.")
      )
      main.env$local.rv[[main.env$local.rv$current.file]]$UI
    }) # end of renderUI

    # Set Server ----
    # Suspend observers
    observeEvent(main.env$local.rv$current.index, {
      req(main.env$EAL$page == 4)
      req(main.env$local.rv$current.file)
      sapply(main.env$local.rv[[main.env$local.rv$current.file]]$obs, function(obs) {
        obs$suspend()
      })
    },
      label = "EAL4: suspend observers",
      priority = 2
    )

    # Run observers
    observeEvent(main.env$local.rv$current.file, {
      req(main.env$EAL$page == 4)
      req(main.env$local.rv$current.file)
      sapply(main.env$local.rv[[main.env$local.rv$current.file]]$obs, function(obs) {
        obs$resume()
      })
    },
    label = "EAL4: run observers",
    priority = 0
    )

    # Saves ----
    observe({
      req(main.env$EAL$page == 4)
      req(main.env$local.rv$catvar.files)
      
      main.env$EAL$completed <- all(
        sapply(basename(main.env$local.rv$catvar.files), function(file.name) {
          all(sapply(main.env$local.rv[[file.name]]$CatVars$definition, isContentTruthy))
        })
      )
    },
    label = "EAL4: continuous save"
    )

    # Process data ----
    observeEvent(main.env$EAL$.next, {
      req(main.env$EAL$page == 4)
      
      saveReactive(main.env)
    },
    label = "EAL4: process data",
    priority = 1,
    ignoreInit = TRUE
    )
  })
}
