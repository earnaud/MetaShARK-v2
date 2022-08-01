#' @import shiny
#'
#' @noRd
DataFilesUI <- function(id) {
  ns <- NS(id)

  return(
    fluidPage(
      tags$div(
        id = ns("disclaimer"),
        class = "disclaimer",
        tags$h4("Disclaimer"),
        tags$p("(click to dismiss)"),
        "Until now, only table files are supported."
      ),
      fluidRow(
        column(
          12, #6,
          div(
            fileInput(
              ns("add_data_files"),
              "Select data file(s) from your dataset",
              buttonLabel = span("Load files", icon("plus-circle")),
              multiple = TRUE,
              width = "100%"
            ),
            style = "display: inline-block; vertical-align: top;"
          )
        )#,
        # column(
        #   6,
        #   shinyjs::hidden(
        #     tags$div(
        #       id = "url_div",
        #       wipRow(
        #         urlInput_UI(
        #           ns("url_files"),
        #           "Select data file(s) from an URL"
        #         ),
        #         actionButton(
        #           ns("add_url_files"),
        #           label = "Get",
        #           icon = icon("download")
        #         )
        #       )
        #     )
        #   )
        # )
      ),
      tags$div(id = "inserthere_eal2")
    ) # end fluidPage
  ) # end return
}

#' @import shiny
#' @importFrom fs is_dir
#' @importFrom gdata humanReadable
DataFiles <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    if (main_env$dev) .browse_dev(main_env, 2, input, output, session)
    # remote data
    # shinyjs::show("url_div")

    # Setup UI on load =====
    # observeEvent(main_env$EAL$page,
    #   { # on load
    #     req(main_env$EAL$page == 2)
    #
    #     if (isContentTruthy(main_env$local_rv$data_files) &&
    #       nrow(main_env$local_rv$data_files) > 0)
    #       sapply(seq_row(main_env$local_rv$data_files), function(row.id) {
    #         insertDataFileInput(
    #           session$ns(sprintf("_%s", as.character(row.id))),
    #           main_env
    #         )
    #       })
    #   },
    #   priority = -1,
    #   label = "EAL2: setup UI"
    # )

    # Remove disclaimer ----
    shinyjs::onclick(session$ns("disclaimer"), {
      req(main_env$EAL$page == 2)
      # animate cause it looks nice
      shinyjs::hide("disclaimer", anim = TRUE, time = 0.25)
    }, asis = TRUE)

    # Add data files ====
    observeEvent(input$add_data_files, {
        # validity checks
        req(isContentTruthy(input$add_data_files))

        ## retrieve files info ----
        .loaded_files <- input$add_data_files

        # FIXME clean file input

        # remove spaces
        file.rename(
          .loaded_files$datapath,
          gsub(" ", "_", .loaded_files$datapath)
        )
        .loaded_files$name <- gsub(" ", "_", .loaded_files$name)
        .loaded_files$datapath <- gsub(" ", "_", .loaded_files$datapath)
        # add URL, description and table_name columns
        .loaded_files$url <- rep("", dim(.loaded_files)[1])
        .loaded_files$description <- rep("", dim(.loaded_files)[1])
        .loaded_files$table_name <- rep("", dim(.loaded_files)[1])

        # Check for folders
        .to_remove <- c()
        sapply(seq_row(.loaded_files), function(.ind) {
          .loaded_file <- .loaded_files[.ind, ]
          filepath <- .loaded_file$datapath
          if (fs::is_dir(filepath)) {
            showNotification(
              paste(filename, "is a folder."),
              type = "warning"
            )
            .to_remove <<- c(.to_remove, .ind)
          }
        })
        if (length(.to_remove) > 0) {
          .loaded_files <- .loaded_files[-.to_remove, ]
        }

        # Do not go further if no more files left
        req(length(.loaded_files) > 0)

        ## bind into local_rv ----
        ### empty local_rv ----
        if (isFALSE(
          isContentTruthy(main_env$local_rv$data_files) &&
            all(dim(main_env$local_rv$data_files) > 0)
        )) {
          # Bind data
          main_env$local_rv$data_files <- cbind(
            id = main_env$local_rv$counter + seq_row(.loaded_files) - 1,
            .loaded_files
          )
          # Add UI
          sapply(
            seq_row(.loaded_files),
            function(ind) {
              # Render
              insertModule(
                "Data_Files",
                list(
                  ui = session$ns(main_env$local_rv$counter),
                  server = as.character(main_env$local_rv$counter)
                ),
                main_env
              )
              # insertDataFileInput(
              #   session$ns(main_env$local_rv$counter),
              #   main_env
              # )
              # Increase file counter
              main_env$local_rv$counter <- main_env$local_rv$counter + 1
            }
          )
          # Copy new files to the server
          destination <- sprintf(
            "%s/%s",
            main_env$PATHS$eal_tmp,
            main_env$local_rv$data_files$name
          )
          file.copy(main_env$local_rv$data_files$datapath, destination)
          main_env$local_rv$data_files$datapath <- destination
        } else {
          ### non-empty local_rv ----
          sapply(seq_along(.loaded_files$name), function(.row) {
            filename <- .loaded_files$name[.row]
            
            # Add entry in variable
            if (!filename %in% main_env$local_rv$data_files$name) {
              if (main_env$dev) devmsg("uploaded %s", filename)
              # Bind data
              main_env$local_rv$data_files <- unique(rbind(
                main_env$local_rv$data_files,
                cbind(
                  id = main_env$local_rv$counter,
                  .loaded_files[.row, ]
                )
              ))
              # Render
              insertModule(
                "Data_Files",
                list(
                  ui = session$ns(main_env$local_rv$counter),
                  server = as.character(main_env$local_rv$counter)
                ),
                main_env
              )
              # Increase file counter
              main_env$local_rv$counter <- main_env$local_rv$counter + 1
            } else {
              if (main_env$dev) devmsg("updated %s", filename)
              .row2 <- which(main_env$local_rv$data_files$name == filename)

              # replace
              main_env$local_rv$data_files[.row2, -1] <- .loaded_files[.row, ]
              # update UI
              if (!isTruthy(main_env$local_rv$data_files$description[.row2])) {
                main_env$local_rv$data_files$description[.row2] <- sprintf(
                  "Content of %s", main_env$local_rv$data_files$name[.row2]
                )
              }
              updateTextAreaInput(
                session = session,
                paste0(.row2, "-data_description"),
                value = main_env$local_rv$data_files$description[.row2]
              )
            }
          })
        }

        ## copy to the server ----
        destination <- sprintf(
          "%s/%s",
          main_env$PATHS$eal_tmp,
          main_env$local_rv$data_files$name
        )
        file.copy(main_env$local_rv$data_files$datapath, destination)
        main_env$local_rv$data_files$datapath <- destination
      },
      ignoreInit = TRUE,
      label = "EAL2: add files"
    )

    # Data size ----
    observeEvent(main_env$local_rv$data_files, {
        main_env$EAL$tag_list <- tagList()
        req(isContentTruthy(main_env$local_rv$data_files))

        files_size <- if (isContentTruthy(main_env$local_rv$data_files$size)) {
          sum(main_env$local_rv$data_files$size)
        } else {
          0
        }
        files_size_max <- main_env$VALUES$thresholds$files_size_max

        style <- if (files_size < 0.9 * files_size_max) {
          "color: green;"
        } else if (files_size >= 0.9 * files_size_max &&
          files_size < files_size_max) {
          "color: gold;"
        } else {
          "color: red;"
        }

        main_env$EAL$tag_list <- tagList(
          "Files size:",
          tags$p(
            gdata::humanReadable(files_size),
            if (files_size >= files_size_max) {
              paste("Max. recommended:", gdata::humanReadable(files_size_max))
            } else {
              NULL
            },
            style = style
          )
        )
      },
      label = "EAL2: data files size"
    )

    # Saves ----
    observe({
      req(main_env$EAL$page == 2)

      main_env$EAL$completed <- isContentTruthy(main_env$local_rv$data_files) &&
        all(dim(main_env$local_rv$data_files) > 0)
    },
    priority = -1,
    label = "EAL2: set completed"
    )

    # Remove inserted UI ----
    observeEvent(main_env$EAL$page, {
        req(main_env$EAL$old_page == 2) # left the DataFiles step

        sapply(
          paste0(main_env$local_rv$data_files$id, "-container"),
          function(id) {
            removeUI(sprintf("#%s", session$ns(id)), immediate = TRUE)
          }
        )
      },
      priority = 1
    )
  })
}
