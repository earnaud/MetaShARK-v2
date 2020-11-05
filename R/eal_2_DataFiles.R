#' @param id character. An ID string that corresponds with the ID used to call 
#' the module's UI function.
#' @param main.env environment. An internal environment for MetaShARK save 
#' variables.
#' 
#' @import shiny
#' @importFrom shinyFiles shinyFilesButton
#'
#' @noRd
DataFilesUI <- function(id, main.env) {
  return(
    fluidPage(
      tags$b("Disclaimers:"),
      tags$ul(
        tags$li("Until now, only table files are supported."),
        tags$li("Selecting a file will immediately upload it (heavy files might 
                be slow)."),
        class = "disclaimer"
      ),
      fluidRow(
        column(6,
          div(
            fileInput(
              NS(id, "add_data_files"),
              "Select data file(s) from your dataset",
              buttonLabel = span("Load files", icon("plus-circle")),
              multiple = TRUE,
              width = "100%"
            ),
            style = "display: inline-block; vertical-align: top;"
          )
        ),
        column(6,
          if(main.env$wip){
            wipRow(
              URL_Input_UI(
                NS(id, "url_files"),
                "Select data file(s) from an URL"
              ),
              actionButton(
                NS(id,"add_url_files"),
                label = "Get",
                icon = icon("download")
              )
            )
          }
        )
      ),
      tags$div(id = "inserthere_eal2")
    ) # end fluidPage
  ) # end return
}

#' @import shiny
#' @importFrom fs is_dir
#' @importFrom gdata humanReadable
#'
#' @noRd
DataFiles <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Variable initialization (deprecated)
    
    # Setup UI on load
    observeEvent(main.env$EAL$page, { # on load
      req(main.env$EAL$old.page %in% c(0,1) && 
            main.env$EAL$page == 2)
      
      if (isContentTruthy(main.env$local.rv$data.files) && 
          nrow(main.env$local.rv$data.files) > 0) {
        sapply(seq(nrow(main.env$local.rv$data.files)), function(row.id) {
          insertDataFileInput(
            session$ns(sprintf("_%s", row.id)),
            main.env
          )
        })
      }
    }, priority = -1)
    
    # Add data files ====
    observeEvent(input$add_data_files,
      {
        # validity checks
        req(isContentTruthy(input$add_data_files))
        
            # * retrieve files info ----
            .loaded.files <- input$add_data_files
            
            # remove spaces
            file.rename(
              .loaded.files$datapath,
              gsub(" ", "_",.loaded.files$datapath)
            )
            .loaded.files$name <- gsub(" ", "_", .loaded.files$name)
            .loaded.files$datapath <- gsub(" ", "_", .loaded.files$datapath)
            # add URL, description and table name columns
            .loaded.files$url <- rep("", dim(.loaded.files)[1])
            .loaded.files$description <- rep("", dim(.loaded.files)[1])
            .loaded.files$table.name <- rep("", dim(.loaded.files)[1])
            
            # * bind into local.rv ----
            # empty local.rv
            if (isFALSE(
              isContentTruthy(main.env$local.rv$data.files) && 
              all(dim(main.env$local.rv$data.files) > 0)
            )) {
              # Bind data
              main.env$local.rv$data.files <- cbind(
                id = seq(nrow(.loaded.files)),
                .loaded.files
              )
              # Add UI
              sapply(
                seq(nrow(.loaded.files)),
                function(ind){
                  # Render
                  insertDataFileInput(
                    session$ns(main.env$local.rv$counter),
                    main.env
                  )
                  # Increase file counter
                  main.env$local.rv$counter <- main.env$local.rv$counter+1
                }
              )
              # Copy new files to the server
              destination <- sprintf(
                "%s/%s",
                main.env$PATHS$eal.tmp,
                main.env$local.rv$data.files$name
              )
              file.copy(main.env$local.rv$data.files$datapath, destination)
              main.env$local.rv$data.files$datapath <- destination
              
            } else {
              # non-empty local.rv
              sapply(.loaded.files$name, function(filename) {
                filepath <- .loaded.files$datapath[.loaded.files$name == filename]
                if (fs::is_dir(filepath)) {
                  showNotification(
                    paste(filename, "is a folder."),
                    type = "warning"
                  )
                } else {
                  if (!filename %in% main.env$local.rv$data.files$name) {
                    # Bind data
                    main.env$local.rv$data.files <- unique(rbind(
                      main.env$local.rv$data.files,
                      cbind(
                        id = main.env$local.rv$counter,
                        .loaded.files[.loaded.files$name == filename, ]
                      )
                    ))
                    # Render
                    insertDataFileInput(
                      session$ns(main.env$local.rv$counter),
                      main.env
                    )
                    # Increase file counter
                    main.env$local.rv$counter <- main.env$local.rv$counter + 1
                  }
                }
              })
            }
            
            # * copy to the server ----
            destination <- sprintf(
              "%s/%s",
              main.env$PATHS$eal.tmp,
              main.env$local.rv$data.files$name
            )
            file.copy(main.env$local.rv$data.files$datapath, destination)
            main.env$local.rv$data.files$datapath <- destination
      },
      ignoreInit = TRUE,
      label = "EAL2: add files"
    )

    # Data size ----
    observeEvent(main.env$local.rv$data.files, {
      req(isContentTruthy(main.env$local.rv$data.files))
      files.size <- if (isContentTruthy(main.env$local.rv$data.files$size)) {
        sum(main.env$local.rv$data.files$size)
      } else {
        0
      }
      files.size.max <- main.env$VALUES$thresholds$files.size.max

      style <- if (files.size < 0.9 * files.size.max) {
        "color: green;"
      } else if (files.size >= 0.9 * files.size.max && files.size < files.size.max) {
        "color: gold;"
      } else {
        "color: red"
      }

      main.env$EAL$tag.list <- tagList(
        "Files size:",
        tags$p(
          gdata::humanReadable(files.size),
          if (files.size >= files.size.max) {
            paste("Max. recommended:", gdata::humanReadable(files.size.max))
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
      req(main.env$EAL$page == 2)
      invalidateLater(1000)
      main.env$EAL$completed <- isContentTruthy(main.env$local.rv$data.files) &&
        all(dim(main.env$local.rv$data.files) > 0)
    },
    label = "EAL2: set completed"
    )

    # Process data (deprecated)
  })
}