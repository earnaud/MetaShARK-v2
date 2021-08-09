# @param id character. An ID string that corresponds with the ID used to call 
# the module's UI function.
# @param main.env environment. An internal environment for MetaShARK save 
# variables.
# 
# insertModule(
# session$ns("_1111"),
# "#inserthere_eal2",
# moduleUI = DataFileInputUI2,
# moduleUI.args = list(main.env = main.env),
# module = DataFileInput2,
# module.args = list(main.env = main.env)
# )
#' @import shiny
#' @importFrom shinyFiles shinyFilesButton
#'
#' @noRd
DataFilesUI <- function(id) {
  return(
    fluidPage(
      tags$b("Disclaimers:"),
      tags$ul(
        tags$li("Until now, only table files are supported."),
        tags$li("Selecting a file will immediately upload it (heavy files might 
                be slow)."),
        tags$li("Editing a data file requires removing it and uploading its
                newest version."),
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
        column(
          6,
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
    if (main.env$dev){
      observeEvent(
        main.env$dev.browse(), 
        {
          if (main.env$current.tab() == "fill" &&
              main.env$EAL$page == 2) {
            browser()
          }
        },
        label = "EAL2: dev"
      )
    }
    
    # Setup UI on load
    observeEvent(main.env$EAL$page, { # on load
      req(main.env$EAL$page == 2)
      
      if (isContentTruthy(main.env$local.rv$data.files) && 
          nrow(main.env$local.rv$data.files) > 0) {
        sapply(seq(nrow(main.env$local.rv$data.files)), function(row.id) {
          insertDataFileInput(
            session$ns(sprintf("_%s", as.character(row.id))),
            main.env
          )
        })
      }
    }, 
    priority = -1, 
    label = "EAL2: setup UI"
    )
    
    # Add data files ====
    observeEvent(input$add_data_files, {
      # validity checks
      req(isContentTruthy(input$add_data_files))
      
      # * retrieve files info ----
      .loaded.files <- input$add_data_files
      # FIXME clean file input - buggy
      # shinyjs::reset(input$add_data_files)
      
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
      
      # Check for folders
      .to.remove <- c()
      sapply(seq(nrow(.loaded.files)), function(.ind) {
        .loaded.file <- .loaded.files[.ind,]
        filepath <- .loaded.file$datapath
        if (fs::is_dir(filepath)) {
          showNotification(
            paste(filename, "is a folder."),
            type = "warning"
          )
          .to.remove <<- c(.to.remove, .ind)
        }
      })
      if(length(.to.remove) > 0)
        .loaded.files <- .loaded.files[-.to.remove,]
      
      # Do not go further if no more files left
      req(length(.loaded.files) > 0)
      
      # * bind into local.rv ----
      # empty local.rv
      if (isFALSE(
        isContentTruthy(main.env$local.rv$data.files) && 
        all(dim(main.env$local.rv$data.files) > 0)
      )) {
        # Bind data
        main.env$local.rv$data.files <- cbind(
          id = main.env$local.rv$counter + seq(nrow(.loaded.files)) - 1,
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
          .row <- which(.loaded.files$name == filename)
          filepath <- .loaded.files$datapath[.row]
          
          if (!filename %in% main.env$local.rv$data.files$name) {
            # Bind data
            main.env$local.rv$data.files <- unique(rbind(
              main.env$local.rv$data.files,
              cbind(
                id = main.env$local.rv$counter,
                .loaded.files[.row, ]
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
      main.env$EAL$tag.list <- tagList()
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
        "color: red;"
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
    
    # Remove inserted UI ----
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$old.page == 2) # left the DataFiles step
      
      sapply(paste0(main.env$local.rv$data.files$id, "-container"), function(id) {
        removeUI(sprintf("#%s", session$ns(id)), immediate = TRUE)
      })
    },
    priority = 1
    )
  })
}