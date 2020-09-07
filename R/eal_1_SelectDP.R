#' @import shiny
#' @importFrom shinyFiles shinyDirButton
#'
#' @noRd
SelectDPUI <- function(id, main.env) {
  # UI output
  return(
    fluidPage(
      title = "Organize data packages",
      fluidRow(
        collapsibleUI(
          NS(id, "usage"),
          "TUTORIAL: EML Assembly Line workflow",
          ... = tagList(
            tags$p(tags$b("Welcome in the EML Assembly line."), "This tool is
              basically designed as a package, embedded in Shiny within MetaShARK.
              This little helper aims to show you what awaits you in the further 
              steps."),
            tags$hr(),
            tags$p("After loading/creating a data package, a navigation bar will 
              appear on your right. There features the following buttons:"),
            tags$ul(
              tags$li(tags$b("Quit: "), "click this to leave the edition of the
                current data package. You will be asked if you wanted to save the
                current changes. You can switch to other section of the app (e.g.
                documentation) without losing the current metadata."),
              tags$li(tags$b("Save: "), "click this to save the current changes 
                in the filled metadata. This will write a save file in the data
                package directory. Then, metadata will only be lost with data 
                package removal."),
              tags$li(tags$b("Next: "), "click this to continue your metadata
                filling. It will bring you to the next step."),
              tags$li(tags$b("Previous:"), "click this to come back to one of
                the previous steps. You can also use the steps", tags$span(
                icon("circle"),
                style = "color: dodgerblue;"
              ), " markers to get
                to the desired step.")
            )
          )
        )
      ),
      # },
      fluidRow(
        # Load existing DP ----
        column(
          6,
          tags$h4("Edit existing data package",
            style = "text-align:center"
          ),
          uiOutput(NS(id, "dp_list"))
        ),
        # Create DP ----
        column(
          6,
          tags$h4("Create new data package",
            style = "text-align:center"
          ),
          checkboxInput(
            NS(id, "quick"),
            tagList(
              tags$b("Quick mode"),
              "Most fields will be automatically filled"
            ),
            value = TRUE
          ),
          # Data package title
          textInput(
            NS(id, "dp_name"),
            "Data package name",
            placeholder = paste0(Sys.Date(), "_project")
          ),
          textInput(
            NS(id, "dp_title"),
            "Dataset title",
            placeholder = "Any title is a title"
          ),
          tags$p(
            "Only use alphanumerics, or one of:",
            HTML(paste(
              tags$code('  '), tags$code('.'), tags$code(','), 
              tags$code(':'), tags$code('_'), tags$code('-'),
              sep = "&nbsp&nbsp"
            ))
          ),
          tags$div(
            id = "license-help",
            selectInput(
              NS(id, "license"),
              "Select an Intellectual Rights License:",
              c("CCBY", "CC0"),
              multiple = FALSE
            )
          ),
          HTML("License: <br>
               <b>CC0:</b> public domain. <br>
               <b>CC-BY-4.0:</b> open source with authorship. <br>
               For more details, visit Creative Commons."),
          # DP creation
          # uiOutput(NS(id, "dp_create"))
          shinyjs::disabled(
            actionButton(NS(id, "dp_create"), "Create")
          )
        ) # end column2
      ) # end fluidRow
    ) # end fluidPage
  ) # end return
}

#' @import shiny
#' @importFrom shinyFiles getVolumes shinyDirChoose parseDirPath
#' @importFrom shinyjs enable disable onclick
#' @importFrom EMLassemblyline template_directories template_core_metadata
#' @importFrom jsonlite read_json unserializeJSON
#'
#' @noRd
SelectDP <- function(id,main.env) {
  moduleServer(id, function(input, output, session) {

    # Help server
    collapsible("usage")

    # variable initialization ----
    observe({
      req(main.env$EAL$page == 1)
      main.env$EAL$page.load$depend()
      
      main.env$local.rv$dp.name <- reactive(input$dp_name)
      main.env$local.rv$dp.title <- reactive(input$dp_title)
      main.env$local.rv$dp.license <- reactive(input$license)
    },
    label = "EAL1: set DP"
    )
    
    # update packages every 10 seconds
    .dp.list <- reactivePoll(
      intervalMillis = 10000,
      session = session,
      checkFunc = function(){
        identical(
          listDP(main.env),
          main.env$local.rv$dp.list
        )
      },
      valueFunc = function(){
        showNotification(
          "Refreshed list of DP"
        )
        listDP(main.env)
      }
    )
    
    observeEvent(.dp.list, {
      req(main.env$EAL$page == 1)
      main.env$local.rv$dp.list <- .dp.list()
    },
    label = "EAL 1: dp list"
    )
    
    # Render DP list ====
    # UI output
    output$dp_list <- renderUI({
        validate(
          need(
            isTruthy(main.env$local.rv$dp.list),
            "No data package has been written."
          )
        )
        tagList(
          radioButtons(
            session$ns("dp_list"),
            NULL,
            choiceNames = c("None selected", main.env$local.rv$dp.list),
            choiceValues = c("", gsub(" \\(.*\\)$", "", main.env$local.rv$dp.list))
          ),
          actionButton(
            session$ns("dp_load"),
            "Load",
            icon = icon("folder-open")
          ),
          actionButton(
            session$ns("dp_delete"),
            "Delete",
            icon = icon("minus-circle"),
            class = "redButton"
          ),
          downloadButton(
            session$ns("dp_download"),
            label = "Download .zip",
            icon = icon("file-download")
          ),
          tags$p(
            "If you have handled manually some packages in",
            isolate(main.env$PATHS$eal.dp),
            ", some packages might not be listed here."
          )
        )
      })

    # Manage DP download ----
    output$dp_download <- downloadHandler(
      filename = function() {
        paste0(input$dp_list, "_emldp.zip")
      },
      content = function(file) {
        .path <- getwd()
        setwd(main.env$PATHS$eal.dp)
        utils::zip(
          zipfile = file,
          files = dir(
            gsub(
              "/+",
              "/",
              dir(
                ".",
                full.names = TRUE,
                pattern = input$dp_list
              )
            ),
            recursive = TRUE,
            full.names = TRUE
          )
        )
        setwd(.path)
      },
      contentType = "application/zip"
    )

    # toggle Load and Delete buttons
    observeEvent(input$dp_list,
      {
        if (input$dp_list != "") {
          shinyjs::enable("dp_load")
          shinyjs::enable("dp_delete")
          shinyjs::enable("dp_download")
        }
        else {
          shinyjs::disable("dp_load")
          shinyjs::disable("dp_delete")
          shinyjs::disable("dp_download")
        }
      },
      label = "EAL1: [UX] dp buttons"
    )

    # DP create ----
    # * Check name ----
    observeEvent(input$dp_name, {
      shinyjs::disable("dp_create") # default
      shinyFeedback::hideFeedback("dp_name")
      
      if(nchar(input$dp_name) <= 3) {
        shinyFeedback::showFeedbackDanger(
          "dp_name",
          "Not enough characters."
        )
      } else if(isFALSE(grepl("^[[:alnum:]_-]+$", input$dp_name))) {
          shinyFeedback::showFeedbackDanger(
            "dp_name",
            "Only use alphanumeric, '_' and '-' characters."
          )
      } else if(input$dp_name %in% main.env$local.rv$dp.list) {
        shinyFeedback::showFeedbackDanger(
          "dp_name",
          "Already used."
        )
      } else {
        shinyFeedback::showFeedbackSuccess("dp_name")
        shinyjs::enable("dp_create")
      }
    })
    
    # * Check title ----
    observeEvent(input$dp_title, {
      shinyjs::disable("dp_create")
      shinyFeedback::hideFeedback("dp_title")
      
      if(nchar(input$dp_title) <= 3) {
        shinyFeedback::showFeedbackDanger(
          "dp_title",
          "Not enough characters."
        )
      } else if(isFALSE(
        grepl("^[[:alnum:]\\ \\.,:_-]+$", input$dp_title)
      )) {
        shinyFeedback::showFeedbackDanger(
          "dp_title",
          "Invalid characters used."
        )
      } else {
        shinyFeedback::showFeedbackSuccess(
          "dp_title"
        )
        shinyjs::enable("dp_create")
      }
    })

    observeEvent(input$quick,
      {
        req(input$dp_name %in% c("", paste0(Sys.Date(), "_project"))) # Do not change a yet changed name
        if (input$quick) {
          updateTextInput(session, "dp_name", value = paste0(Sys.Date(), "_project"))
        } else {
          updateTextInput(session, "dp_name", placeholder = paste0(Sys.Date(), "_project"))
        }
      },
      label = "EAL1: quick"
    )

    # DP management - on clicks ----
    # * Create DP ----
    shinyjs::onclick("dp_create", {
      req(input$dp_create)
      req(main.env$local.rv$dp.name())

      # verbose
      withProgress(
        {
          # save in empty dedicated variable
          main.env$save.variable <- initReactive(
            "emlal", 
            main.env$save.variable,
            main.env
          )
          saveReactive(main.env, main.env$EAL$old.page)
          incProgress(0.2)
          
          dir.create(
            main.env$save.variable$SelectDP$dp.path,
            recursive = TRUE
          )
          incProgress(0.2)

          x <- template(main.env, main.env$EAL$page)
          incProgress(0.6)
        },
        message = paste(
          "Creating:", 
          main.env$local.rv$dp.name(),
          sep = ""
        )
      )
    })

    # * Load DP ----
    shinyjs::onclick("dp_load", {
      req(input$dp_list)
      shinyjs::disable("dp_load")
      
      # variable operation - legibility purpose
      dp <- input$dp_list
      path <- paste0(main.env$PATHS$eal.dp, dp, "_emldp")

      # verbose
      showNotification(
        paste("Loading:", dp),
        type = "message"
      )

      # read variables
      main.env$save.variable <- initReactive("emlal", main.env$save.variable, main.env)

      .tmp <- jsonlite::read_json(paste0(path, "/", dp, ".json"))[[1]] %>%
        jsonlite::unserializeJSON()
      
      # save.variable adaptations
      # TODO remove this later
      
      # - emlal/metafin difference
      if (identical(names(.tmp), c("metafin", "emlal")))
        .tmp <- .tmp$emlal
      
      # - creator
      if(isFALSE("creator" %in% names(.tmp) && isTruthy(.tmp$creator)))
        .tmp$creator <- main.env$SETTINGS$user
      
      # - history
      .tmp$history <- sapply(.tmp$history, function(h) {
        switch(h,
               create = "SelectDP",
               DataFiles = "Data Files",
               attributes = "Attributes",
               CustomUnits = NULL,
               CatVars = "Categorical Variables",
               GeoCov = "Geographic Coverage",
               TaxCov = "Taxonomic Coverage",
               h # unchanged
        )
      }) %>% unname()
      
      # - check quick mode
      .tmp$quick <- isTRUE(.tmp$quick)
      
      # Once prepared, properly merge tmp and save variables
      main.env$save.variable <- setSaveVariable(.tmp, main.env$save.variable)

      # Update paths from another file system
      # * selectDP
      sapply(
        names(main.env$save.variable$SelectDP),
        function(.dp.item) {
          main.env$save.variable$SelectDP[[.dp.item]] <- gsub(
            pattern = "^.*/dataPackagesOutput/emlAssemblyLine/",
            replacement = main.env$PATHS$eal.dp,
            main.env$save.variable$SelectDP[[.dp.item]]
          )
        }
      )

      # * datafiles
      if (isContentTruthy(main.env$save.variable$DataFiles)) {
        sapply(names(main.env$save.variable$DataFiles), function(col) {
          main.env$save.variable$DataFiles[, col] <- gsub(
            pattern = ".*/dataPackagesOutput/emlAssemblyLine/",
            replacement = main.env$PATHS$eal.dp,
            main.env$save.variable$DataFiles[, col]
          )
          if (col == "size") {
            main.env$save.variable$DataFiles[, col] <- as.integer(
              main.env$save.variable$DataFiles[, col]
            )
          }
        })
      }

      # * misc
      if (isContentTruthy(main.env$save.variable$Misc$abstract$file)) {
        main.env$save.variable$Misc$abstract <- gsub(
          ".*/dataPackagesOutput/emlAssemblyLine/",
          main.env$PATHS$eal.dp,
          main.env$save.variable$Misc$abstract$file
        )
        main.env$save.variable$Misc$methods <- gsub(
          ".*/dataPackagesOutput/emlAssemblyLine/",
          main.env$PATHS$eal.dp,
          main.env$save.variable$Misc$methods$file
        )
        main.env$save.variable$Misc$additional.information <- gsub(
          ".*/dataPackagesOutput/emlAssemblyLine/",
          main.env$PATHS$eal.dp,
          main.env$save.variable$Misc$additional.information$file
        )
      }

      # resume at saved page
      if(main.env$save.variable$step == 1){ # crashed on going to next
        main.env$EAL$page <- main.env$save.variable$step+1
        main.env$EAL$history <- main.env$VALUES$steps[1:main.env$EAL$page]
      }
      else{ # expected normal way
        main.env$EAL$page <- main.env$save.variable$step
        main.env$EAL$history <- main.env$save.variable$history
      }
      
      shinyjs::enable("dp_load")
    })

    # * Delete DP ----
    observeEvent(input$dp_delete, {
      req(isTruthy(input$dp_list))

      # variable operation - legibility purpose
      dp <- input$dp_list

      # actions
      showModal(
        modalDialog(
          title = "Delete data package?",
          ... = paste("Are you sure to delete", dp, "?"),
          easyClose = FALSE,
          footer = tagList(
            modalButton("No"),
            actionButton(
              session$ns("delete_confirm"), "Yes",
              class = "redButton"
            )
          ) # end footer
        ) # end modalDialog
      ) # end showModal
    },
    label = "EAL1: delete DP"
    )

    # If deletion is confirmed
    observeEvent(input$delete_confirm, {
      # variable operation - legibility purpose
      dp <- gsub(" \\(public\\)", "", input$dp_list)
      path <- paste0(main.env$PATHS$eal.dp, dp, "_emldp")

      # verbose
      showNotification(
        paste("Deleting:", dp, sep = "")
      )

      # actions
      unlink(path, recursive = TRUE)
      main.env$local.rv$dp.list <- main.env$local.rv$dp.list[
        !grepl(dp, main.env$local.rv$dp.list)
      ]
      # main.env$DP.LIST <- main.env$DP.LIST[!grepl(dp, main.env$DP.LIST$name)]
      removeModal()
    },
    label = "EAL1: confirm delete DP"
    )
  })
}
