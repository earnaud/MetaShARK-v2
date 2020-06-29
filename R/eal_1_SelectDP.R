#' @title Data Package selection
#'
#' @description UI part for the Data Package selection. Allow the user to choose between
#' creating a new data package or loading an existing one.
#'
#' @importFrom shiny NS fluidPage fluidRow column tags tagList icon textOutput uiOutput selectInput textInput HTML
#' @importFrom shinyFiles shinyDirButton
SelectDPUI <- function(id, width = 12, dev = FALSE) {
  ns <- NS(id)

  # UI output
  return(
    fluidPage(
      title = "Organize data packages",
      fluidRow(
        collapsibleUI(
          ns("usage"),
          "TUTORIAL: EML Assembly Line workflow",
          ... = tagList(
            tags$p(tags$b("Welcome in the EML Assembly line."), "This tool is
              basically designed as a package, embedded in Shiny within MetaShARK.
              This little helper aims to show you what awaits you in the further 
              steps."),
            hr(),
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
      # Data package location -----------------------------------------------------
      # if (!isTRUE(server)) {
      #   tagList(
      #     fluidRow(
      #       column(4,
      #         if (isTRUE(server)) {
      #           tags$b("Data Package will be saved in:")
      #         } else {
      #           shinyDirButton(
      #             ns("dp_location"),
      #             "Choose directory",
      #             "DP save location",
      #             icon = icon("folder-open")
      #           )
      #         }
      #       ),
      #       column(8,
      #         textOutput(ns("dp_location")),
      #         style = "text-align: left;"
      #       ),
      #       class = "inputBox"
      #     ),
      #     fluidRow(
      #       tags$p("This is the location where your data packages will be
      #     saved. A folder will be created, respectively named
      #     after your input.")
      #     )
      #   )
      # } else {
      hr(),
      # },
      fluidRow(
        # Load existing DP -----------------------------------------------------
        column(
          ceiling(width / 2),
          tags$h4("Edit existing data package",
            style = "text-align:center"
          ),
          uiOutput(ns("dp_list"))
        ),
        # Create DP -----------------------------------------------------
        column(
          floor(width / 2),
          tags$h4("Create new data package",
            style = "text-align:center"
          ),
          checkboxInput(
            ns("quick"),
            tagList(
              tags$b("Quick mode"),
              "Most fields will be automatically filled"
            ),
            value = TRUE
          ),
          # Data package title
          textInput(
            ns("dp_name"),
            "Data package name",
            placeholder = paste0(Sys.Date(), "_project")
          ),
          textInput(
            ns("dp_title"),
            "Dataset title",
            placeholder = "Any title is a title"
          ),
          tags$div(
            id = "license-help",
            selectInput(
              ns("license"),
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
          uiOutput(ns("dp_create"))
        ) # end column2
      ) # end fluidRow
    ) # end fluidPage
  ) # end return
}

#' @title Data Package selection
#'
#' @description UI part for the Data Package selection. Allow the user to choose between
#' creating a new data package or loading an existing one.
#'
#' @importFrom shiny reactiveValues observeEvent req renderText renderUI validate radioButtons actionButton reactive
#' showModal modalDialog modalButton removeModal
#' @importFrom shinyFiles getVolumes shinyDirChoose parseDirPath
#' @importFrom shinyjs enable disable onclick
#' @importFrom EMLassemblyline template_directories template_core_metadata
#' @importFrom jsonlite read_json unserializeJSON
SelectDP <- function(input, output, session,
                     savevar, globals) {
  ns <- session$ns

  if (globals$dev) {
    onclick("dev",
      {
        req(globals$EMLAL$NAVIGATE == 1)
        browser()
      },
      asis = TRUE
    )
  }

  callModule(collapsible, "usage")

  # variable initialization -----------------------------------------------------
  rv <- reactiveValues(
    dp_location = globals$DEFAULT.PATH,
    dp_name = character(),
    dp_title = character(),
    dp_list = NULL,
    dp_license = NULL,
    warning_dp_name = NULL
  )

  # DP location -----------------------------------------------------
  observeEvent(input$dp_location,
    {
      req(input$dp_location)
      rv$dp_location <- input$dp_location
    },
    label = "EAL1: input dp location"
  )

  # Render selected DP location
  output$dp_location <- renderText({
    rv$dp_location
  })

  # DP load -----------------------------------------------------
  # reset input if user comes back on this screen
  # fetch list of DP at selected location
  observeEvent(rv$dp_location,
    {
      dpList <- list.files(rv$dp_location, pattern = "_emldp$")
      if (length(dpList) != 0) {
        rv$dp_list <- sub("_emldp", "", dpList)
      } else {
        rv$dp_list <- NULL
      }
    },
    label = "EAL1: build dp list"
  )

  # Render list of DP at selected location
  output$dp_list <- renderUI({
    # req(rv$dp_list)
    validate(
      need(
        isTruthy(rv$dp_list),
        "No existing data package at this location"
      )
    )
    tagList(
      radioButtons(
        ns("dp_list"),
        NULL,
        choiceNames = c("None selected", rv$dp_list),
        choiceValues = c("", rv$dp_list)
      ),
      actionButton(ns("dp_load"), "Load", icon = icon("folder-open")),
      actionButton(ns("dp_delete"), "Delete", icon = icon("minus-circle"), class = "redButton"),
      downloadButton(ns("dp_download"), label = "Download .zip", icon = icon("file-download"))
    )
  })

  output$dp_download <- downloadHandler(
    filename = function() {
      paste0(input$dp_list, "_emldp.zip")
    },
    content = function(file) {
      .path <- getwd()
      setwd(globals$DEFAULT.PATH)
      zip(
        zipfile = file,
        files = dir(
          gsub("/+", "/", dir(".", full.names = TRUE, pattern = input$dp_list)),
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
        enable("dp_load")
        enable("dp_delete")
        enable("dp_download")
      }
      else {
        disable("dp_load")
        disable("dp_delete")
        disable("dp_download")
      }
    },
    label = "EAL1: UX hs"
  )

  # DP create -----------------------------------------------------
  # check name input
  rv$valid_name <- FALSE
  output$dp_create <- renderUI({
    rv$valid_name <- FALSE
    validate(
      need(
        nchar(input$dp_name) > 3,
        "Please type a name with at least 3 characters."
      ),
      need(
        grepl("^[[:alnum:]_-]+$", input$dp_name)
        && nzchar(input$dp_name),
        "Only authorized characters are alphanumeric, '_' (underscore) and '-' (hyphen)."
      ),
      need(
        input$dp_name != ""
        && !(input$dp_name %in% rv$dp_list),
        "This name is already used: change either save directory or data package name."
      ),
      need(
        input$dp_title != ""
        && grepl("^[[:alnum:]\\ \\.,:_-]+$", input$dp_title),
        "This title has invalid character: use alphanumerics, 
        \" \", \".\", \",\", \":\", \"_\" or \"-\"."
      )
    )
    rv$valid_name <- TRUE
    return(actionButton(ns("dp_create"), "Create"))
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
    label = "EAL1: quick?"
  )

  observeEvent(input$dp_name,
    {
      rv$dp_name <- input$dp_name
    },
    label = "EAL1: save dp name"
  )

  observeEvent(input$dp_title,
    {
      rv$dp_title <- input$dp_title
    },
    label = "EAL1: save dp title"
  )

  # license choice
  observeEvent(input$license,
    {
      rv$dp_license <- input$license
    },
    label = "EAL1: save dp license"
  )

  # DP management - on clicks -----------------------------------------------------
  # * Create DP -----------------------------------------------------
  onclick("dp_create", {
    req(input$dp_create)
    req(input$dp_name)
    req(rv$valid_name)

    # variable operation - legibility purpose
    dp <- input$dp_name
    path <- paste0(rv$dp_location, dp, "_emldp")
    title <- input$dp_title
    license <- rv$dp_license

    # verbose
    withProgress(
      {
        # save in empty dedicated variable
        savevar$emlal <- initReactive("emlal", savevar, globals$EMLAL)
        savevar$emlal$SelectDP$dp_name <- dp
        savevar$emlal$SelectDP$dp_path <- path
        savevar$emlal$SelectDP$dp_metadata_path <- paste(path, dp, "metadata_templates", sep = "/")
        savevar$emlal$SelectDP$dp_data_path <- paste(path, dp, "data_objects", sep = "/")
        savevar$emlal$SelectDP$dp_eml_path <- paste(path, dp, "eml", sep = "/")
        savevar$emlal$SelectDP$dp_title <- title
        savevar$emlal$quick <- input$quick
        incProgress(0.2)

        dir.create(path, recursive = TRUE)
        incProgress(0.2)

        # EAL template import
        try(
          template_directories(
            savevar$emlal$SelectDP$dp_path,
            savevar$emlal$SelectDP$dp_name
          )
        )
        incProgress(0.2)
        x <- try(
          template_core_metadata(
            savevar$emlal$SelectDP$dp_metadata_path,
            license
          )
        )
        incProgress(0.2)

        if (class(x) != "try-error") {
          rv$dp_list <- c(rv$dp_list, dp)
          globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + 1
          saveReactive(savevar)
          incProgress(0.2)
        } else {
          unlink(path, recursive = TRUE)
          savevar <- initReactive(glob = globals$EMLAL)
          incProgress(0.2)
          showNotification(x, type = "error")
        }
      },
      message = paste("Creating:", path, "\n", sep = "")
    )
  })

  # * Load DP -----------------------------------------------------
  onclick("dp_load", {
    req(input$dp_list)
    disable("dp_load")
    # variable operation - legibility purpose
    dp <- input$dp_list
    path <- paste0(rv$dp_location, dp, "_emldp")

    # verbose
    showNotification(
      paste("Loading:", path, "\n", sep = ""),
      type = "message"
    )

    # actions
    savevar$emlal <- initReactive("emlal", savevar, globals$EMLAL)

    .savevar <- read_json(paste0(path, "/", dp, ".json"))[[1]] %>%
      unserializeJSON()
    savevar$emlal <- setSavevar(.savevar$emlal, savevar$emlal)

    # TODO remove this later : update history
    savevar$emlal$history <- sapply(savevar$emlal$history, function(h) {
      switch(h,
        create = "Select Data Package",
        DataFiles = "Data Files",
        attributes = "Attributes",
        CustomUnits = NULL,
        CatVars = "Categorical Variables",
        GeoCov = "Geographic Coverage",
        TaxCov = "Taxonomic Coverage",
        h
      )
    }) %>% unname()
    savevar$emlal$quick <- isTRUE(savevar$emlal$quick)
    globals$EMLAL$NAVIGATE <- ifelse(savevar$emlal$step > 1, # resume where max reached
      savevar$emlal$step,
      globals$EMLAL$NAVIGATE + 1
    )
    globals$EMLAL$HISTORY <- savevar$emlal$history
    enable("dp_load")
  })

  # * Delete DP -----------------------------------------------------
  onclick("dp_delete", {
    req(input$dp_list)

    # variable operation - legibility purpose
    dp <- input$dp_list

    # actions
    showModal(
      modalDialog(
        title = "Delete data package?",
        paste("Are you sure to delete", dp, "?"),
        footer = tagList(
          modalButton("No"),
          actionButton(
            ns("delete_confirm"), "Yes",
            class = "redButton"
          )
        ) # end footer
      ) # end modalDialog
    ) # end showModal
  })

  # If deletion is confirmed
  onclick("delete_confirm", {
    # variable operation - legibility purpose
    dp <- input$dp_list
    path <- paste0(rv$dp_location, dp, "_emldp")

    # verbose
    showNotification(
      paste("Deleting:", path, "\n", sep = "")
    ) # to replace by deleting DP

    # actions
    unlink(path, recursive = TRUE)
    rv$dp_list <- rv$dp_list[rv$dp_list != dp]
    removeModal()
  })

  # Output -----------------------------------------------------
  return(savevar)
}

#' @describeIn SelectDPUI
#'
#' Sets up a DP from a pre-written EML file.
#'
#' @importFrom EML read_eml
loadFromEALDP <- function(savevar, file) {
  loaded.eml <- read_eml(file)
}
