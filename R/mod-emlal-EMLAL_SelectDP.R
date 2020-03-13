#' @title Data Package selection
#'
#' @description UI part for the Data Package selection. Allow the user to choose between
#' creating a new data package or loading an existing one.
#'
#' @importFrom shiny NS fluidPage fluidRow column tags tagList icon textOutput uiOutput selectInput textInput HTML
#' @importFrom shinyFiles shinyDirButton
SelectDPUI <- function(id, title, width = 12, dev = FALSE, server) {
  ns <- NS(id)

  # UI output
  return(
    fluidPage(
      title = "Organize data packages",
      # Data package location ----
      if (!isTRUE(server)) {
        fluidRow(
          column(
            4,
            if (isTRUE(server)) {
              tags$b("Data Package will be saved in:")
            } else {
              shinyDirButton(
                ns("dp_location"),
                "Choose directory",
                "DP save location",
                icon = icon("folder-open")
              )
            }
          ),
          column(8,
            textOutput(ns("dp_location")),
            style = "text-align: left;"
          ),
          class = "inputBox"
        )
      } else {
        NULL
      },
      tags$p("This is the location where your data packages will be
        saved. A folder will be created, respectively named
        after your input."),
      fluidRow(
        # Load existing DP ----
        column(
          ceiling(width / 2),
          tags$h4("Edit existing data package",
            style = "text-align:center"
          ),
          uiOutput(ns("dp_list"))
        ),
        # Create DP ----
        column(
          floor(width / 2),
          tags$h4("Create new data package",
            style = "text-align:center"
          ),

          # Data package title
          textInput(ns("dp_name"), "Data package name",
            placeholder = paste0(Sys.Date(), "_project")
          ),
          textInput(ns("dp_title"), "Dataset title",
            placeholder = "Any title is a title"
          ),
          tags$div(
            id = "license-help",
            selectInput(ns("license"),
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
#' @importFrom shinyjs enable disable
#' @importFrom EMLassemblyline template_directories template_core_metadata
SelectDP <- function(input, output, session,
                     savevar, globals, server) {
  # variable initialization ----
  ns <- session$ns
  DP.path <- globals$DEFAULT.PATH

  # local values - to save will communicate with other modules
  rv <- reactiveValues(
    # to save
    # Default DP location
    dp_location = DP.path,
    dp_name = character(),
    dp_title = character(),
    # local only
    dp_list = NULL,
    dp_license = NULL,
    warning_dp_name = NULL
  )
  observeEvent(input$dev, {
    browser()
  })

  # DP location ----
  if (!isTRUE(server)) {
    volumes <- c(Home = globals$HOME, base = getVolumes()())

    # chose DP location
    shinyDirChoose(input, ns("dp_location"),
      roots = volumes,
      # defaultRoot = HOME,
      session = session
    )
    # update reactive value
    observeEvent(input$dp_location, {
      # validity checks
      req(input$dp_location)

      # variable initialization
      save <- rv$dp_location

      # actions
      rv$dp_location <- parseDirPath(volumes, input$dp_location)
      if (is.na(rv$dp_location)) {
        rv$dp_location <- save
      }
    })
  }
  else {
    observeEvent(input$dp_location, {
      req(input$dp_location)
      rv$dp_location <- input$dp_location
    })
  }

  # Render selected DP location
  output$dp_location <- renderText({
    rv$dp_location
  })

  # DP load ----
  # reset input if user comes back on this screen
  # fetch list of DP at selected location
  observeEvent(rv$dp_location, {
    dpList <- list.files(rv$dp_location, pattern = "_emldp$")
    if (length(dpList) != 0) {
      rv$dp_list <- sub("_emldp", "", dpList)
    } else {
      rv$dp_list <- NULL
    }
  })

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
      if (server) {
        downloadButton(ns("dp_download"), label = "Download .zip", icon = icon("file-download"))
      } else {
        NULL
      }
    )
  })

  output$dp_download <- downloadHandler(
    filename = function() {
      paste(input$dp_list, "zip", sep = ".")
    },
    content = function(file) {
      browser()
      zip(
        zipfile = file,
        files = dir(
          gsub("/+", "/", dir(globals$DEFAULT.PATH, full.names = TRUE, pattern = input$dp_list)),
          recursive = TRUE,
          full.names = TRUE
        )
      )
    },
    contentType = "application/zip"
  )

  # toggle Load and Delete buttons
  observeEvent(input$dp_list, {
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
  })

  # DP create ----
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
        \" \" \".\" \",\" \":\" \"_\" or \"-\""
      )
    )
    rv$valid_name <- TRUE
    rv$dp_name <- input$dp_name
    rv$dp_title <- input$dp_title
    return(actionButton(ns("dp_create"), "Create"))
  })

  # license choice
  observeEvent(input$license, {
    rv$dp_license <- input$license
  })
  # rv$dp_license <- reactive({
  #   input$license
  # })

  # DP management - on clicks ----
  # * Create DP ----
  observeEvent(input$dp_create, {
    req(input$dp_name)
    req(rv$valid_name)

    # variable operation - legibility purpose
    dp <- input$dp_name
    path <- paste0(rv$dp_location, dp, "_emldp")
    title <- input$dp_title
    license <- rv$dp_license

    # save in empty dedicated variable
    savevar$emlal <- initReactive("emlal", savevar)
    savevar$emlal$SelectDP$dp_name <- dp
    savevar$emlal$SelectDP$dp_path <- path
    savevar$emlal$SelectDP$dp_metadata_path <- paste(path, dp, "metadata_templates", sep = "/")
    savevar$emlal$SelectDP$dp_data_path <- paste(path, dp, "data_objects", sep = "/")
    savevar$emlal$SelectDP$dp_eml_path <- paste(path, dp, "eml", sep = "/")
    savevar$emlal$SelectDP$dp_title <- title

    # verbose
    message("Creating:", path, "\n", sep = "")

    # actions
    rv$dp_list <- c(rv$dp_list, dp)

    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + 1
    globals$EMLAL$HISTORY <- "create"

    dir.create(path, recursive = TRUE)

    # initial "commit"
    saveReactive(savevar)

    # EAL template import
    template_directories(path, dp)
    template_core_metadata(path, license)
  })

  # * Load DP ----
  observeEvent(input$dp_load, {
    req(input$dp_list)
    disable("dp_load")
    # variable operation - legibility purpose
    dp <- input$dp_list
    path <- paste0(rv$dp_location, dp, "_emldp")

    # verbose
    message("Loading:", path, "\n", sep = "") # to replace by loading DP
    # actions
    savevar$emlal <- initReactive("emlal", savevar)
    savevar$emlal <- readRDS(paste0(path, "/", dp, ".rds"))$emlal
    globals$EMLAL$NAVIGATE <- ifelse(savevar$emlal$step > 1, # resume where max reached
      savevar$emlal$step,
      globals$EMLAL$NAVIGATE + 1
    )
    globals$EMLAL$HISTORY <- savevar$emlal$history
    enable("dp_load")
  })

  # * Delete DP ----
  observeEvent(input$dp_delete, {
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
  observeEvent(input$delete_confirm, {
    # variable operation - legibility purpose
    dp <- input$dp_list
    path <- paste0(rv$dp_location, dp, "_emldp")

    # verbose
    message("Deleting:", path, "\n", sep = "") # to replace by deleting DP

    # actions
    unlink(path, recursive = TRUE)
    rv$dp_list <- rv$dp_list[rv$dp_list != dp]
    removeModal()
  })

  # Output ----
  return(savevar)
}
