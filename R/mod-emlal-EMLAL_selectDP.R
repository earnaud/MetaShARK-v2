#' @title selectDPUI
#'
#' @description UI part of the EML AL module (step 1: load an existing
#' data package OR create a new one)
#'
#' @param title page title
#' @param width same width as in shiny::column
#' @param dev logical. Shall the dev items appear?
#'
#' @importFrom shiny fluidPage HTML imageOutput uiOutput NS icon
#' textOutput tags  textInput selectInput
#' @importFrom shinydashboard box
#' @importFrom shinyFiles shinyDirButton
selectDPUI <- function(id, title, width = 12, dev = FALSE) {
  ns <- NS(id)

  # UI output
  return(
    fluidPage(
      title = "Organize data packages",
      # Data package location ----
      fluidRow(
        column(
          4,
          shinyDirButton(ns("dp_location"), "Choose directory",
            "DP save location",
            icon = icon("folder-open")
          )
        ),
        column(8,
          textOutput(ns("dp_location")),
          style = "text-align: right;"
        ),
        class = "inputBox"
      ),
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
          # textOutput(ns("warning_dp_name")),
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
          # actionButton(ns("dp_create"),"Create")
        ) # end column2
      ) # end fluidRow
    ) # end fluidPage
  ) # end return
}

#' @title selectDP
#'
#' @description server part of the EML AL module (step 1: load an existing
#' data package OR create a new one)
#'
#' @param savevar global reactiveValue containing the saved information
#' entered by the user.
#' @param globals global list containing fixed setting values for the
#' app.
#'
#' @importFrom shiny observeEvent renderUI HTML reactiveValues req
#' renderText validate need radioButtons showModal modalDialog
#' modalButton removeModal
#' @importFrom data.table fread
#' @importFrom shinyFiles getVolumes shinyDirChoose parseDirPath
#' @importFrom shinyjs enable disable
#' @importFrom EMLassemblyline template_core_metadata template_directories
selectDP <- function(input, output, session,
                     savevar, globals) {
  # variable initialization ----
  ns <- session$ns
  DP.path <- globals$DEFAULT.PATH

  # local values - to save will communicate with other modules
  rv <- reactiveValues(
    # to save
    # Default DP location
    dp_location = DP.path,
    dp_name = NULL,
    # local only
    dp_list = NULL,
    dp_license = NULL,
    warning_dp_name = NULL
  )
  observeEvent(input$dev0, {
    browser()
  })
  volumes <- c(Home = globals$HOME, base = getVolumes()())

  # DP location ----
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
    # rv$dp_location <- input$dp_location
    rv$dp_location <- parseDirPath(volumes, input$dp_location)
    if (is.na(rv$dp_location)) {
      rv$dp_location <- save
    }
  })

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
        !is.null(rv$dp_list),
        "No existing data package"
      )
    )
    tagList(
      radioButtons(ns("dp_list"),
        NULL,
        choiceNames = c("None selected", rv$dp_list),
        choiceValues = c("", rv$dp_list)
      ),
      actionButton(ns("dp_load"), "Load"),
      actionButton(ns("dp_delete"), "Delete",
        class = "redButton"
      )
    )
  })

  # toggle Load and Delete buttons
  observeEvent(input$dp_list, {
    if (input$dp_list != "") {
      enable("dp_load")
      enable("dp_delete")
    }
    else {
      disable("dp_load")
      disable("dp_delete")
    }
  })

  # DP create ----
  # check name input
  rv$dp_name <- ""
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
      )
    )
    rv$valid_name <- TRUE
    return(actionButton(ns("dp_create"), "Create"))
  })

  # license choice
  rv$dp_license <- reactive({
    input$license
  })

  # DP management - on clicks----

  # Create DP
  observeEvent(input$dp_create, {
    req(input$dp_name)
    req(rv$valid_name)

    # variable operation - legibility purpose
    dp <- input$dp_name
    path <- paste0(rv$dp_location, dp, "_emldp")
    license <- rv$dp_license()

    # save in empty dedicated variable
    savevar$emlal <- initReactive("emlal", savevar)
    savevar$emlal$selectDP$dp_name <- dp
    savevar$emlal$selectDP$dp_path <- path

    # verbose
    message("Creating:", path, "\n", sep = "")

    # actions
    rv$dp_list <- c(rv$dp_list, dp)

    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + 1
    globals$EMLAL$PREVIOUS <- "create"

    dir.create(path)
    saveReactive(savevar, path, dp) # initial "commit"
    template_directories(
      path,
      dp
    )
    template_core_metadata(
      path,
      license
    )
  })

  # Load DP
  observeEvent(input$dp_load, {
    req(input$dp_list)

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
    globals$EMLAL$PREVIOUS <- "load"
  })

  # Delete DP
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
