# Create user counter -- only available inside a single container
users <- reactiveValues(count = 0)

#' @import shiny
#' @importFrom dataone listFormats CNode
#' @importFrom taxonomyCleanr view_taxa_authorities
#' @importFrom shinyjs hide show
#'
#' @noRd
server <- function(input, output, session) {
  # get variables & clean
  args <- base::get("metashark_args", envir = .GlobalEnv)
  if (args$dev) {
    rm("metashark_args", envir = .GlobalEnv)
  }

  # Setup environment
  main_env <- .setGlobalVariables(.args = args, envir = session$userData)
  # Locate "media/" for app
  addResourcePath("media", system.file("media/", package = "MetaShARK"))

  # Set user-specific data
  # TODO later
  assign(
    "credentials",
    reactiveValues(
      orcid = character(),
      name = character()
    ),
    envir = session$userData
  )
  assign(
    "contents",
    reactiveValues(
      dp.index = character()
    ),
    envir = session$userData
  )

  # App variables
  assign(
    "current_tab",
    reactive(input$sidemenu),
    envir = main_env
  )

  # Dev jobs ====
  # Dev button ----
  # all-accessible reactive for dev button
  assign(
    "dev_browse",
    reactive(input$dev),
    envir = main_env
  )
  # Output EAL step completeness
  if (main_env$dev) {
    output$eal_complete <- renderText(main_env$EAL$completed)
  }
  # Display / create dev elements
  if (main_env$dev) {
    shinyjs::show("dev")
    shinyjs::show("eal_complete")
    observeEvent(input$dev, {
        if (main_env$current_tab() != "fill" &&
          main_env$current_tab() != "upload") {
          browser()
        }
      },
      label = "server: dev toggle"
    )
  }

  # Test mode ----
  if (isTRUE(args$use.test)) {
    shinyjs::show("test_end")
    observeEvent(input$test_end, {
        stopApp()
      },
      label = "server: end test"
    )
  }

  # JS messages ----
  observeEvent(input$js_messages, {
    showNotification(
      tagList(
        tags$b("IMPORTANT !"),
        tags$p(input$js_messages)
      ),
      duration = NULL,
      type = "warning"
    )
  })

  # Update values ====
  # require to be in server, not .globalScript
  invisible({
    # Taxa authorities
    .TAXA_AUTHORITIES <- if (!main_env$dev) {
      try(taxonomyCleanr::view_taxa_authorities())
    } else {
      try(silent = TRUE)
    }
    if (class(.TAXA_AUTHORITIES) != "try-error") {
      .TAXA_AUTHORITIES <- readDataTable(
        isolate(main_env$PATHS$resources$taxaAuthorities.txt)
      )
      isolate(main_env$taxa_authorities <- .TAXA_AUTHORITIES)
    }

    # Ontology list
    .ONTOLOGIES <- data.table::fread(
      system.file(
        "resources/ontologies_list.tsv",
        package = "MetaShARK"
      )
    )
    isolate(main_env$ontologies <- .ONTOLOGIES)

    if (exists("template_issues")) {
      rm("template_issues", envir = .GlobalEnv)
    }
  })

  # User count ====
  # onSessionStart <-
  isolate({
    users$count <- users$count + 1
  })

  onSessionEnded(function() {
    isolate({
      users$count <- users$count - 1
    })
  })

  # Disclaimer
  observe({
      # Refresh every 5 minutes
      invalidateLater(5 * 60 * 1000)
      devmsg(
        tag = "Metric",
        "Connected users at %s: %s",
        Sys.time(),
        isolate(users$count)
      )
    },
    label = "users count"
  )

  # Modules called ====
  welcome("welcome", main_env)
  fill("fill", main_env) # EAL fill-in server part
  upload("upload", main_env)
  documentation("documentation", main_env)
  about("about")
  settings("settings", main_env)

  # Hide the loading message when the rest of the server function has executed
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")

  # Other information ----
  # Version -- based on .tar.gz archive
  output$version <- renderText({
    dir(".", pattern = "MetaShARK_.*.tar.gz") |>
      gsub(pattern = "MetaShARK_(.*).tar.gz", replacement = "\\1")
  })

  # File input max size
  output$file_limit <- renderText({
    sprintf(
      "Maximum size per file input : %s",
      options("shiny.maxRequestSize") |>
        gdata::humanReadable()
    )
  })

  # Observer for changes in settings side_tab
  observeEvent(main_env$SETTINGS$side_tab, {
    req(is.character(main_env$SETTINGS$side_tab))

    updateTabsetPanel(
      inputId = "settings_menu",
      selected = main_env$SETTINGS$side_tab
    )

    # reset
    isolate(main_env$SETTINGS$side_tab <- c())
  })
}

#' @noRd
listDP <- function(main_env) {
  list.files(
    main_env$PATHS$eal_dp,
    pattern = "_emldp$",
    full.names = FALSE
  ) |>
    gsub(pattern = "_emldp$", replacement = "")
}
