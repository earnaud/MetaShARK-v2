# Create user counter -- only available inside a single container
users = reactiveValues(count = 0)

#' @import shiny
#' @importFrom dataone listFormats CNode
#' @importFrom taxonomyCleanr view_taxa_authorities
#' @importFrom shinyjs hide show
#'
#' @noRd
server <- function(input, output, session) {
  # get variables & clean
  args <- base::get("metashark.args", envir = .GlobalEnv)
  if(args$dev)
    rm("metashark.args", envir = .GlobalEnv)
  
  # Setup environment
  main.env <- .globalScript(
    .args = args, 
    envir = session$userData
  )
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
    "current.tab",
    reactive(input$sidemenu),
    envir = main.env
  )
  
  # Dev jobs ====
  # Dev button ----
  # all-accessible reactive for dev button
  assign(
    "dev.browse",
    reactive(input$dev),
    envir = main.env
  )
  # Output EAL step completeness
  if (main.env$dev)
    output$eal_complete <- renderText(main.env$EAL$completed)
  # Display / create dev elements 
  if (main.env$dev){
    shinyjs::show("dev")
    shinyjs::show("eal_complete")
    shinyjs::show("disclaimer-wip")
    observeEvent(input$dev, {
      if (main.env$current.tab() != "fill" &&
          main.env$current.tab() != "upload")
        browser()
    }, label = "server: dev toggle")
  }
  
  # Test mode ----
  if(isTRUE(args$use.test)) {
    shinyjs::show("test_end")
    observeEvent(input$test_end, {
      stopApp()
    }, label = "server: end test")
    
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
    .TAXA.AUTHORITIES <- if(!main.env$dev)
      try(taxonomyCleanr::view_taxa_authorities()) else
        try(silent = TRUE)
    if (class(.TAXA.AUTHORITIES) != "try-error") {
      .TAXA.AUTHORITIES <- readDataTable(
        isolate(main.env$PATHS$resources$taxaAuthorities.txt)
      )
      isolate(main.env$taxa.authorities <- .TAXA.AUTHORITIES)
    }
    
    # Ontology list
    .ONTOLOGIES <- data.table::fread(
      system.file(
        "resources/ontologies_list.tsv",
        package = "MetaShARK"
      )
    )
    isolate(main.env$ontologies <- .ONTOLOGIES)
    
    if(exists("template_issues"))
      rm("template_issues", envir = .GlobalEnv)
  })
  
  # User count ====
  onSessionStart = isolate({
    users$count = users$count + 1
  })
  
  onSessionEnded(function() {
    isolate({
      users$count = users$count - 1
    })
  })
  
  # Disclaimer
  observe({
    # Refresh every 5 minutes
    invalidateLater(5*60*1000)
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
  fill("fill", main.env) # EAL fill-in server part
  upload("upload", main.env)
  documentation("documentation", main.env)
  about("about")
  settings("settings", main.env)
  
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
  
  # Observer for changes in settings side tab 
  observeEvent(main.env$SETTINGS$side.tab, {
    req(is.character(main.env$SETTINGS$side.tab))
    
    updateTabsetPanel(
      inputId = "settings_menu",
      selected = main.env$SETTINGS$side.tab
    )
    
    # reset
    isolate(main.env$SETTINGS$side.tab <- c())
  })
}

#' @noRd
listDP <- function(main.env) {
  list.files(
    main.env$PATHS$eal.dp,
    pattern = "_emldp$",
    full.names = FALSE
  ) |> 
    gsub(pattern = "_emldp$", replacement = "")
}