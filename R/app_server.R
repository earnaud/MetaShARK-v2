#' @import shiny
#' @importFrom dataone listFormats CNode
#' @importFrom taxonomyCleanr view_taxa_authorities
#' @importFrom shinyjs hide show
#'
#' @noRd
server <- function(input, output, session) {
  # get variables
  args <- get("metashark.args", envir = .GlobalEnv)
  rm("metashark.args", envir = .GlobalEnv)
  ui.steps <- get("ui.steps", envir = .GlobalEnv)
  rm("ui.steps", envir = .GlobalEnv)
  main.env <- .globalScript(
    .args = args, 
    envir = session$userData, 
    list(ui.steps = ui.steps)
  )
  addResourcePath("media", system.file("media/", package = "MetaShARK"))
  
  # Set user-specific data
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
  assign(
    "dev.browse",
    reactive(input$dev),
    envir = main.env
  )
  
  if (main.env$dev){
    shinyjs::show("dev")
    observeEvent(input$dev, {
      if (main.env$current.tab() != "fill" &&
          main.env$current.tab() != "upload")
        browser()
    }, label = "server: dev toggle")
  }
  
  if(isTRUE(args$use.test)) {
    shinyjs::show("test_end")
    observeEvent(input$test_end, {
      stopApp()
    }, label = "server: end test")
    
  }
  
  # Update values ====
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
    isolate(main.env$ontologies <- .TAXA.AUTHORITIES)
    
    if(exists("template_issues"))
      rm("template_issues", envir = .GlobalEnv)
  })

  ## modules called ----
  fill("fill", main.env)
  upload("upload", main.env)
  documentation("documentation", main.env)
  about("about")
  settings("settings", main.env)

  # Hide the loading message when the rest of the server function has executed
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
  
  # Version ----
  output$version <- renderText({
    dir(".", pattern = "MetaShARK_.*.tar.gz") |>
      gsub(pattern = "MetaShARK_(.*).tar.gz", replacement = "\\1")
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