#' @import shiny
#' @importFrom dataone listFormats CNode
#' @importFrom taxonomyCleanr view_taxa_authorities
#' @importFrom shinyjs hide show
#'
#' @noRd
server <- function(input, output, session) {
  # get variables
  main.env <- get("main.env", options()$metashark.env)

  assign(
    "current.tab",
    reactive(input$side_menu),
    envir = main.env
  )

  if (main.env$dev)
    observeEvent(input$dev, {
      if (main.env$current.tab() != "fill")
        browser()
    })

  # Update values ====
  invisible({
    # DataONE nodes
    .DATAONE.LIST <- if(!main.env$dev) 
      try(dataone::listFormats(dataone::CNode())) else
        try(silent = TRUE)
    if (class(.DATAONE.LIST) != "try-error") {
      .DATAONE.LIST <- readDataTable(
        isolate(main.env$PATHS$resources$dataoneCNodesList.txt)
      )
      isolate(main.env$dataone.list <- .DATAONE.LIST)
    }
  
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
        "resources/bioportal_ontologies_list.csv",
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
  documentation("documentation")
  about("about")
  settings("settings", main.env)

  # Hide the loading message when the rest of the server function has executed
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
}

#' @importFrom dplyr %>% 
#'
#' @noRd
listDP <- function(main.env) {
  list.files(
    main.env$PATHS$eal.dp,
    pattern = "_emldp$",
    full.names = FALSE
  ) %>% gsub(pattern = "_emldp$", replacement = "")
}