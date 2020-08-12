#' @import shiny
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyjs onclick
#'
#' @noRd
appServer <- function(input, output, session) {
  # get variables
  main.env <- get("main.env", .GlobalEnv)

  assign(
    "current.tab",
    reactive(input$side_menu),
    envir = main.env
  )

  if (main.env$dev) {
    shinyjs::onclick(
      "dev",
      {
        if (main.env$current.tab() != "fill") {
          browser()
        }
      },
      asis = TRUE
    )
  }

  # Update values ====
  # DataONE nodes
  .DATAONE.LIST <- try(dataone::listFormats(dataone::CNode()))
  if (class(.DATAONE.LIST) != "try-error") {
    isolate(main.env$dataone.list <- .DATAONE.LIST)
    data.table::fwrite(
      .DATAONE.LIST,
      isolate(main.env$PATHS$resources$dataoneCNodesList.txt)
    )
  }

  # Taxa authorities
  .TAXA.AUTHORITIES <- try(taxonomyCleanr::view_taxa_authorities())
  if (class(.TAXA.AUTHORITIES) != "try-error") {
    isolate(main.env$taxa.authorities <- .TAXA.AUTHORITIES)
    data.table::fwrite(
      .TAXA.AUTHORITIES,
      isolate(main.env$PATHS$resources$taxaAuthorities.txt)
    )
  }

  # Head bar server ----
  # Options
  # observeEvent(input$settings, {
  #   shinydashboard::updateTabItems(session, "side_menu", "settings")
  # })

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

#' @noRd
listDP <- function(main.env) {
  list.files(
    main.env$PATHS$eal.dp,
    pattern = "_emldp$",
    full.names = FALSE
  ) %>% gsub(pattern = "_emldp$", replacement = "")
}