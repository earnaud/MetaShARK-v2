#' @importFrom shiny NS uiOutput
orcidUI <- function(id, globals) {
  ns <- NS(id)

  uiOutput(ns("infos"))
}

#' @importFrom shiny renderUI tagList actionButton icon tags
#' @importFrom shinyjs onclick
orcid <- function(input, output, session) {
  ns <- session$ns

  # Initialize variables ====
  globals <- reactiveValues(
    SESSION = reactiveValues(
      LOGGED = FALSE,
      ORCID.TOKEN = character()
    )
  )

  # Set UI ====
  observeEvent(globals$SESSION$LOGGED, {
    output$infos <- renderUI({
      if (isFALSE(globals$SESSION$LOGGED)) {
        tagList(
          actionButton(
            ns("connect"),
            "Connect with ORCID",
            icon = icon("orcid")
          )
        )
      } else {
        tagList(
          tags$h3("You name here"),
          tags$p("WIP: here will be your infos."),
          actionButton(ns("disconnect"), "Logout")
        )
      }
    })
  })

  # Log in/out ====
  onclick("connect", {
    ###
    reso <- httr::GET(
      "https://orcid.org/"
    )
    cookie <- cookies(reso)[cookies(reso)$name == "XSRF-TOKEN", ]
    credentials <- list(userId = "000-0003-3416-7653", password = "Z@bud!78")
    resa <- httr::POST(
      "https://orcid.org/oauth/token",
      authenticate(
        credentials$userId,
        credentials$password
      ),
      add_headers(c(
        accept = "application/json",
        `x-xsrf-token` = cookie$value
      ))
    )
    ###

    # globals$SESSION$ORCID.TOKEN <- rorcid::orcid_auth(
    # ORCID.TOKEN <- rorcid::orcid_auth(
    #   reauth = TRUE
    # )
    # res <- httr::GET(
    #   "https://cn.dataone.org/portal/oauth?action=start",
    #   add_headers(paste("Authorization: ", ORCID.TOKEN))
    #   # add_headers(paste("Authorization: ", globals$SESSION$ORCID.TOKEN))
    # )
    # token <- httr::GET(
    #   "https://cn.dataone.org/portal/token"
    # )
    #
    # orcid <- crul::HttpClient$new(
    #   url = "https://orcid.org/"
    # )
    # reso <- orcid$get(
    #   "signin/auth.json"
    # )
    # cookie <- curl::handle_cookies(reso$handle) %>%
    #   filter(name == "XSRF-TOKEN")
    #
    #
    # session <- crul::HttpClient$new(
    #   url = "https://cn.dataone.org/",
    #   headers = list(
    #     Authorization = ORCID.TOKEN
    #   )
    # )
    # res <- session$get(
    #   "/portal/oauth?action=start"
    # )
    # res2 <- session$get(
    #   "/portal/token"
    # )

    browser()
    stop("still working")
    globals$SESSION$LOGGED <- TRUE
    message(globals$SESSION$LOGGED)
  })

  onclick("disconnect", {
    globals$SESSION <- reactiveValues(
      LOGGED = FALSE,
      ORCID.TOKEN = character()
    )
  })
}
