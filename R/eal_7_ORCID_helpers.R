# EAL 7 Personnel ====
#' @import shiny
#'
#' @noRd
orcidInputUI <- function(id, val) {
  ns <- NS(id)

  textInput(
    ns("orcid"),
    tags$span("ORCID", icon("orcid"), "- facultative"),
    value = val
  )
}

#' @import shiny
#' @importFrom stringr str_extract
#'
#' @noRd
orcidInput <- function(id, main_env, row) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$orcid, {
      .value <- input$orcid
      .valid <- main_env$PATTERNS$ORCID %grep% .value

      if (isFALSE(.valid)) {
        # Save value
        main_env$local_rv$Personnel$userId[row()] <- ""
      } else {
        # Save value
        main_env$local_rv$Personnel$userId[row()] <-
          stringr::str_extract(.value, main_env$PATTERNS$ORCID)
        .orcid <- main_env$local_rv$Personnel$userId[row()]
        # Correct input
        updateTextInput(
          session = session,
          inputId = "orcid",
          value = .orcid
        )
      }

      # Feedbiack
      checkFeedback(
        input,
        "orcid",
        .valid,
        type = "warning",
        text = "ORCID format required"
      )
    },
    ignoreNULL = TRUE
    )
  })
}

# Settings ====

#' @import shiny
orcidUI <- function(id, globals) {
  uiOutput(NS(id, "infos"))
}

#' @import shiny
#' @importFrom stringr str_extract
#' @importFrom shinyjs onclick
#'
#' @noRd
orcid <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {

    # Set UI ====
    output$infos <- renderUI({
      if (isFALSE(main_env$SETTINGS$logged)) {
        tagList(
          textInput(session$ns("orcid"), "Write your ORCID here"),
          actionButton(
            session$ns("connect"),
            "Connect with ORCID",
            icon = icon("sign-in-alt")
          )
        )
      } else {
        tagList(
          tags$h3("Your name here"),
          tags$p("WIP: here will be your infos:"),
          tags$p(tags$b("ORCID: "), main_env$SETTINGS$user),
          actionButton(session$ns("disconnect"), "Logout",
            icon = icon("sign-out-alt")
          )
        )
      }
    })

    # Log in/out ====
    shinyjs::onclick("connect", {
      if (isTruthy(input$orcid) &&
        grepl(main_env$PATTERNS$ORCID, input$orcid)) {
        main_env$SETTINGS$logged <- TRUE
        main_env$SETTINGS$user <- stringr::str_extract(
          input$orcid, main_env$PATTERNS$ORCID
        )
      } else {
        showNotification(
          "Invalid or mistyped orcid.",
          type = "error"
        )
        updateTextInput(inputId = "orcid", value = "")
      }
    })

    shinyjs::onclick("disconnect", {
      main_env$SETTINGS$logged <- FALSE
      main_env$SETTINGS$user <- "public"
    })

    # TODO check for SNAKE connection

    ###
    # reso <- httr::GET(
    #   "https://orcid.org/"
    # )
    # cookie <- cookies(reso)[cookies(reso)$name == "XSRF-TOKEN", ]
    # credentials <- list(userId = "000-0003-3416-7653", password = "Z@bud!78")
    # resa <- httr::POST(
    #   "https://orcid.org/oauth/token",
    #   authenticate(
    #     credentials$userId,
    #     credentials$password
    #   ),
    #   add_headers(c(
    #     accept = "application/json",
    #     `x-xsrf-token` = cookie$value
    #   ))
    # )
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
    # cookie <- curl::handle_cookies(reso$handle) |>
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
  })
}
