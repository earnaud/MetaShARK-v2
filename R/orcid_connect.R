#' @import shiny
orcidUI <- function(id, globals) {
  ns <- NS(id)
  
  uiOutput(NS(id, "infos"))
}

#' @import shiny
#' @importFrom shinyjs onclick
orcid <- function(input, output, session, main.env) {
  ns <- session$ns
  
  # Variable initialization ====
  .SETTINGS <- main.env$SETTINGS
  
  # Set UI ====
  observeEvent(.SETTINGS$logged, {
    output$infos <- renderUI({
      if (isFALSE(.SETTINGS$logged)) {
        tagList(
          textInput(NS(id, "orcid"), "Write your ORCID here"),
          actionButton(
            NS(id, "connect"),
            "Connect with ORCID",
            icon = icon("sign-in-alt")
          )
        )
      } else {
        tagList(
          tags$h3("Your name here"),
          tags$p("WIP: here will be your infos:"),
          fluidRow(
            column(6,
              tags$p(tags$b("ORCID: "), .SETTINGS$user)
            ),
            column(6)
          ),
          actionButton(NS(id, "disconnect"), "Logout", icon = icon("sign-out-alt"))
        )
      }
    })
  })
  
  # Log in/out ====
  shinyjs::onclick("connect", {
    if(isTruthy(input$orcid) && grepl(main.env$PATTERNS$ORCID, input$orcid)){
      .SETTINGS$logged <- TRUE
      .SETTINGS$user <- stringr::str_extract(input$orcid, main.env$PATTERNS$ORCID)
    }
    else {
      showNotification(
        "Invalid or mistyped orcid.",
        type = "error"
      )
      updateTextInput(inputId = "orcid", value = "")
    }
  })
  
  shinyjs::onclick("disconnect", {
    .SETTINGS$logged <- FALSE
    .SETTINGS$user <- "public"
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
  
}
