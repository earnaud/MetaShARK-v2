# EAL 7 Personnel ====
#' @import shiny
#'
#' @noRd
orcidInputUI <- function(id, val) {
  ns <- NS(id)
  
  textInput(
    NS(id, "orcid"),
    tags$span("ORCID", icon("orcid")),
    value = val
  )
}

#' @import shiny
#' @importFrom stringr str_extract
#'
#' @noRd
orcidInput <- function(id, main.env, row) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$orcid, {
      .value <- input$orcid
      .valid <- main.env$PATTERNS$ORCID %grep% .value
      
      if(isFALSE(.valid)) {
        # Save value
        main.env$local.rv$Personnel$userId[row()] <- ""
      } else {
        # Save value
        main.env$local.rv$Personnel$userId[row()] <-
          stringr::str_extract(.value, main.env$PATTERNS$ORCID)
        .orcid <- main.env$local.rv$Personnel$userId[row()]
        # Correct input
        updateTextInput(
          session = session,
          inputId = "orcid",
          value = .orcid
        )
      }
      
      # Feedback 
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
  
# * (ORCID) Personnel identification
#     orcid.pattern <- main.env$PATTERNS$ORCID
#     
#     # Orcid 
#     observeEvent(input$userId, {
#       shinyFeedback::hideFeedback("userId")
#       local.rv$userId <- input$userId
#       
#       if (orcid.pattern %grep% input$userId) {
#         local.rv$userId <- stringr::str_extract(local.rv$userId, orcid.pattern)
#         updateTextInput(
#           session = session,
#           inputId = "userId",
#           value = local.rv$userId
#         )
#         shinyFeedback::showFeedbackSuccess("userId")
#       } else {
#         shinyFeedback::showFeedbackDanger("userId", text = "")
#       }
#       
#       orcid.connect <- try(
#         rorcid::as.orcid(
#           local.rv$userId # TODO ORCID auth % snake
#         )
#       )
#       
#       if (orcid.pattern %grep% input$userId && isTruthy(orcid.connect)) {
#         orcid <- local.rv$userId
#         orcid.info <- list()
#         
#         # names
#         orcid.info$names <- rorcid::orcid_person(orcid)[[orcid]]$name
#         if (isTruthy(unlist(orcid.info$names$`given-names`$value))) {
#           local.rv$givenName <- orcid.info$names$`given-names`$value
#           updateTextInput(session, "givenName", value = local.rv$givenName)
#         }
#         if (isTruthy(unlist(orcid.info$names$`family-name`$value))) {
#           local.rv$surName <- orcid.info$names$`family-name`$value
#           updateTextInput(session, "surName", value = local.rv$surName)
#         }
#         
#         # organization
#         orcid.info$employment <- rorcid::orcid_employments(orcid)[[orcid]]$`affiliation-group`$summaries[[1]]
#         if (isTruthy(unlist(orcid.info$employment$`employment-summary.organization.name`))) {
#           local.rv$organizationName <- orcid.info$employment$`employment-summary.organization.name`
#           updateTextInput(session, "organizationName", value = local.rv$organizationName)
#         }
#         if (is.null(role) &&
#             isTruthy(unlist(orcid.info$employment$`employment-summary.role-title`))) {
#           local.rv$role <- "(other)"
#           updateTextInput(session, "role", value = local.rv$role)
#           local.rv$`role-other` <- orcid.info$employment$`employment-summary.role-title`
#           updateTextInput(session, "role-other", value = local.rv$`role-other`)
#         }
#         
#         # email
#         orcid.info$email <- rorcid::orcid_email(orcid)[[orcid]]$email
#         if (isTruthy(unlist(orcid.info$email$email))) {
#           local.rv$electronicMailAddress <- orcid.info$email$email
#           updateTextInput(session, "electronicMailAddress", value = local.rv$electronicMailAddress)
#         }
#         
#         # fundings
#         if (local.rv$role == "PI (principal investigator)") {
#           orcid.info$fundings <- rorcid::orcid_fundings(orcid)[[orcid]]$group$`funding-summary`[[1]]
#           if (isTruthy(unlist(orcid.info$fundings$`title.title.value`))) {
#             local.rv$projectTitle <- orcid.info$fundings$`title.title.value`
#             updateTextInput(session, "projectTitle", value = local.rv$projectTitle)
#           }
#           if (isTruthy(unlist(orcid.info$fundings$`organization.name`))) {
#             local.rv$fundingAgency <- orcid.info$fundings$`organization.name`
#             updateTextInput(session, "fundingAgency", value = local.rv$fundingAgency)
#           }
#           if (isTruthy(unlist(orcid.info$fundings$`put-code`))) {
#             local.rv$fundingNumber <- orcid.info$fundings$`put-code`
#             updateTextInput(session, "fundingNumber", value = local.rv$fundingNumber)
#           }
#         }
#       } else {
#         showNotification(
#           id = session$ns("invalid_userid"),
#           "Input 'userId' is not a valid ORCID.",
#           type = "warning"
#         )
#       }
#     })


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
orcid <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {

    # Variable initialization ====
    .SETTINGS <- main.env$SETTINGS

    # Set UI ====
    # observeEvent(.SETTINGS$logged, {
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
          tags$p(tags$b("ORCID: "), .SETTINGS$user),
          actionButton(NS(id, "disconnect"), "Logout", icon = icon("sign-out-alt"))
        )
      }
    })
    # })

    # Log in/out ====
    shinyjs::onclick("connect", {
      if (isTruthy(input$orcid) && grepl(main.env$PATTERNS$ORCID, input$orcid)) {
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
  })
}
