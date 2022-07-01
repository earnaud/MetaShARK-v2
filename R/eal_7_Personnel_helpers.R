#' @import shiny
#'
#' @noRd
insertPersonnelInput <- function(id, main_env) {
  # Add row
  .id <- unns(id)
  if (!grepl("^_", .id)) {
    # add a new row to local table
    main_env$local_rv$Personnel[nrow(main_env$local_rv$Personnel) + 1, ] <-
      c(rep("", ncol(main_env$local_rv$Personnel) - 1), id = .id)
  }
  # create the UI
  new_ui <- PersonnelInputUI(id, main_env)
  # insert the UI
  insertUI(selector = "#inserthere_eal7", ui = new_ui, immediate = TRUE)
  # create the server
  PersonnelInput(.id, main_env)
}

#' @import shiny
#' @importFrom shinyjs hidden
#'
#' @noRd
PersonnelInputUI <- function(id, main_env) {
  # Setup
  ref <- unns(id)
  .value <- main_env$local_rv$Personnel[
    which(main_env$local_rv$Personnel$id == ref),
  ]

  # Form ====
  # ui <- tags$div(
  ui <- tags$div(
    useShinyjs(),
    id = NS(id, "container"),
    shinydashboard::box(
      id = NS(id, "box"),
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,

      # * Header ----
      title = tags$div(
        id = NS(id, "box-header-title"),
        # class = "topInputRow",
        class = "box-title-row",
        # Collapse
        # actionLink(NS(id, "collapse"), "", icon("chevron-circle-right")),
        tags$span(
          class = "box-title-form",
          # Role
          roleInputUI(
            NS(id, "role"),
            main_env$local_rv$role.choices,
            val = .value$role,
            width = "50%"
          ),
          # Show name
          tags$div(
            uiOutput(NS(id, "name")),
            style = "margin-top: 20px; padding: 6px; height: 40px; width: 50%;"
          )
        ),
        # Remove UI
        actionButton(NS(id, "remove"), "", icon("trash"), class = "redButton")
      ), # end of header
      tags$div(
        id = NS(id, "content"),
        # class = "contentRow",
        # * Identity ----
        fluidRow(
          # ORCID input
          column(4, orcidInputUI(NS(id, "orcid"), val = .value$userId)),
          # First name
          column(
            3,
            textInput(
              NS(id, "first_name"),
              "First name",
              value = .value$givenName
            )
          ),
          # Middle initial
          column(
            1,
            textInput(
              NS(id, "middle_initial"),
              label = tags$div("MI", title = "Middle Initial"),
              value = .value$middleInitial
            )
          ),
          # Last name
          column(
            4,
            textInput(
              NS(id, "last_name"),
              "Last name",
              value = .value$surName
            )
          )
        ),
        # * Address ----
        fluidRow(
          # Email
          column(
            5,
            textInput(
              NS(id, "email"),
              "Email address",
              value = .value$electronicMailAddress
            )
          ),
          # Organization
          column(
            7,
            textInput(
              NS(id, "organization"),
              "Organization name",
              value = .value$organizationName
            )
          )
        ),
        # * PI fields ----
        shinyjs::hidden(
          tags$div(
            id = NS(id, "PI"),
            fluidRow(
              # Project title
              column(
                4,
                textInput(
                  NS(id, "project_title"),
                  "Project title",
                  value = .value$projectTitle
                )
              ),
              # Funding agency
              column(
                4,
                textInput(
                  NS(id, "funding_agency"),
                  "Funding agency",
                  value = .value$fundingAgency
                )
              ),
              # Funding number
              column(
                4,
                textInput(
                  NS(id, "funding_number"),
                  "Funding number",
                  value = .value$fundingNumber
                )
              )
            )
          )
        ) # end of hidden PI fields
      ) # end of content
    )
  )
  # Output ====
  return(ui)
}

#' @import shiny
#' @importFrom shinyjs toggle show hide
#'
#' @noRd
PersonnelInput <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    # Setup ----
    row <- reactive({
      which(main_env$local_rv$Personnel$id == id)
    })
    name_pattern <- main_env$PATTERNS$name
    mail_pattern <- main_env$PATTERNS$email

    # [U] Collapse ====
    shinyjs::onclick("box-header-title", {
      shinyjs::runjs(
        sprintf(
          "$('#%s').closest('.box').find('[data-widget=collapse]').click();",
          session$ns("box")
        )
      )
    })

    # [I] Role ====
    roleInput("role", main_env, row = row)

    # Toggle PI fields
    observeEvent(input$`role-role`, {
      req(main_env$EAL$page == 7)
      shinyjs::toggle(
        "PI",
        condition = "PI" %grep% main_env$local_rv$Personnel$role[row()])
    },
    priority = -1,
    ignoreNULL = FALSE,
    label = "EAL7 toggle PI"
    )

    # [U] Verbose name ====
    output$name <- renderUI({
      .name <- paste(
        main_env$local_rv$Personnel$givenName[row()],
        main_env$local_rv$Personnel$middleInitial[row()],
        main_env$local_rv$Personnel$surName[row()],
        collapse = " "
      ) |>
        gsub(pattern = " +", replacement = " ") |>
        gsub(pattern = "^ +$", replacement = "")

      validate(
        need(.name != "", "No provided name")
      )

      return(.name)
    })

    # [U] Remove ====
    observeEvent(input$remove, {
      # remove the UI
      removeUI(
        selector = paste0("#", session$ns("container")), immediate = TRUE
      )
      # remove data
      main_env$local_rv$Personnel <- main_env$local_rv$Personnel[-row(), ]
    })

    # [I] ORCID ====
    orcidInput("orcid", main_env, row = row)
    observeEvent(input[["orcid-orcid"]], {
      .value <- input[["orcid-orcid"]]

      req(main_env$PATTERNS$ORCID %grep% .value)

      .value <- stringr::str_extract(.value, main_env$PATTERNS$ORCID)
      # Get ORCID record
      result <- httr::GET(
        sprintf("https://pub.orcid.org/v3.0/%s", .value),
        httr::add_headers(Accept = "application/json")
      )
      result$content <- result$content |>
        rawToChar() |>
        jsonlite::fromJSON()

      # Update inputs with orcid record
      .given_name <- try(result$content$person$name$`given-names`$value)
      if (class(.given_name) != "try-error") {
        updateTextInput(session, "first_name", value = .given_name)
      }

      .last_name <- try(result$content$person$name$`family-name`$value)
      if (class(.last_name) != "try-error") {
        updateTextInput(session, "last_name", value = .last_name)
      }

      email <- try(result$content$person$emails$email$email)
      if (class(email) != "try-error") {
        updateTextInput(session, "email", value = email)
      }

      .organization_name <- try(result$content$`activities-summary`$employments$
        `affiliation-group`$summaries[[1]]$
        `employment-summary`$organization$name)
      if (class(.organization_name) != "try-error") {
        updateTextInput(session, "organization", value = .organization_name)
      }
    })

    # [I] Names ====
    # * First name ----
    observeEvent(input$first_name, {
      main_env$local_rv$Personnel$givenName[row()] <- input$first_name
      checkFeedback(input, "first_name", name_pattern %grep% input$first_name)
    })

    # * Middle initial ----
    observeEvent(input$middle_initial, {
      main_env$local_rv$Personnel$middleInitial[row()] <- input$middle_initial
    })

    # * Last name ----
    observeEvent(input$last_name, {
      main_env$local_rv$Personnel$surName[row()] <- input$last_name
      checkFeedback(input, "last_name", name_pattern %grep% input$last_name)
    })

    # [I] Email ====
    observeEvent(input$email, {
      main_env$local_rv$Personnel$electronicMailAddress[row()] <- input$email
      checkFeedback(input, "email", mail_pattern %grep% input$email)
    })

    # [I] Organization ====
    observeEvent(input$organization, {
      main_env$local_rv$Personnel$organizationName[row()] <- input$organization
      checkFeedback(input, "organization")
    })

    # [I] PI fields ====

    # * Project title ----
    observeEvent(input$project_title, {
      main_env$local_rv$Personnel$projectTitle[row()] <- input$project_title
    })

    # * Funding agency ----
    observeEvent(input$funding_agency, {
      main_env$local_rv$Personnel$fundingAgency[row()] <- input$funding_agency
    })

    # * Funding number ----
    observeEvent(input$funding_number, {
      main_env$local_rv$Personnel$fundingNumber[row()] <- input$funding_number
    })

    # (End) ====
  })
}
