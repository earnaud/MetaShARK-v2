#' @import shiny
#'
#' @noRd
insertPersonnelInput <- function(id, rv, ns, main.env, role = NULL, saved = NULL) {

  # initialize IDs ----
  div.id <- id
  site.id <- paste0("site_", id)
  rmv.id <- paste0("rmv_", id)

  # Proper module server ----
  # insert new UI
  newUI <- PersonnelModUI(
    NS(id, id), div.id, site.id, rmv.id,
    role = role, saved = saved
  )
  insertUI(
    selector = paste0("#", NS(id, "inserthere")),
    ui = newUI
  )

  # create associated server
  rv <- PersonnelMod(
    id,
    main.env, rv, # reactiveValues
    rmv.id, site.id, div.id, # renderUI ids
    role = role, saved = saved # set saved
  )

  # Output ----
  return(rv)
}

#' @importFrom shinyBS bsTooltip
#'
#' @noRd
PersonnelModUI <- function(id, div.id, site.id, rmv.id,
                           role = NULL, saved = NULL) {
  value <- if (isContentTruthy(saved)) {
    saved[saved$id == div.id, ]
  } else {
    NULL
  }

  # set Project Information embedding tag
  .pi.embed <- if (!is.null(role)) {
    shinyjs::hidden
  } else {
    shiny::tagList
  }

  tags$div(
    id = site.id,
    fluidRow(
      class = "inputBox",
      # Form ----
      # column(11,
      tagList(
        # * (ORCID) Personnel identification ----
        fluidRow(
          class = "topInputRow",
          column(
            11,
            column(
              4,
              if (is.null(role)) {
                selectInput(
                  NS(id, "role"),
                  c("creator", "PI (principal investigator)", "contact", "(other)"),
                  label = withRedStar("Role"),
                  selected = if (!is.null(value)) {
                    if (value$role %in% c("creator", "PI (principal investigator)", "contact")) {
                      value$role
                    } else {
                      "(other)"
                    }
                  } else {
                    ""
                  }
                )
              } else {
                tags$b(paste("Role: ", role))
              }
            ),
            column(
              4,
              shinyjs::hidden(
                div(
                  id = NS(id, "role-other"),
                  textInput(
                    NS(id, "role-other"),
                    label = "Title of the custom role",
                    value = if (!is.null(value) &&
                      !value$role %in% c("creator", "PI (principal investigator)", "contact")) {
                      value$role
                    } else {
                      ""
                    }
                  )
                )
              )
            ),
            column(
              4,
              textInput(
                NS(id, "userId"),
                label = "ORCID",
                value = if (!is.null(value)) value$userId else ""
              )
            )
          ),
          column(1,
            if (is.null(role)) {
              actionButton(
                NS(id, rmv.id),
                "",
                icon("trash"),
                class = "danger"
              )
            },
            style = "padding-left: 0"
          )
        ), # end of fluidRow 1
        # * Basic identity ----
        fluidRow(
          style = "padding:5px",
          column(
            4,
            textInput(
              NS(id, "givenName"),
              label = withRedStar("First name"),
              value = if (!is.null(value)) value$givenName else ""
            )
          ),
          column(
            4,
            textInput(
              NS(id, "middleInitial"),
              label = "Middle initial",
              value = if (!is.null(value)) value$middleInitial else ""
            )
          ),
          column(
            4,
            textInput(
              NS(id, "surName"),
              label = withRedStar("Last name"),
              value = if (!is.null(value)) value$surName else ""
            )
          )
        ), # end of fluidRow 1
        # * Contact ----
        fluidRow(
          style = "padding:5px",
          column(
            8,
            textInput(
              NS(id, "organizationName"),
              label = withRedStar("Name of organization the person is associated with."),
              value = if (!is.null(value)) value$organizationName else ""
            )
          ),
          column(
            4,
            textInput(
              NS(id, "electronicMailAddress"),
              label = withRedStar("Email address"),
              value = if (!is.null(value)) value$electronicMailAddress else ""
            )
          )
        ), # end of fluidRow 2
        # * Project information ----
        .pi.embed(
          div(
            style = "padding:5px",
            id = "project_information",
            fluidRow(
              column(
                4,
                textInput(
                  NS(id, "projectTitle"),
                  label = "Project title for this dataset",
                  value = if (!is.null(value)) value$projectTitle else ""
                )
              ),
              column(
                4,
                textInput(
                  NS(id, "fundingAgency"),
                  label = "Entity funding the creation of this dataset",
                  value = if (!is.null(value)) value$fundingAgency else ""
                )
              ),
              column(
                4,
                textInput(
                  NS(id, "fundingNumber"),
                  label = "Number of the grant or award that supported creation of this dataset",
                  value = if (!is.null(value)) value$fundingNumber else ""
                )
              )
            )
          )
        ) # end of project information
      )
    )
  ) # end of module div
}

#' @import shiny
#' @importFrom rorcid as.orcid orcid_person orcid_employments orcid_email orcid_fundings
#' @importFrom stringr str_extract
#'
#' @noRd
PersonnelMod <- function(id, main.env, rv, rmv.id, site.id, ref, role = NULL, saved = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Variable initialization ----
    if (!is.null(saved)) {
      value <- saved[saved$id == ref, ]
    } else {
      value <- NULL
    }

    local.rv <- reactiveValues(
      id = ref,
      # Basic Identity
      givenName = if (!is.null(value)) value$givenName else character(),
      middleInitial = if (!is.null(value)) value$middleInitial else character(),
      surName = if (!is.null(value)) value$surName else character(),
      # Contact
      organizationName = if (!is.null(value)) value$organizationName else character(),
      electronicMailAddress = if (!is.null(value)) value$electronicMailAddress else character(),
      # Personnel information
      userId = if (!is.null(value)) value$userId else character(),
      role = if (!is.null(role)) role else if (!is.null(value)) value$role else character(),
      `role-other` = if (!is.null(role)) role else if (!is.null(value)) value$`role-other` else character(),
      # Project information
      projectTitle = if (!is.null(value)) value$projectTitle else NA,
      fundingAgency = if (!is.null(value)) value$fundingAgency else NA,
      fundingNumber = if (!is.null(value)) value$fundingNumber else NA
    )

    # * Basic Identity ----
    name.pattern <- main.env$PATTERNS$name

    observeEvent(input$givenName, {
      local.rv$givenName <- if (grepl(name.pattern, input$givenName)) {
        input$givenName
      }
    })

    observeEvent(input$middleInitial, {
      local.rv$middleInitial <- input$middleInitial
    })

    observeEvent(input$surName, {
      local.rv$surName <- if (grepl(name.pattern, input$surName)) {
        input$surName
      }
    })

    # * Contact ----
    mail.pattern <- main.env$PATTERNS$email

    observeEvent(input$organizationName, {
      local.rv$organizationName <- input$organizationName
    })

    observeEvent(input$electronicMailAddress, {
      local.rv$electronicMailAddress <- if (grepl(mail.pattern, input$electronicMailAddress)) {
        input$electronicMailAddress
      }
    })

    # * (ORCID) Personnel identification ----
    orcid.pattern <- main.env$PATTERNS$ORCID

    observeEvent(input$userId, {
      req(input$userId)
      local.rv$userId <- input$userId

      if (grepl(orcid.pattern, input$userId)) {
        local.rv$userId <- stringr::str_extract(local.rv$userId, orcid.pattern)
        updateTextInput(
          session = session,
          inputId = "userId",
          value = local.rv$userId
        )
      }

      orcid.connect <- try(
        rorcid::as.orcid(
          local.rv$userId # TODO ORCID auth % snake
        )
      )

      if (
        grepl(orcid.pattern, input$userId) &&
          isTruthy(orcid.connect)
      ) {
        orcid <- local.rv$userId
        orcid.info <- list()

        # names
        orcid.info$names <- rorcid::orcid_person(orcid)[[orcid]]$name
        if (isTruthy(unlist(orcid.info$names$`given-names`$value))) {
          local.rv$givenName <- orcid.info$names$`given-names`$value
          updateTextInput(session, "givenName", value = local.rv$givenName)
        }
        if (isTruthy(unlist(orcid.info$names$`family-name`$value))) {
          local.rv$surName <- orcid.info$names$`family-name`$value
          updateTextInput(session, "surName", value = local.rv$surName)
        }

        # organization
        orcid.info$employment <- rorcid::orcid_employments(orcid)[[orcid]]$`affiliation-group`$summaries[[1]]
        if (isTruthy(unlist(orcid.info$employment$`employment-summary.organization.name`))) {
          local.rv$organizationName <- orcid.info$employment$`employment-summary.organization.name`
          updateTextInput(session, "organizationName", value = local.rv$organizationName)
        }
        if (is.null(role) &&
          isTruthy(unlist(orcid.info$employment$`employment-summary.role-title`))) {
          local.rv$role <- "(other)"
          updateTextInput(session, "role", value = local.rv$role)
          local.rv$`role-other` <- orcid.info$employment$`employment-summary.role-title`
          updateTextInput(session, "role-other", value = local.rv$`role-other`)
        }

        # email
        orcid.info$email <- rorcid::orcid_email(orcid)[[orcid]]$email
        if (isTruthy(unlist(orcid.info$email$email))) {
          local.rv$electronicMailAddress <- orcid.info$email$email
          updateTextInput(session, "electronicMailAddress", value = local.rv$electronicMailAddress)
        }

        # fundings
        if (local.rv$role == "PI (principal investigator)") {
          orcid.info$fundings <- rorcid::orcid_fundings(orcid)[[orcid]]$group$`funding-summary`[[1]]
          if (isTruthy(unlist(orcid.info$fundings$`title.title.value`))) {
            local.rv$projectTitle <- orcid.info$fundings$`title.title.value`
            updateTextInput(session, "projectTitle", value = local.rv$projectTitle)
          }
          if (isTruthy(unlist(orcid.info$fundings$`organization.name`))) {
            local.rv$fundingAgency <- orcid.info$fundings$`organization.name`
            updateTextInput(session, "fundingAgency", value = local.rv$fundingAgency)
          }
          if (isTruthy(unlist(orcid.info$fundings$`put-code`))) {
            local.rv$fundingNumber <- orcid.info$fundings$`put-code`
            updateTextInput(session, "fundingNumber", value = local.rv$fundingNumber)
          }
        }
      } else {
        showNotification(
          id = NS(id, "invalid_userid"),
          "Input 'userId' is not a valid ORCID.",
          type = "warning"
        )
      }
    })

    # * Project information ----
    if (is.null(role)) {
      observeEvent(
        {
          input$role
          input$`role-other`
        },
        {
          # custom role
          if (input$role == "(other)") {
            shinyjs::show("role-other")
            local.rv$role <- input$`role-other`
          } else {
            shinyjs::hide("role-other")
            local.rv$role <- input$role
          }
          # project
          if (input$role == "PI (principal investigator)") {
            shinyjs::show("projectTitle")
            shinyjs::show("fundingAgency")
            shinyjs::show("fundingNumber")
          } else {
            shinyjs::hide("projectTitle")
            shinyjs::hide("fundingAgency")
            shinyjs::hide("fundingNumber")
          }
        },
        ignoreInit = FALSE
      )

      observeEvent(input$projectTitle, {
        if (input$role == "PI (principal investigator)") {
          local.rv$projectTitle <- input$projectTitle
        }
      })

      observeEvent(input$fundingAgency, {
        if (input$role == "PI (principal investigator)") {
          local.rv$fundingAgency <- input$fundingAgency
        }
      })

      observeEvent(input$fundingNumber, {
        if (input$role == "PI (principal investigator)") {
          local.rv$fundingNumber <- input$fundingNumber
        }
      })
    } else {
      local.rv$role <- role
      local.rv$projectTitle <- ""
      local.rv$fundingAgency <- ""
      local.rv$fundingNumber <- ""
    }

    # Metadata save ----
    observe({
      req(
        !is.null(role) ||
          (any(grepl(rmv.id, names(input))) &&
            input[[rmv.id]] < 1)
      )
      personnel <- isolate(rv$Personnel)
      # Fetch correct index
      ind <- if (ref %in% personnel$id) {
        match(ref, personnel$id) # find its index
      }
      else {
        dim(personnel)[1] + 1
      }

      # print values into rv at selected index
      .values <- printReactiveValues(local.rv)
      .values <- .values[colnames(personnel)]
      .values[which(!sapply(.values, isTruthy))] <- ""
      isolate(rv$Personnel[ind, ] <- .values)
    })

    # Remove UI ----
    if (is.null(role)) {
      observeEvent(input$rmv.id, {
        # unload the RV
        ind <- match(ref, rv$Personnel$id)
        rv$Personnel <- rv$Personnel %>% slice(-ind)

        # remove the UI
        removeUI(selector = paste0("#", site.id), immediate = TRUE)
      })
    }

    # Output ----
    return(rv)
  })
}
