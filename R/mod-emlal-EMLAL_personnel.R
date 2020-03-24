#' @title PersonnelUI
PersonnelUI <- function(id, title, dev) {
  ns <- NS(id)

  return(
    fluidPage(
      # Features UI -----------------------------------------------------
      column(
        10,
        fluidRow(
          column(
            2,
            actionButton(ns("addui"), "", icon("plus"))
          ),
          column(
            10,
            HTML("
              <p>Two roles are required to be filled: <b>creator and 
              contact.</b></p>
              <p>If a person serves more than one role, duplicate 
              this persons information. Similarly if a role is shared 
              among many people, duplicate these persons information.</p>
              <p>Filling the <b>orcid</b> field will automatically 
              fill the remaining fields.</p>
            ")
          )
        ),
        tags$div(id = ns("inserthere"))
      ), # end of column1
      column(2, navSidebar(ns("nav")) )
    ) # end of fluidPage
  ) # end of return
}

#' @title Personnel
#'
#' @description server part of Personnel module
#'
#' @importFrom shiny reactiveValues callModule observeEvent observe isTruthy 
#' @importFrom shinyjs enable disable
#' @importFrom data.table fwrite
Personnel <- function(input, output, session, savevar, globals) {
  ns <- session$ns

  # Variable initialization -----------------------------------------------------
  rv <- reactiveValues(
    Personnel = reactiveValues(
      id = numeric(),
      # Basic Identity
      givenName = character(),
      middleInitial = character(),
      surName = character(),
      # Contact
      organizationName = character(),
      electronicMailAddress = character(),
      # Personnel information
      userId = character(),
      role = character(),
      # Project information
      projectTitle = NA,
      fundingAgency = NA,
      fundingNumber = NA
    )
  )

  # Fill Personnel -----------------------------------------------------
  # Default created
  observeEvent(TRUE, once = TRUE, {
    rv$Personnel <- insertPersonnelInput(
      "creator",
      rv$Personnel,
      ns,
      globals,
      role = "creator"
    )
    rv$Personnel <- insertPersonnelInput(
      "contact",
      rv$Personnel,
      ns,
      globals,
      role = "contact"
    )
  })

  # User-dependant
  observeEvent(input$addui, {
    rv$Personnel <- insertPersonnelInput(
      as.numeric(input$addui),
      rv$Personnel,
      ns,
      globals
    )
  })

  # NSB -----------------------------------------------------
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar
  )
  callModule(
    nextTab, "nav",
    globals, "Personnel"
  )
  callModule(
    prevTab, "nav",
    globals
  )

  # Complete -----------------------------------------------------
  observe({
    rv$complete <- all(
      # Personnel
      isTruthy(rv$Personnel$givenName) &&
        isTruthy(rv$Personnel$surName) &&
        isTruthy(rv$Personnel$organizationName) &&
        isTruthy(rv$Personnel$electronicMailAddress) &&
        all(c("creator", "contact") %in% rv$Personnel$role)
    )

    if (rv$complete) {
      enable("nav-nextTab")
    } else {
      disable("nav-nextTab")
    }
  })

  # Process data -----------------------------------------------------
  observeEvent(input[["nav-nextTab"]], {
    message("Writing 'Personnel.txt'.")

    # prettify
    personnel <- printReactiveValues(rv$Personnel) %>%
      as.data.frame() %>%
      select(-id)
    personnel$role %>%
      replace(
        .,
        which(personnel$role == "(other)"),
        personnel$`role-other`[which(personnel$role == "(other)")]
      )
    cols <- c("givenName", "middleInitial", "surName", "organizationName", "electronicMailAddress", "userId", "role", "projectTitle", "fundingAgency", "fundingNumber")
    personnel <- personnel[, cols]

    # save
    savevar$emlal$Personnel$personnel <- personnel

    # write file
    fwrite(
      personnel,
      paste0(
        savevar$emlal$SelectDP$dp_metadata_path,
        "/personnel.txt"
      ),
      sep = "\t"
    )
  })

  # Output -----------------------------------------------------
  return(savevar)
}



#' @title PersonnelModUI
#'
#' @description module to document EML Personnel
#'
#' @importFrom shinyBS bsTooltip
PersonnelModUI <- function(id, site_id, rmv_id, role = NULL) {
  ns <- NS(id)

  div_id <- id

  tags$div(
    id = site_id,
    fluidRow(
      style = "inputBox",
      class = "inputBox",
      # Form -----------------------------------------------------
      column(11,
        tagList(
          # * Basic identity -----------------------------------------------------
          fluidRow(
            column(4,
              textInput(
                ns("givenName"),
                label = with_red_star("First name of person.")
              )
            ),
            column(4,
              textInput(
                ns("middleInitial"),
                label = "Middle initial of person."
              )
            ),
            column(4,
              textInput(
                ns("surName"),
                label = with_red_star("Last name of person.")
              )
            )
          ), # end of fluidRow 1
          # * Contact -----------------------------------------------------
          fluidRow(
            column(8,
              textInput(
                ns("organizationName"),
                label = with_red_star("Name of organization the person is associated with.")
              )
            ),
            column(4,
              textInput(
                ns("electronicMailAddress"),
                label = with_red_star("Email address of person.")
              )
            )
          ), # end of fluidRow 2
          # * Personnel identification -----------------------------------------------------
          fluidRow(
            column(4,
              textInput(
                ns("userId"),
                label = "ORCID of person"
              )
            ),
            column(4,
              if (is.null(role)) {
                selectInput(
                  ns("role"),
                  c("creator", "PI (principal investigator)", "contact", "(other)"),
                  label = with_red_star("Role of person.")
                )
              } else {
                tags$b(role)
              }
            ),
            column(4,
              hidden(
                div(
                  id = ns("role-other"),
                  textInput(
                    ns("role-other"),
                    label = "Type the title of the custom role"
                  )
                )
              )
            )
          ), # end of fluidRow 3
          # * Project information -----------------------------------------------------
          div(
            id = "project_information",
            fluidRow(
              column(4,
                textInput(
                  ns("projectTitle"),
                  label = "Title of the project this dataset was created under."
                )
              ),
              column(4,
                textInput(
                  ns("fundingAgency"),
                  label = "Name of the entity funding the creation of this dataset."
                )
              ),
              column(4,
                textInput(
                  ns("fundingNumber"),
                  label = "Number of the grant or award that supported creation of this dataset"
                )
              )
            )
          ) # end of fluidRow 4
        )
      ),
      # Destroy -----------------------------------------------------
      column(1,
        if (is.null(role)) {
          actionButton(
            ns(rmv_id),
            "",
            icon("trash"),
            class = "danger"
          )
        }
      )
    )
  )
}

#' @title PersonnelMod
#'
#' @describeIn PersonnelModUI
#'
#' @importFrom shiny insertUI removeUI
#' @importFrom rorcid as.orcid orcid_person orcid_employments orcid_email orcid_fundings 
#' @importFrom stringr str_extract
PersonnelMod <- function(input, output, session,
                         globals, rv, rmv_id, site_id, ref, role = NULL) {
  ns <- session$ns

  # Variable initialization -----------------------------------------------------
  localRV <- reactiveValues(
    id = ref,
    # Basic Identity
    givenName = character(),
    middleInitial = character(),
    surName = character(),
    # Contact
    organizationName = character(),
    electronicMailAddress = character(),
    # Personnel information
    userId = character(),
    role = if (is.null(role)) character() else role,
    `role-other` = character(),
    # Project information
    projectTitle = NA,
    fundingAgency = NA,
    fundingNumber = NA
  )

  # Basic Identity -----------------------------------------------------
  name.pattern <- globals$PATTERNS$NAME

  observeEvent(input$givenName, {
    localRV$givenName <- if (grepl(name.pattern, input$givenName)) {
      input$givenName
    }
  })

  observeEvent(input$middleInitial, {
    localRV$middleInitial <- input$middleInitial
  })

  observeEvent(input$surName, {
    localRV$surName <- if (grepl(name.pattern, input$surName)) {
      input$surName
    }
  })

  # Contact -----------------------------------------------------
  mail.pattern <- globals$PATTERNS$EMAIL

  observeEvent(input$organizationName, {
    req(input$organizationName)
    localRV$organizationName <- input$organizationName
  })

  observeEvent(input$electronicMailAddress, {
    req(input$electronicMailAddress)
    localRV$electronicMailAddress <- if (grepl(mail.pattern, input$electronicMailAddress)) {
      input$electronicMailAddress
    }
  })

  # (ORCID) Personnel identification -----------------------------------------------------
  orcid.pattern <- globals$PATTERNS$ORCID

  observeEvent(input$userId, {
    req(input$userId)
    localRV$userId <- input$userId
    
    if (grepl(orcid.pattern, input$userId))
      localRV$userId <- str_extract(localRV$userId, orcid.pattern)
    
    if (
      grepl(orcid.pattern, input$userId) &&
        isTruthy(try(as.orcid(input$userId)))
    ) {
      orcid <- input$userId
      orcid_info <- list()

      # names
      orcid_info$names <- orcid_person(orcid)[[orcid]]$name
      if (isTruthy(unlist(orcid_info$names$`given-names`$value))) {
        rv$givenName <- orcid_info$names$`given-names`$value
        updateTextInput(session, "givenName", value = rv$givenName)
      }
      if (isTruthy(unlist(orcid_info$names$`family-name`$value))) {
        rv$surName <- orcid_info$names$`family-name`$value
        updateTextInput(session, "surName", value = rv$surName)
      }

      # organization
      orcid_info$employment <- orcid_employments(orcid)[[orcid]]$`affiliation-group`$summaries[[1]]
      if (isTruthy(unlist(orcid_info$employment$`employment-summary.organization.name`))) {
        rv$organizationName <- orcid_info$employment$`employment-summary.organization.name`
        updateTextInput(session, "organizationName", value = rv$organizationName)
      }
      if (isTruthy(unlist(orcid_info$employment$`employment-summary.role-title`))) {
        rv$role <- "(other)"
        updateTextInput(session, "role", value = rv$role)
        rv$`role-other` <- orcid_info$employment$`employment-summary.role-title`
        updateTextInput(session, "role-other", value = rv$`role-other`)
      }

      # email
      orcid_info$email <- orcid_email(orcid)[[orcid]]$email
      if (isTruthy(unlist(orcid_info$email$email))) {
        rv$electronicMailAddress <- orcid_info$email$email
        updateTextInput(session, "electronicMailAddress", value = rv$electronicMailAddress)
      }

      # fundings
      orcid_info$fundings <- orcid_fundings(orcid)[[orcid]]$group$`funding-summary`[[1]]
      if (isTruthy(unlist(orcid_info$fundings$`title.title.value`))) {
        rv$projectTitle <- orcid_info$fundings$`title.title.value`
        updateTextInput(session, "projectTitle", value = rv$projectTitle)
      }
      if (isTruthy(unlist(orcid_info$fundings$`organization.name`))) {
        rv$fundingAgency <- orcid_info$fundings$`organization.name`
        updateTextInput(session, "fundingAgency", value = rv$fundingAgency)
      }
      if (isTruthy(unlist(orcid_info$fundings$`put-code`))) {
        rv$fundingNumber <- orcid_info$fundings$`put-code`
        updateTextInput(session, "fundingAgency", value = rv$fundingAgency)
        # NOTE post Git issue concerning: 'EAL fundingNumber == ORCID::put-code'?
      }
    } else {
      message("Input 'userId' is not a valid ORCID.")
    }
  })

  observeEvent(input$role, {
    req(input$role)
    if (input$role == "(other)") {
      # custom role
      show("role-other")
      localRV$role <- input$`role-other`
      # project
      hide("project_information")
    } else if (input$role == "PI (principal investigator)") {
      # custom role
      hide("role-other")
      localRV$role <- input$`role`
      # project
      show("project_information")
    } else {
      # custom role
      hide("role-other")
      localRV$role <- input$role
      # project
      hide("project_information")
    }
  })

  # Project information -----------------------------------------------------
  observeEvent(input$projectTitle, {
    req(input$projectTitle)
    if (input$role == "PI (principal investigator") {
      localRV$projectTitle <- input$projectTitle
    }
  })

  observeEvent(input$fundingAgency, {
    req(input$fundingAgency)
    if (input$role == "PI (principal investigator") {
      localRV$fundingAgency <- input$fundingAgency
    }
  })

  observeEvent(input$fundingNumber, {
    req(input$fundingNumber)
    if (input$role == "PI (principal investigator") {
      localRV$fundingNumber <- input$fundingNumber
    }
  })

  # Metadata save -----------------------------------------------------
  observe({
    # Fetch correct index
    ind <- if (ref %in% rv$id) {
      match(ref, rv$id) # find its index
    }
    else if (ref > length(rv$id)) {
      length(rv$id) + 1 # set its index
    }
    else {
      NA
    }

    if (!is.na(ind)) {
      # print values into rv at selected index
      actualValues <- printReactiveValues(localRV)
      sapply(names(rv), function(rvid) {
        rv[[rvid]][ind] <- actualValues[rvid]
      })

      # print(ind)
    }
  })

  # Remove UI -----------------------------------------------------
  observeEvent(input[[rmv_id]],
    {
      # remove the UI
      removeUI(selector = paste0("#", site_id), immediate = TRUE)

      # unload the UI
      ind <- match(ref, rv$id)
      sapply(names(rv), function(rvid) {
        if (rvid == "id") { # keep length
          rv[[rvid]][[ind]] <<- -1 * rv[[rvid]][[ind]]
        } else {
          rv[[rvid]] <<- rv[[rvid]][-ind]
        }
      })

      # message("Removed UI !")
    },
    ignoreInit = TRUE,
    once = TRUE,
    autoDestroy = TRUE
  )

  # Output -----------------------------------------------------
  return(rv)
}

#' @title insertPersonnelInput
#' 
#' @description helper function to insert PersonnelMod* functions. Calling this from
#' a shiny server will insert PersonnelModUI and create its server part. Provided with
#' features to delete them.
insertPersonnelInput <- function(id, rv, ns, globals, role = NULL) {
  # NOTE warning: rv = rv$Personnel here !!!

  # initialize IDs -----------------------------------------------------
  div_id <- id
  site_id <- paste0("site_", div_id)
  rmv_id <- paste0("rmv_", div_id)

  # Proper module server -----------------------------------------------------
  # create the UI
  newUI <- PersonnelModUI(ns(div_id), site_id, rmv_id, role = role)

  # insert the UI
  insertUI(
    selector = paste0("#", ns("inserthere")),
    ui = newUI
  )

  # create the server
  rv <- callModule(PersonnelMod, id, globals, rv, rmv_id, site_id, div_id, role = role)

  # Output -----------------------------------------------------
  return(rv)
}
