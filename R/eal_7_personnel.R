#' @title PersonnelUI
PersonnelUI <- function(id, title, dev) {
  ns <- NS(id)
  
  return(
    fluidPage(
      fluidRow(
        column(2,
          actionButton(ns("addui"), "", icon("plus"))
        ),
        column(10,
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
    ) # end of fluidPage
  ) # end of return
}

#' @title Personnel
#'
#' @description server part of Personnel module
#'
#' @importFrom shiny reactiveValues callModule observeEvent observe isTruthy
#' @importFrom shinyjs onclick enable disable
#' @importFrom data.table fwrite
Personnel <- function(input, output, session, 
  savevar, globals, NSB) {
  ns <- session$ns
  
  if(globals$dev)
    onclick("dev", {
      req(globals$EMLAL$NAVIGATE == 7)
      browser()
    }, asis=TRUE)
  
  # Variable initialization -----------------------------------------------------
  rv <- reactiveValues(
    Personnel = data.frame(
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
      projectTitle = character(),
      fundingAgency = character(),
      fundingNumber = character(),
      stringsAsFactors = FALSE
    )
  )
  
  personnelFile <- dir(
    savevar$emlal$SelectDP$dp_metadata_path,
    pattern = "ersonnel",
    full.names = TRUE
  )
  saved_table <- if(isTruthy(personnelFile))
    fread(personnelFile, data.table = FALSE, stringsAsFactors = FALSE)
  else if(isTruthy(unlist(savevar$emlal$Personnel)))
    isolate(savevar$emlal$Personnel)
  else
    NULL
  saved_table[is.na(saved_table)] <- ""
  if(checkTruth(saved_table)){
    saved_table$id <- c(
      saved_table$role[1:2],
      seq_along(saved_table$givenName)[-(1:2)]
    )
    isolate(rv$Personnel <- saved_table)
    
    sapply(rv$Personnel$id, function(rvid){
      rv <- insertPersonnelInput(
        rvid,
        rv,
        ns,
        globals,
        role = if(rvid %in% c("creator", "contact")) rvid,
        saved = saved_table
      )
      return()
    })
  } else { # New
    rv <- insertPersonnelInput(
      "creator",
      rv,
      ns,
      globals,
      role = "creator",
      saved = saved_table
    )
    
    rv <- insertPersonnelInput(
      "contact",
      rv,
      ns,
      globals,
      role = "contact",
      saved = saved_table
    )
  }
  
  # Fill Personnel -----------------------------------------------------
  onclick("addui", {
    id <- dim(rv$Personnel[-c(1:2),])[1]+1
    while(as.character(id) %in% rv$Personnel$id){
      id <- id+1
    }
    rv <- insertPersonnelInput(
      as.character(id),
      rv,
      ns,
      globals
    )
  })
  
  # Saves -----------------------------------------------------
  observe({
    globals$EMLAL$COMPLETE_CURRENT <- all(
      # Personnel
      isTruthy(rv$Personnel$givenName) &&
        isTruthy(rv$Personnel$surName) &&
        isTruthy(rv$Personnel$organizationName) &&
        isTruthy(rv$Personnel$electronicMailAddress) &&
        all(c("creator", "contact") %in% rv$Personnel$role)
    )
  })
  
  observeEvent(NSB$SAVE, {
    req(globals$EMLAL$CURRENT == "Personnel")
    
    # save
    savevar <- saveReactive(
      savevar = savevar, 
      rv = list(Personnel = rv)
    )
  }, ignoreInit = TRUE)
  
  # Process data -----------------------------------------------------
  observeEvent(NSB$NEXT, {
    req(checkTruth(rv$Personnel))
    req(globals$EMLAL$CURRENT == "Personnel")
    
    savevar <- saveReactive(
      savevar, 
      rv = list(Personnel = rv)
    )
    showNotification(
      "Personnel has been written.",
      type = "message"
    )
  }, priority = 1, ignoreInit = TRUE)
  
  # Output -----------------------------------------------------
  return(savevar)
}

#' @title insertPersonnelInput
#'
#' @description helper function to insert PersonnelMod* functions. Calling this from
#' a shiny server will insert PersonnelModUI and create its server part. Provided with
#' features to delete them.
insertPersonnelInput <- function(id, rv, ns, globals, role = NULL, saved = NULL) {
  
  # initialize IDs -----------------------------------------------------
  div_id <- id
  site_id <- paste0("site_", id)
  rmv_id <- paste0("rmv_", id)
  
  # Proper module server -----------------------------------------------------
  # insert new UI
  newUI <- PersonnelModUI(
    ns(id), div_id, site_id, rmv_id, 
    role = role, saved = saved
  )
  insertUI(
    selector = paste0("#", ns("inserthere")),
    ui = newUI
  )
  
  # create associated server
  rv <- callModule(
    PersonnelMod, id, # module args
    globals, rv, # reactiveValues
    rmv_id, site_id, div_id, # renderUI ids
    role = role, saved = saved # set saved
  )
  
  # Output -----------------------------------------------------
  return(rv)
}

#' @title PersonnelModUI
#'
#' @description module to document EML Personnel
#'
#' @importFrom shinyBS bsTooltip
PersonnelModUI <- function(id, div_id, site_id, rmv_id, 
  role = NULL, saved = NULL) {
  ns <- NS(id)
  
  value <- if(checkTruth(saved)){
    saved[saved$id == div_id,]
  } else {
    NULL
  }
  
  piTag <- if(!is.null(role)) {
    shinyjs::hidden
  } else {
    shiny::tagList
  }
  
  tags$div(
    id = site_id,
    fluidRow(
      class = "inputBox",
      # Form -----------------------------------------------------
      # column(11,
      tagList(
        # * (ORCID) Personnel identification -----------------------------------------------------
        fluidRow(
          class = "topInputRow",
          column(3,
            if (is.null(role)) {
              selectInput(
                ns("role"),
                c("creator", "PI (principal investigator)", "contact", "(other)"),
                label = with_red_star("Role"),
                selected = if(!is.null(value)) {
                  if(value$role %in% c("creator", "PI (principal investigator)", "contact"))
                    value$role
                  else
                    "(other)"
                } else ""
              )
            } else {
              tags$b(paste("Role: ", role))
            }
          ),
          column(4,
            hidden(
              div(
                id = ns("role-other"),
                textInput(
                  ns("role-other"),
                  label = "Title of the custom role",
                  value = if(!is.null(value) &&
                      !value$role %in% c("creator", "PI (principal investigator)", "contact")) {
                    value$role 
                  } else ""
                )
              )
            )
          ),
          column(4,
            textInput(
              ns("userId"),
              label = "ORCID",
              value = if(!is.null(value)) value$userId else ""
            )
          ),
          column(1,
            if (is.null(role)) {
              actionButton(
                ns(rmv_id),
                "",
                icon("trash"),
                class = "danger"
              )
            },
            style = "padding-left: 0"
          )
        ), # end of fluidRow 1
        # * Basic identity -----------------------------------------------------
        fluidRow(
          style = "padding:5px",
          column(4,
            textInput(
              ns("givenName"),
              label = with_red_star("First name"),
              value = if(!is.null(value)) value$givenName else ""
            )
          ),
          column(4,
            textInput(
              ns("middleInitial"),
              label = "Middle initial",
              value = if(!is.null(value)) value$middleInitial else ""
            )
          ),
          column(4,
            textInput(
              ns("surName"),
              label = with_red_star("Last name"),
              value = if(!is.null(value)) value$surName else ""
            )
          )
        ), # end of fluidRow 1
        # * Contact -----------------------------------------------------
        fluidRow(
          style = "padding:5px",
          column(8,
            textInput(
              ns("organizationName"),
              label = with_red_star("Name of organization the person is associated with."),
              value = if(!is.null(value)) value$organizationName else ""
            )
          ),
          column(4,
            textInput(
              ns("electronicMailAddress"),
              label = with_red_star("Email address"),
              value = if(!is.null(value)) value$electronicMailAddress else ""
            )
          )
        ), # end of fluidRow 2
        # * Project information -----------------------------------------------------
        piTag(
          div(
            style = "padding:5px",
            id = "project_information",
            fluidRow(
              column(4,
                textInput(
                  ns("projectTitle"),
                  label = "Project title for this dataset",
                  value = if(!is.null(value)) value$projectTitle else ""
                )
              ),
              column(4,
                textInput(
                  ns("fundingAgency"),
                  label = "Entity funding the creation of this dataset",
                  value = if(!is.null(value)) value$fundingAgency else ""
                )
              ),
              column(4,
                textInput(
                  ns("fundingNumber"),
                  label = "Number of the grant or award that supported creation of this dataset",
                  value = if(!is.null(value)) value$fundingNumber else ""
                )
              )
            )
          )
        )
        # end of fluidRow 4
        # )
      )#,
      # Destroy -----------------------------------------------------
      # column(1,
      #   if (is.null(role)) {
      #     actionButton(
      #       ns(rmv_id),
      #       "",
      #       icon("trash"),
      #       class = "danger"
      #     )
      #   }
      # )
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
PersonnelMod <- function(input, output, session,globals,
  rv, rmv_id, site_id, ref, role = NULL, saved = NULL) {
  ns <- session$ns
  
  # Variable initialization -----------------------------------------------------
  if(!is.null(saved)){
      value <- saved[saved$id == ref,]
  } else {
    value <- NULL
  }
  
  localRV <- reactiveValues(
    id = ref,
    # Basic Identity
    givenName = if(!is.null(value)) value$givenName else character(),
    middleInitial = if(!is.null(value)) value$middleInitial else character(),
    surName = if(!is.null(value)) value$surName else character(),
    # Contact
    organizationName = if(!is.null(value)) value$organizationName else character(),
    electronicMailAddress = if(!is.null(value)) value$electronicMailAddress else character(),
    # Personnel information
    userId = if(!is.null(value)) value$userId else character(),
    role = if (!is.null(role)) role else if(!is.null(value)) value$role else character(),
    `role-other` = if (!is.null(role)) role else if(!is.null(value)) value$`role-other` else character(),
    # Project information
    projectTitle = if(!is.null(value)) value$projectTitle else NA,
    fundingAgency = if(!is.null(value)) value$fundingAgency else NA,
    fundingNumber = if(!is.null(value)) value$fundingNumber else NA
  )
  
  # * Basic Identity -----------------------------------------------------
  name.pattern <- globals$PATTERNS$NAME
  
  observeEvent(input$givenName, {
    localRV$givenName <- if (grepl(name.pattern, input$givenName))
      input$givenName
  }, )
  
  observeEvent(input$middleInitial, {
    localRV$middleInitial <- input$middleInitial
  })
  
  observeEvent(input$surName, {
    localRV$surName <- if (grepl(name.pattern, input$surName))
      input$surName
  })
  
  # * Contact -----------------------------------------------------
  mail.pattern <- globals$PATTERNS$EMAIL
  
  observeEvent(input$organizationName, {
    localRV$organizationName <- input$organizationName
  })
  
  observeEvent(input$electronicMailAddress, {
    localRV$electronicMailAddress <- if (grepl(mail.pattern, input$electronicMailAddress))
      input$electronicMailAddress
  })
  
  # * (ORCID) Personnel identification -----------------------------------------------------
  orcid.pattern <- globals$PATTERNS$ORCID
  
  observeEvent(input$userId, {
    req(input$userId)
    localRV$userId <- input$userId
    
    if (grepl(orcid.pattern, input$userId)) {
      localRV$userId <- str_extract(localRV$userId, orcid.pattern)
      updateTextInput(
        session = session, 
        inputId = "userId", 
        value = localRV$userId
      )
    }
    
    orcid.connect <- try(
      as.orcid(
        localRV$userId
      )
    )
    
    if (
      grepl(orcid.pattern, input$userId) &&
        isTruthy(orcid.connect)
    ) {
      orcid <- localRV$userId
      orcid_info <- list()
      
      # names
      orcid_info$names <- orcid_person(orcid)[[orcid]]$name
      if (isTruthy(unlist(orcid_info$names$`given-names`$value))) {
        localRV$givenName <- orcid_info$names$`given-names`$value
        updateTextInput(session, "givenName", value = localRV$givenName)
      }
      if (isTruthy(unlist(orcid_info$names$`family-name`$value))) {
        localRV$surName <- orcid_info$names$`family-name`$value
        updateTextInput(session, "surName", value = localRV$surName)
      }
      
      # organization
      orcid_info$employment <- orcid_employments(orcid)[[orcid]]$`affiliation-group`$summaries[[1]]
      if (isTruthy(unlist(orcid_info$employment$`employment-summary.organization.name`))) {
        localRV$organizationName <- orcid_info$employment$`employment-summary.organization.name`
        updateTextInput(session, "organizationName", value = localRV$organizationName)
      }
      if (is.null(role) &&
          isTruthy(unlist(orcid_info$employment$`employment-summary.role-title`))) {
        localRV$role <- "(other)"
        updateTextInput(session, "role", value = localRV$role)
        localRV$`role-other` <- orcid_info$employment$`employment-summary.role-title`
        updateTextInput(session, "role-other", value = localRV$`role-other`)
      }
      
      # email
      orcid_info$email <- orcid_email(orcid)[[orcid]]$email
      if (isTruthy(unlist(orcid_info$email$email))) {
        localRV$electronicMailAddress <- orcid_info$email$email
        updateTextInput(session, "electronicMailAddress", value = localRV$electronicMailAddress)
      }
      
      # fundings
      if(localRV$role == "PI (principal investigator)") {
        orcid_info$fundings <- orcid_fundings(orcid)[[orcid]]$group$`funding-summary`[[1]]
        if (isTruthy(unlist(orcid_info$fundings$`title.title.value`))) {
          localRV$projectTitle <- orcid_info$fundings$`title.title.value`
          updateTextInput(session, "projectTitle", value = localRV$projectTitle)
        }
        if (isTruthy(unlist(orcid_info$fundings$`organization.name`))) {
          localRV$fundingAgency <- orcid_info$fundings$`organization.name`
          updateTextInput(session, "fundingAgency", value = localRV$fundingAgency)
        }
        if (isTruthy(unlist(orcid_info$fundings$`put-code`))) {
          localRV$fundingNumber <- orcid_info$fundings$`put-code`
          updateTextInput(session, "fundingNumber", value = localRV$fundingNumber)
          # NOTE post Git issue concerning: 'EAL fundingNumber == ORCID::put-code'?
        }
      }
    } else {
      showNotification(
        id = ns("invalid_userid"),
        "Input 'userId' is not a valid ORCID.",
        type = "warning"
      )
    }
  })
  
  # * Project information -----------------------------------------------------
  if(is.null(role)){
    observeEvent({
      input$role
      input$`role-other`
    }, {
      if (input$role == "(other)") {
        # custom role
        show("role-other")
        localRV$role <- input$`role-other`
      } else {
        # custom role
        hide("role-other")
        localRV$role <- input$role
      }
      if (input$role == "PI (principal investigator)") {
        # project
        show("projectTitle")
        show("fundingAgency")
        show("fundingNumber")
      } else {
         # project
        hide("projectTitle")
        hide("fundingAgency")
        hide("fundingNumber")
      }
    }, ignoreInit = FALSE)
    
    observeEvent(input$projectTitle, {
      if (input$role == "PI (principal investigator)")
        localRV$projectTitle <- input$projectTitle
    })
    
    observeEvent(input$fundingAgency, {
      if (input$role == "PI (principal investigator)")
        localRV$fundingAgency <- input$fundingAgency
    })
    
    observeEvent(input$fundingNumber, {
      if (input$role == "PI (principal investigator)")
        localRV$fundingNumber <- input$fundingNumber
    })
  } else {
    localRV$role <- role
    localRV$projectTitle <- ""
    localRV$fundingAgency <- ""
    localRV$fundingNumber <- ""
  }
  
  # Metadata save -----------------------------------------------------
  observe({
    req(
      !is.null(role) ||
        (any(grepl(rmv_id, names(input))) &&
            input[[rmv_id]] < 1)
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
    localValues <- printReactiveValues(localRV)
    localValues <- localValues[colnames(personnel)]
    localValues[which(!sapply(localValues, isTruthy))] <- ""
    isolate(rv$Personnel[ind,] <- localValues)
  })
  
  # Remove UI -----------------------------------------------------
  if(is.null(role))
    onclick(rmv_id, {
      # unload the RV
      ind <- match(ref, rv$Personnel$id)
      rv$Personnel <- rv$Personnel %>% slice(-ind)
      
      # remove the UI
      removeUI(selector = paste0("#", site_id), immediate = TRUE)
    })
  
  # Output -----------------------------------------------------
  return(rv)
}