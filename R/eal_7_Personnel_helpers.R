#' @import shiny
#'
#' @noRd
insertPersonnelInput <- function(id, main.env) {
  # Add row
  .id <- unns(id)
  if(!grepl("^_", .id)) {
    # add a new row to local table
    main.env$local.rv$Personnel[nrow(main.env$local.rv$Personnel)+1,] <- 
      c(rep("", ncol(main.env$local.rv$Personnel)-1), id = .id)
  }
  # create the UI
  new.ui <- PersonnelInputUI(id, main.env)
  # insert the UI
  insertUI(selector = "#inserthere_eal7", ui = new.ui, immediate = TRUE)
  # create the server
  PersonnelInput(.id, main.env)
}

#' @import shiny
#' @importFrom shinyjs hidden
#' @importFrom tippy tippy
#'
#' @noRd
PersonnelInputUI <- function(id, main.env) {
  # Setup
  ref <- unns(id)
  .value <- main.env$local.rv$Personnel[
    which(main.env$local.rv$Personnel$id == ref),
  ]
  
  # Form ====
  ui <- tags$div(
    id = NS(id, "container"),
    class = "inputBox",
    # * Header ----
    tags$div(
      class = "topInputRow",
      # Collapse
      actionLink(NS(id, "collapse"), "", icon("chevron-right")),
      tags$span(
        style="width: calc(100% - 100px); margin: 0 5px 0;",
        # Role
        roleInputUI(
          NS(id, "role"),
          main.env$local.rv$role.choices,
          val = .value$role,
          width = "50%"
        ),
        # Show name
        tags$div(
          uiOutput(NS(id, "name")),
          style="margin-top: 20px; padding: 6px; height: 40px; width: 50%;"
        )
      ),
      # Remove UI
      actionButton(NS(id, "remove"), "", icon("trash"), class = "redButton")
    ), # end of header
    shinyjs::hidden(
      tags$div(
        id = NS(id, "content"),
        class = "contentRow",
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
              label = tippy::tippy("MI","Middle Initial"),
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
      )
    ) # end of content
  )
  
  # Output ====
  return(ui)
}

#' @import shiny
#' @importFrom shinyjs toggle show hide
#' @importFrom dplyr %>%
#'
#' @noRd
PersonnelInput <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Setup ----
    row <- reactive({which(main.env$local.rv$Personnel$id == id)})
    name.pattern <- main.env$PATTERNS$name
    orcid.pattern <- main.env$PATTERNS$ORCID
    mail.pattern <- main.env$PATTERNS$email
    
    # [U] Collapse ====
    observeEvent(input$collapse, {
      shinyjs::toggle(
        id = "content",
        anim = TRUE,
        animType = "slide",
        time = 0.25,
        condition = input$collapse %% 2 == 1
      )
      
      updateActionButton(
        session, 
        "collapse", 
        icon = icon(
          ifelse(input$collapse %% 2 == 0, "chevron-right", "chevron-down")
        )
      )
    })
    
    # [I] Role ====
    roleInput("role", main.env, row = row)
    
    # Toggle PI fields
    observeEvent(input$`role-role`, {
      req(main.env$EAL$page == 7)
      #FIXME toggle bugs here, no idea why      
      if("PI" %grep% main.env$local.rv$Personnel$role[row()])
        shinyjs::show("PI") else
          shinyjs::hide("PI")
    },
    priority = -1,
    ignoreNULL = FALSE,
    label = "EAL7 toggle PI"
    )
    
    # [U] Verbose name ====
    output$name <- renderUI({
      
      .name <- paste(
        main.env$local.rv$Personnel$givenName[row()],
        main.env$local.rv$Personnel$middleInitial[row()],
        main.env$local.rv$Personnel$surName[row()],
        collapse = " "
      ) %>% gsub(" +", " ", x = .) %>% gsub("^ +$", "", x = .)
      
      validate(
        need(.name != "", "No provided name")
      )
      
      return(.name)
    })
    
    # [U] Remove ====
    observeEvent(input$remove, {
      # remove the UI
      removeUI(selector = paste0("#", session$ns("container")), immediate = TRUE)
      # remove data
      main.env$local.rv$Personnel <- main.env$local.rv$Personnel[-row(),]
    })
    
    # [I] ORCID ====
    orcidInput("orcid", main.env, row = row)
    observeEvent(input[["orcid-orcid"]], {
      .value <- input[["orcid-orcid"]]
      
      req(main.env$PATTERNS$ORCID %grep% .value)
      
      # Get ORCID record
      result <- httr::GET(
        sprintf("https://pub.orcid.org/v3.0/%s", .value),
        httr::add_headers(Accept = "application/json")
      )
      result$content <- result$content %>% 
        rawToChar() %>% 
        jsonlite::fromJSON()
      
      # Update inputs with orcid record
      given.name <- try(result$content$person$name$`given-names`$value)
      if(class(given.name) != "try-error") {
        updateTextInput(session, "first_name", value = given.name)
      }
      
      last.name <- try(result$content$person$name$`family-name`$value)
      if(class(last.name) != "try-error") {
        updateTextInput(session, "last_name", value = last.name)
      }
      
      email <- try(result$content$person$emails$email$email)
      if(class(email) != "try-error") {
        updateTextInput(session, "email", value = email)
      }
      
      organization.name <- try(result$content$`activities-summary`$employments$
                                 `affiliation-group`$summaries[[1]]$`employment-summary`$organization$
                                 name)
      if(class(organization.name) != "try-error") {
        updateTextInput(session, "organization", value = organization.name)
      }
    })
    
    # [I] Names ====
    # * First name ----
    observeEvent(input$first_name, {
      main.env$local.rv$Personnel$givenName[row()] <- input$first_name
      checkFeedback(input, "first_name", name.pattern %grep% input$first_name)
    })
    
    # * Middle initial ----
    observeEvent(input$middle_initial, {
      main.env$local.rv$Personnel$middleInitial[row()] <- input$middle_initial
    })
    
    # * Last name ----
    observeEvent(input$last_name, {
      main.env$local.rv$Personnel$surName[row()] <- input$last_name
      checkFeedback(input, "last_name", name.pattern %grep% input$last_name)
    })
    
    # [I] Email ====
    observeEvent(input$email, {
      main.env$local.rv$Personnel$electronicMailAddress[row()] <- input$email
      checkFeedback(input, "email", mail.pattern %grep% input$email)
    })
    
    # [I] Organization ====
    observeEvent(input$organization, {
      main.env$local.rv$Personnel$organizationName[row()] <- input$organization
      checkFeedback(input, "organization")
    })
    
    # [I] PI fields ====
    
    # * Project title ----
    observeEvent(input$project_title, {
      main.env$local.rv$Personnel$projectTitle[row()] <- input$project_title
    })
    
    # * Funding agency ----
    observeEvent(input$funding_agency, {
      main.env$local.rv$Personnel$fundingAgency[row()] <- input$funding_agency
    })
    
    # * Funding number ----
    observeEvent(input$funding_number, {
      main.env$local.rv$Personnel$fundingNumber[row()] <- input$funding_number
    })
    
    # (End) ====
  })
}