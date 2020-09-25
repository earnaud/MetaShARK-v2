#' @import shiny
#'
#' @noRd
insertPersonnelInput <- function(id, main.env) {
  # Add row
  .id <- unns(id)
  if(grepl("^_", .id)) {
    # already set local table
  } else { 
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

#' @importFrom shinyBS bsTooltip
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
#' @importFrom rorcid as.orcid orcid_person orcid_employments orcid_email orcid_fundings
#' @importFrom stringr str_extract
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
    # TODO modulify orcid with SNAKE
    orcidInput("orcid", main.env, row = row)
    
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
    
    # observe({
    #   shinyjs::toggle(
    #     "PI", TRUE,
    #     condition = "PI" %grep% main.env$local.rv$Personnel$role[row()]
    #   )
    # })
    
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

# .old ====
# PersonnelInputUI <- function(id, main.env) {
#   # Set variables
#   value <- if (isContentTruthy(main.env$local.rv) && 
#                ref %in% main.env$local.rv$id)
#     main.env$local.rv[main.env$local.rv$id == ref,] else 
#       NULL
#   ref <- unns(id)
#   
#   # Form
#   ui <- tags$div(
#     id = NS(id, "person"),
#     fluidRow(
#       class = "inputBox",
#       tagList(
#         # * (ORCID) Personnel identification
#         fluidRow(
#           class = "topInputRow",
#           column(
#             11,
#             column(
#               4,
#               selectInput(
#                 NS(id, "role"),
#                 c("creator", "PI (principal investigator)", "contact", "(other)"),
#                 label = "Role",
#                 selected = if (!isTruthy(value))
#                   "" else if (value$role %in% c("creator", "PI (principal investigator)", "contact"))
#                     value$role else
#                       "(other)"
#               )
#             ),
#             column(
#               4,
#               shinyjs::hidden(
#                 div(
#                   id = NS(id, "role-other"),
#                   textInput(
#                     NS(id, "role-other"),
#                     label = "Title of the custom role",
#                     value = if (
#                       !is.null(value) && 
#                       !value$role %in% c(
#                         "creator", "PI (principal investigator)", 
#                         "contact"
#                       )
#                     )
#                       value$role
#                     else
#                       ""
#                   )
#                 )
#               )
#             ),
#             column(
#               4,
#               textInput(
#                 NS(id, "userId"),
#                 label = "ORCID",
#                 value = if (!is.null(value)) value$userId else ""
#               )
#             )
#           ),
#           # * Remove
#           column(
#             1,
#             if (is.null(role)) {
#               actionButton(
#                 NS(id, "remove"),
#                 "",
#                 icon("trash"),
#                 class = "danger"
#               )
#             },
#             style = "padding-left: 0"
#           )
#         ), # end of fluidRow 1
#         # * Basic identity
#         fluidRow(
#           style = "padding:5px",
#           column(
#             4,
#             textInput(
#               NS(id, "givenName"),
#               label = "First name",
#               value = if (!is.null(value)) value$givenName else ""
#             )
#           ),
#           column(
#             4,
#             textInput(
#               NS(id, "middleInitial"),
#               label = "Middle initial",
#               value = if (!is.null(value)) value$middleInitial else ""
#             )
#           ),
#           column(
#             4,
#             textInput(
#               NS(id, "surName"),
#               label = "Last name",
#               value = if (!is.null(value)) value$surName else ""
#             )
#           )
#         ), # end of fluidRow 1
#         # * Contact
#         fluidRow(
#           style = "padding:5px",
#           column(
#             8,
#             textInput(
#               NS(id, "organizationName"),
#               label = "Name of organization the person is associated with.",
#               value = if (!is.null(value)) value$organizationName else ""
#             )
#           ),
#           column(
#             4,
#             textInput(
#               NS(id, "electronicMailAddress"),
#               label = "Email address",
#               value = if (!is.null(value)) value$electronicMailAddress else ""
#             )
#           )
#         ), # end of fluidRow 2
#         # * Project information
#         div(
#           style = "padding:5px",
#           id = NS(id, "project_information"),
#           fluidRow(
#             column(
#               4,
#               textInput(
#                 NS(id, "projectTitle"),
#                 label = "Project title for this dataset",
#                 value = if (!is.null(value)) value$projectTitle else ""
#               )
#             ),
#             column(
#               4,
#               textInput(
#                 NS(id, "fundingAgency"),
#                 label = "Entity funding the creation of this dataset",
#                 value = if (!is.null(value)) value$fundingAgency else ""
#               )
#             ),
#             column(
#               4,
#               textInput(
#                 NS(id, "fundingNumber"),
#                 label = "Number of the grant or award that supported creation of this dataset",
#                 value = if (!is.null(value)) value$fundingNumber else ""
#               )
#             )
#           )
#         ) # end of project information
#       )
#     )
#   ) # end of module div
#   
#   # Output
#   return(ui)
# }
# 
# 
# PersonnelInput <- function(id, main.env, ref) {
#   # id, main.env, rv, rmv.id, site.id, ref, role = NULL, default = NULL
#   moduleServer(id, function(input, output, session) {
#     # Variable initialization
#     value <- if (isContentTruthy(main.env$local.rv))
#       main.env$local.rv[main.env$local.rv$id == ref, ] else
#         NULL
#     role <- if (ref %in% c("creator", "contact")) ref else NULL
#     
#     local.rv <- reactiveValues(
#       id = ref,
#       # Basic Identity
#       givenName = if (!is.null(value)) value$givenName else character(),
#       middleInitial = if (!is.null(value)) value$middleInitial else character(),
#       surName = if (!is.null(value)) value$surName else character(),
#       # Contact
#       organizationName = if (!is.null(value)) value$organizationName else character(),
#       electronicMailAddress = if (!is.null(value)) value$electronicMailAddress else character(),
#       # Personnel information
#       userId = if (!is.null(value)) value$userId else character(),
#       role = if (!is.null(role)) role else if (!is.null(value)) value$role else character(),
#       `role-other` = if (!is.null(role)) role else if (!is.null(value)) value$`role-other` else character(),
#       # Project information
#       projectTitle = if (!is.null(value)) value$projectTitle else NA,
#       fundingAgency = if (!is.null(value)) value$fundingAgency else NA,
#       fundingNumber = if (!is.null(value)) value$fundingNumber else NA
#     )
#     
#     # * Basic Identity
#     name.pattern <- main.env$PATTERNS$name
#     
#     # Given name
#     observeEvent(input$givenName, {
#       shinyFeedback::hideFeedback("givenName")
#       local.rv$givenName <- input$givenName
#       
#       if (name.pattern %grep% input$givenName) {
#         shinyFeedback::showFeedbackSuccess("givenName")
#       } else {
#         shinyFeedback::showFeedbackDanger("givenName", text = "")
#       }
#     },
#     ignoreNULL = FALSE,
#     label = session$ns("givenName")
#     )
#     
#     # Middle Initial
#     observeEvent(input$middleInitial, {
#       shinyFeedback::hideFeedback("middleInitial")
#       local.rv$middleInitial <- input$middleInitial
#       
#       if (isTruthy(input$middleInitial) && nchar(input$middleInitial) > 0) {
#         shinyFeedback::showFeedbackSuccess("middleInitial")
#       } else {
#         shinyFeedback::showFeedbackDanger("middleInitial", text = "")
#       }
#     },
#     ignoreNULL = FALSE,
#     label = session$ns("middleInitial")
#     )
#     
#     # Last name
#     observeEvent(input$surName, {
#       shinyFeedback::hideFeedback("surName")
#       local.rv$surName <- input$surName
#       
#       if (name.pattern %grep% input$surName) {
#         shinyFeedback::showFeedbackSuccess("surName")
#       } else {
#         shinyFeedback::showFeedbackDanger("surName", text = "")
#       }
#     },
#     ignoreNULL = FALSE,
#     label = session$ns("surName")
#     )
#     
#     # * Contact
#     mail.pattern <- main.env$PATTERNS$email
#     
#     # Organization name
#     observeEvent(input$organizationName, {
#       shinyFeedback::hideFeedback("organizationName")
#       local.rv$organizationName <- input$organizationName
#       
#       if (name.pattern %grep% input$organizationName) {
#         shinyFeedback::showFeedbackSuccess("organizationName")
#       } else {
#         shinyFeedback::showFeedbackDanger("organizationName", text = "")
#       }
#     },
#     ignoreNULL = FALSE,
#     label = session$ns("organizationName")
#     )
#     
#     # Email address
#     observeEvent(input$electronicMailAddress, {
#       shinyFeedback::hideFeedback("electronicMailAddress")
#       local.rv$electronicMailAddress <- input$electronicMailAddress
#       
#       if (mail.pattern %grep% input$electronicMailAddress) {
#         shinyFeedback::showFeedbackSuccess("electronicMailAddress")
#       } else {
#         shinyFeedback::showFeedbackDanger("electronicMailAddress", text = "")
#       }
#     },
#     ignoreNULL = FALSE,
#     label = session$ns("electronicMailAddress")
#     )
#     
#     # * (ORCID) Personnel identification
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
#     
#     # * Project information
#     if (is.null(role)) {
#       observeEvent(
#         {
#           input$role
#           input$`role-other`
#         },
#         {
#           # custom role
#           shinyjs::toggle(
#             "role-other", 
#             condition = input$role == "(other)"
#           )
#           local.rv$role <- if (input$role == "(other)")
#             input$`role-other` else
#               input$role
#           # Feedback
#           shinyFeedback::hideFeedback("role-other")
#           if (isTruthy(input$`role-other`))
#             shinyFeedback::showFeedbackSuccess("role-other") else
#               shinyFeedback::showFeedbackDanger("role-other")
#           # project
#           sapply(
#             c("projectTitle", "fundingAgency", "fundingNumber"),
#             shinyjs::toggle,
#             condition = input$role == "PI (principal investigator)"
#           )
#           # if (input$role == "PI (principal investigator)") {
#           #   shinyjs::show("projectTitle")
#           #   shinyjs::show("fundingAgency")
#           #   shinyjs::show("fundingNumber")
#           # } else {
#           #   shinyjs::hide("projectTitle")
#           #   shinyjs::hide("fundingAgency")
#           #   shinyjs::hide("fundingNumber")
#           # }
#         },
#         ignoreInit = FALSE,
#         ignoreNULL = FALSE,
#         label = session$ns("role")
#       )
#       
#       # Project title
#       observeEvent(input$projectTitle, {
#         shinyFeedback::hideFeedback("projectTitle")
#         local.rv$projectTitle <- input$projectTitle
#         
#         if (input$role == "PI (principal investigator)" &&
#             isTruthy(local.rv$projectTitle))
#           shinyFeedback::showFeedbackSuccess("projectTitle") else
#             shinyFeedback::showFeedbackDanger("projectTitle", text = "")
#       },
#       ignoreNULL = FALSE,
#       label = session$ns("projectTitle")
#       )
#       
#       # Funding agency
#       observeEvent(input$fundingAgency, {
#         shinyFeedback::hideFeedback("fundingAgency")
#         local.rv$fundingAgency <- input$fundingAgency
#           
#         if (input$role == "PI (principal investigator)" &&
#             isTruthy(local.rv$fundingAgency))
#           shinyFeedback::showFeedbackSuccess("fundingAgency") else
#             shinyFeedback::showFeedbackDanger("fundingAgency", text = "")
#       },
#       ignoreNULL = FALSE,
#       label = session$ns("fundingAgency")
#       )
#       
#       # Funding number
#       observeEvent(input$fundingNumber, {
#         shinyFeedback::hideFeedback("fundingNumber")
#         local.rv$fundingNumber <- input$fundingNumber
#         
#         if (input$role == "PI (principal investigator)" &&
#             isTruthy(local.rv$fundingNumber))
#           shinyFeedback::showFeedbackSuccess("fundingNumber") else
#             shinyFeedback::showFeedbackDanger("fundingNumber", text = "")
#       },
#       ignoreNULL = FALSE,
#       label = session$ns("fundingNumber")
#       )
#     } else { 
#       local.rv$role <- role # Role is either 'creator' or 'contact'
#       local.rv$projectTitle <- ""
#       local.rv$fundingAgency <- ""
#       local.rv$fundingNumber <- ""
#     }
#     
#     # Metadata save
#     observe({
#       req(!is.null(role) || ("remove" %grep% names(input) && input$remove < 1))
#       personnel <- isolate(main.env$local.rv)
#       # Fetch correct index
#       ind <- if (ref %in% personnel$id) {
#         match(ref, personnel$id) # find its index
#       } else {
#         dim(personnel)[1] + 1
#       }
#       
#       # print values into rv at selected index
#       .values <- printReactiveValues(local.rv)
#       .values <- .values[colnames(personnel)]
#       .values[which(!sapply(.values, isTruthy))] <- ""
#       isolate(main.env$local.rv[ind, ] <- .values)
#     })
#     
#     # Remove UI
#     if (is.null(role)) {
#       observeEvent(input$remove, {
#         # unload the RV
#         ind <- match(ref, main.env$local.rv$id)
#         main.env$local.rv <- main.env$local.rv %>% slice(-ind)
#         
#         # remove the UI
#         removeUI(
#           selector = paste0("#", session$ns("person")),
#           immediate = TRUE
#         )
#       })
#     }
#   })
# }
