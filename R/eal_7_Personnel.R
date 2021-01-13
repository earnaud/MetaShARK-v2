#' @import shiny
#' 
#' @noRd
PersonnelUI <- function(id, main.env) {
  ns <- NS(id)

  return(
    fluidPage(
      tags$span(
        actionButton(NS(id, "addui"), "", icon("plus")),
        "At least one \"creator\" and \"contact\" are required. Filling the",
        tags$b("ORCID"), "field shall automatically fill the remaining fields."
      ),
      tags$div(id = "inserthere_eal7")
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#'
#' @noRd
Personnel <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    if (main.env$dev){
      observeEvent(
        main.env$dev.browse(), 
        {
          if (main.env$current.tab() == "fill" &&
              main.env$EAL$page == 7) {
            browser()
          }
        }
      )
    }

    # Setup UI on load
    observeEvent(main.env$EAL$page, { # on load
      req(main.env$EAL$old.page %in% c(0,6) && main.env$EAL$page == 7)
      if (isContentTruthy(main.env$local.rv$Personnel) && 
          nrow(main.env$local.rv$Personnel) > 0) {
        sapply(seq(nrow(main.env$local.rv$Personnel)), function(row.id) {
          insertPersonnelInput(
            session$ns(sprintf("_%s", row.id)),
            main.env
          )
        })
      }
    })
    
    # Fill Personnel ====
    # * Setup ----
    # Initial UI
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page == 7)
      req(nrow(main.env$local.rv$Personnel) > 0)
      
      sapply(seq(nrow(main.env$local.rv$Personnel)), function(ind) {
        row <- main.env$local.rv$Personnel[ind,]
        
        insertPersonnelInput(
          session$ns(row$id),
          main.env
        )
      })
    }, priority = -1)
    
    # User's additional UI
    observeEvent(input$addui, {
      insertPersonnelInput(
        session$ns(as.character(input$addui)),
        main.env
      ) 
    },
    label = "EAL7 add personnel UI"
    )

    # Saves ----
    observe({
      req(main.env$EAL$page == 7)
      invalidateLater(1000)
      
      # Roles
      .roles <- if(isTruthy(main.env$local.rv$Personnel$role))
        table(
          .tmp <- main.env$local.rv$Personnel$role %>%
            strsplit(., ",") %>%
            unlist
        ) else
          c()
      if(isFALSE("creator" %in% names(.roles))) 
        .roles <- c(.roles, "creator" = 0)
      if(isFALSE("contact" %in% names(.roles))) 
        .roles <- c(.roles, "contact" = 0)

      main.env$EAL$tag.list <- tagList(
        tags$b("Roles"),
        lapply(seq(.roles), function(role.index) {
          .ui <- tags$div(
            paste0(names(.roles)[role.index], ": ", .roles[role.index]),
            style = if(names(.roles)[role.index] %in% c("creator", "contact") &&
                       .roles[role.index] == 0)
              "color: red"
          )
        })
      )
      
      # Completeness
      main.env$EAL$completed <- all(
        # Personnel
        all(
          sapply(seq(nrow(main.env$local.rv$Personnel)), function(row.index){
            row <- main.env$local.rv$Personnel[row.index,]
            
            main.env$PATTERNS$name %grep% row$givenName &&
              main.env$PATTERNS$name %grep% row$surName &&
              isTruthy(row$organizationName) &&
              main.env$PATTERNS$email %grep% row$electronicMailAddress
          })
        ) &&
        # Required roles
          all(c("creator", "contact") %grep% main.env$local.rv$Personnel$role)
      )
    },
    label = "EAL7 set completed"
    )

    # Process data (deprecated)
  })
}
