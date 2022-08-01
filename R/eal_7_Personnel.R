#' @import shiny
#'
#' @noRd
PersonnelUI <- function(id) {
  ns <- NS(id)

  return(
    fluidPage(
      tags$span(
        actionButton(ns("addui"), "", icon("plus")),
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
Personnel <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    if (main_env$dev) .browse_dev(main_env, 7, input, output, session)

    # Fill Personnel ====
    ## Setup ----
    # Initial UI
    # observeEvent(main_env$EAL$page, {
    #   req(main_env$EAL$page == 7)
    #   req(nrow(main_env$local_rv$Personnel) > 0)
    # 
    #   sapply(seq_row(main_env$local_rv$Personnel), function(ind) {
    #     row <- main_env$local_rv$Personnel[ind, ]
    # 
    #     insertPersonnelInput(
    #       session$ns(row$id),
    #       main_env
    #     )
    #   })
    # },
    # priority = -1
    # )

    # Remove UI after step
    # different priority
    observeEvent(main_env$EAL$page, {
      if (main_env$EAL$old_page == 7) {
        sapply(seq_row(main_env$local_rv$Personnel), function(ind) {
          sapply(
            paste0(main_env$local_rv$Personnel$id, "-container"),
            function(id) {
              removeUI(sprintf("#%s", session$ns(id)), immediate = TRUE)
            }
          )
        })
      }
    },
    priority = 1
    )

    # Add form ====
    # User's additional UI
    observeEvent(input$addui, {
      req(main_env$EAL$page == 7)
      # Add row to the table
      .id <- input$addui
      main_env$local_rv$Personnel[nrow(main_env$local_rv$Personnel) + 1, ] <-
        rep("", ncol(main_env$local_rv$Personnel))
      main_env$local_rv$Personnel[nrow(main_env$local_rv$Personnel), "id"] <- .id
      # Add module to the GUI
      insertModule(
        "Personnel",
        list(
          ui = session$ns(input$addui),
          server = as.character(input$addui)
        ),
        main_env
      )
      # insertPersonnelInput(
      #   session$ns(as.character(input$addui)),
      #   main_env
      # )
    },
    label = "EAL7 add personnel UI"
    )

    # Saves ----
    observe({
      req(main_env$EAL$page == 7)
      invalidateLater(1000)

      # Roles
      .roles <- if (isTruthy(main_env$local_rv$Personnel$role)) {
        table(
          .tmp <- main_env$local_rv$Personnel$role |>
            strsplit(split = ",") |>
            unlist()
        )
      } else {
        c()
      }
      if (isFALSE("creator" %in% names(.roles))) {
        .roles <- c(.roles, "creator" = 0)
      }
      if (isFALSE("contact" %in% names(.roles))) {
        .roles <- c(.roles, "contact" = 0)
      }

      main_env$EAL$tag_list <- tagList(
        tags$b("Roles"),
        lapply(seq(.roles), function(role_index) {
          .ui <- tags$div(
            paste0(names(.roles)[role_index], ": ", .roles[role_index]),
            style = if (
              names(.roles)[role_index] %in% c("creator", "contact") &&
              .roles[role_index] == 0) {
              "color: red"
            }
          )
        })
      )

      # Completeness
      main_env$EAL$completed <- all(
        # Personnel
        all(
          sapply(seq_row(main_env$local_rv$Personnel), function(row.index) {
            row <- main_env$local_rv$Personnel[row.index, ]

            main_env$PATTERNS$name %grep% row$givenName &&
              main_env$PATTERNS$name %grep% row$surName &&
              isTruthy(row$organizationName) &&
              main_env$PATTERNS$email %grep% row$electronicMailAddress
          })
        ) &&
          # Required roles
          all(c("creator", "contact") %grep% main_env$local_rv$Personnel$role)
      )
    },
    label = "EAL7 set completed"
    )
  })
}
