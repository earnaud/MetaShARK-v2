#' @import shiny
#' @noRd
PersonnelUI <- function(id, main.env) {
  ns <- NS(id)

  return(
    fluidPage(
      fluidRow(
        column(
          2,
          actionButton(NS(id, "addui"), "", icon("plus"))
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
              fill the remaining fields.</p>")
        )
      ),
      tags$div(id = NS(id, "inserthere"))
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom shinyjs onclick enable disable
#' @importFrom data.table fwrite
#'
#' @noRd
Personnel <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Variable initialization ----
    observe({
      req(main.env$EAL$page == 7)
      main.env$EAL$page.load$depend()
      
      if (isContentTruthy(main.env$save.variable$SelectDP$dp.metadata.path)) {
        personnel.file <- dir(
          main.env$save.variable$SelectDP$dp.metadata.path,
          pattern = "ersonnel",
          full.names = TRUE
        )
        saved.table <- if (isTruthy(personnel.file)) {
          data.table::fread(
            personnel.file,
            data.table = FALSE,
            stringsAsFactors = FALSE
          )
        } else if (isTruthy(unlist(main.env$save.variable$Personnel))) {
          isolate(main.env$save.variable$Personnel)
        } else {
          NULL
        }
        saved.table[is.na(saved.table)] <- ""

        if (isContentTruthy(saved.table)) {
          saved.table$id <- c(
            saved.table$role[1:2],
            seq_along(saved.table$givenName)[-(1:2)]
          )
          isolate(main.env$local.rv$Personnel <- saved.table)

          sapply(main.env$local.rv$Personnel$id, function(rvid) {
            main.env$local.rv <- insertPersonnelInput(
              rvid,
              main.env$local.rv,
              ns,
              main.env,
              role = if (rvid %in% c("creator", "contact")) rvid,
              saved = saved.table
            )
            return()
          })
        } else { # New
          main.env$local.rv <- insertPersonnelInput(
            "creator",
            main.env$local.rv,
            ns,
            main.env,
            role = "creator",
            saved = saved.table
          )

          main.env$local.rv <- insertPersonnelInput(
            "contact",
            main.env$local.rv,
            ns,
            main.env,
            role = "contact",
            saved = saved.table
          )
        }
      }
    },
    label = "EAL7: set value"
    )

    # Fill Personnel ----
    observeEvent(input$addui, {
      id <- dim(main.env$local.rv$Personnel[-c(1:2), ])[1] + 1
      while (as.character(id) %in% main.env$local.rv$Personnel$id) {
        id <- id + 1
      }
      main.env$local.rv <- insertPersonnelInput(
        as.character(id),
        main.env$local.rv,
        ns,
        main.env
      )
    },
    label = "EAL7: add personnel UI")

    # Saves ----
    observe({
      req(main.env$EAL$page == 7)
      
      main.env$EAL$completed <- all(
        # Personnel
        isTruthy(main.env$local.rv$Personnel$givenName) &&
          isTruthy(main.env$local.rv$Personnel$surName) &&
          isTruthy(main.env$local.rv$Personnel$organizationName) &&
          isTruthy(main.env$local.rv$Personnel$electronicMailAddress) &&
          all(c("creator", "contact") %in% main.env$local.rv$Personnel$role)
      )
    },
    label = "EAL7: continuous save")

    # Process data (deprecated)
    # observeEvent(main.env$EAL$.next,
    #   {
    #     req(main.env$EAL$old.page == 7)
    #     
    #     saveReactive(main.env)
    #   },
    #   label = "EAL7: process data",
    #   priority = 1,
    #   ignoreInit = TRUE
    # )
  })
}
