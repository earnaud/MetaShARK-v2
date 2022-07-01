insertCustomLocationInput <- function(
    id, outer_id, default, selector = "#inserthere", local_rv, removable = TRUE) {
  # create the UI
  new_ui <- customLocationInput_UI(id, default = default, removable = removable)
  # insert the UI
  insertUI(selector, ui = new_ui, immediate = TRUE)
  # create the server
  customLocationInput(unns(id),
    outer_id = outer_id, local_rv,
    removable = removable
  )
}

# Coordinate Input UI ====
customLocationInput_UI <- function(id, default, removable) {
  ns <- NS(id)

  tags$div(
    id = ns("box"),
    coordinateInput_UI(ns("coordinate"), default),
    if (isTRUE(removable)) {
      actionLink(ns("rmv"), "", icon("times"))
    },
    style = "display: flex; align-items: baseline;"
  )
}

# Coordinate Input Server ====
customLocationInput <- function(id, outer_id, local_rv, removable = TRUE) {
  moduleServer(id, function(input, output, session) {
    # grab values from inserted module
    rv <- coordinateInput("coordinate")

    # get values ----
    observeEvent(rv(), {
      req(isContentTruthy(rv()))

      .id <- id # used for dplyr

      ## either create .. ----
      if (!(.id %in% as.character(local_rv$custom[[outer_id]]$points$id))) {
        local_rv$custom[[outer_id]]$points <- rbind(
          local_rv$custom[[outer_id]]$points,
          data.frame(
            id = as.double(.id),
            lat = rv()$lat,
            lon = rv()$lon
          )
        )
      } else { ## .. or change ----
        local_rv$custom[[outer_id]]$points[
          local_rv$custom[[outer_id]]$points$id == .id,
        ] <- local_rv$custom[[outer_id]]$points[
          local_rv$custom[[outer_id]]$points$id == .id,
        ] |>
          mutate(lat = rv()$lat) |>
          mutate(lon = rv()$lon)
      }
    })

    # remove point ----
    if (isTRUE(removable)) { # no need for pointless observer
      observeEvent(input$rmv, {
        devmsg("Removing #%s", session$ns("box"), tag = "customLocationInput.R")
        # remove UI
        removeUI(selector = paste0("#", session$ns("box")), immediate = TRUE)
        # remove data
        .ind <- which(local_rv$custom[[outer_id]]$points$id == id)
        local_rv$custom[[outer_id]]$points <- local_rv$custom[[outer_id]]$
          points[-.ind, ]
      })
    }
  })
}
