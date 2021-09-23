insertCustomLocationInput <- function(
  id, outer.id, default, selector = "#inserthere", main.env
) {
  # create the UI
  new.ui <- customLocationInput_UI(id, default = default)
  # insert the UI
  insertUI(selector, ui = new.ui, immediate = TRUE)
  # create the server
  customLocationInput(unns(id), outer.id = outer.id, main.env)
}

# Coordinate Input UI ====
customLocationInput_UI <- function(id, default) {
  ns <- NS(id)
  
  tags$div(
    id = ns("box"),
    coordinateInput_UI(ns("coordinate"), default),
    if(as.numeric(unns(id)) > 3)
      actionLink(ns("rmv"), "", icon("times")),
    style = "display: flex; align-items: baseline;"
  )
}

# Coordinate Input Server ====
customLocationInput <- function(id, outer.id, main.env) {
  moduleServer(id, function(input, output, session) {
    # grab values from inserted module
    rv <- coordinateInput("coordinate")
    
    # get values ----
    observeEvent(rv(), {
      req(isContentTruthy(rv()))
      
      .id <- id # used for dplyr
      
      ## either create .. ----
      if(isFALSE(.id %in% as.character(main.env$custom[[outer.id]]$points$id))){
        main.env$custom[[outer.id]]$points <- rbind(
          main.env$custom[[outer.id]]$points,
          data.frame(
            id = as.double(.id), 
            lat = rv()$lat, 
            lon = rv()$lon
          )
        )
      } else { ## .. or change ----
        main.env$custom[[outer.id]]$points[
          main.env$custom[[outer.id]]$points$id == .id,
        ] <- main.env$custom[[outer.id]]$points[
          main.env$custom[[outer.id]]$points$id == .id,
        ] |> 
          mutate(lat = rv()$lat) |>
          mutate(lon = rv()$lon)
      }
    })
    
    # remove ----
    observeEvent(input$rmv, {
      message(paste0("#", NS(outer.id, NS(id, "box"))))
      # remove UI
      removeUI(selector = paste0("#", NS(outer.id, NS(id, "box"))), immediate = TRUE)
      # remove data
      .ind <- which(main.env$custom[[outer.id]]$points$id == id)
      main.env$custom[[outer.id]]$points <- main.env$custom[[outer.id]]$points[-.ind,]
    })
  })
}