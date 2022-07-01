MetaFINUI <- function(id, wip) {
  ns <- NS(id)

  if (wip) {
    wipRow(
      fileInput(
        ns("file"),
        "Select a valid EML file (.xml)",
        accept = "application/xml",
        width = "50%"
      ),
      shinyjs::hidden(
        tags$div(
          id = ns("content_area"),
          actionButton(ns("dev"), "DEV"),
          actionButton(
            ns("validate"),
            "Validate",
            icon = icon("check")
          ),
          downloadButton(
            ns("save"),
            "Save",
            icon = icon("save"),
            width = "100%"
          ),
          tags$hr(),
          fluidRow(
            column(
              4,
              shinyTree(
                ns("tree"),
                search = TRUE,
                searchtime = 1000
              ),
              style = "overflow-y: scroll;
                 max-height: 650px"
            ),
            column(
              8,
              tags$div(id = "inserthere_mf_form"),
              collapsibleUI(
                ns("attributes"),
                "Attributes",
                tagList(
                  tags$div(id = "inserthere_mf_attributes")
                )
              ) |>
                shinyjs::hidden()
            )
          )
        ) # end content_area
      )
    )
  } else {
    tags$h3("MetaFIN: WIP")
  }
}

MetaFIN <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    # Not in main_env$local_rv (reserved to EAL fill-in)
    local_rv <- reactiveValues(
      eml.list = list(),
      observers = reactiveValues(
        id = character(), # value of local_rv$count when observer created
        inputID = character(), # inputId observed
        path = character(), # matching path in local_rv$eml.list
        obs = c() # observer itself
      ),
      count = 0
    )

    # File input ====
    xml.path <- reactive({
      req(input$file)
      input$file$datapath
    })

    observe({
      shinyjs::toggle("content_area", condition = isContentTruthy(xml.path()))
    })

    # Resources ====
    # XXX : name for XML doc variable
    # xxx : name for XML node variable

    # * read eml file ----
    EML <- reactive({
      req(xml.path())

      return(XML::xmlParse(xml.path()))
    })

    # * save original eml ----
    EML.save <- reactive({
      req(EML())

      return(XML::xmlClone(EML()))
    })

    # * make root node ----
    eml.root <- reactive({
      req(EML())

      return(XML::xmlRoot(EML()))
    })

    # * make eml as list and RV ----
    observe({
      req(eml.root())

      .out <- XML::xmlToList(eml.root())
      local_rv$eml.list <<- renameList(.out)

      # .out <- listToReactiveValues(eml.list)
      # eml.rv <<- .out
    })

    # * isolate root node ----
    # used to rewrite EML on save
    eml.back <- reactive({
      req(eml.root())

      return(
        XML::xmlClone(eml.root()) %>%
          XML::removeChildren("dataset")
      )
    })

    # * render tree ----
    eml.shinyTree <- reactive({
      validate(
        need(isContentTruthy(local_rv$eml.list), "no eml.list provided")
      )

      listToStructure(local_rv$eml.list)
    })

    output$tree <- renderTree({
      eml.shinyTree()
    })

    # * dev ----
    observeEvent(input$dev, {
      browser()
    })

    # Use tree ====

    # * get content ----
    content <- eventReactive(input$tree, {
      req(isContentTruthy(get_selected(input$tree)))
      devmsg(tag = "MetaFIN", "content()")

      .path <- paste(
        c(
          attr(get_selected(input$tree)[[1]], "ancestry"),
          get_selected(input$tree)[[1]][1]
        ),
        collapse = "/",
        sep = "/"
      )
      # content -- tricks in followPath: same path, different tree
      .content <- followPath(local_rv$eml.list, .path)
      # rename content if unnamed
      if (length(.content) == 1 && is.null(names(.content))) {
        names(.content) <- strsplit(.path, split = "/")[[1]] |> tail(1)
      }

      return(.content)
    })

    # * output form ----
    # check if content() children are to be rendered as UI (depth == 0)
    torender.ui <- reactive({
      req(content())
      devmsg(tag = "MetaFIN", "torender.ui()")

      names(content())[sapply(content(), depth) == 0]
    })

    # check if content() children are to be rendered as links (depth > 0)
    torender.links <- reactive({
      req(content())
      devmsg(tag = "MetaFIN", "torender.links()")

      names(content())[sapply(content(), depth) > 0]
    })

    # * insert forms ----
    observeEvent(
      {
        torender.ui()
        torender.links()
      },
      {
        devmsg(tag = "MetaFIN", "OE insert forms")
        # Remove previous UIs
        if (length(local_rv$observers$inputID) > 0) {
          sapply(
            paste0("#", session$ns(local_rv$observers$inputID)),
            removeUI,
            immediate = TRUE
          )
          # reset observers
          sapply(local_rv$observers$obs, function(.) .$destroy())
          local_rv$observers$obs <<- c()
          # reset input IDs
          local_rv$observers$inputID <<- 0
          # reset paths
          local_rv$observers$path <<- c()
          # reset counter
          local_rv$count <<- 0
        }

        # Insert UI
        lapply(
          names(content()),
          function(content_name) {
            # prepare variables
            local_rv$count <<- local_rv$count + 1
            .contentID <- paste(content_name, local_rv$count, sep = "_", collapse = "_")
            subcontent <- content()[[content_name]]
            .inputID <- paste0(.contentID, "_input")
            .path <- attr(get_selected(input$tree)[[1]], "ancestry")
            # ** Save counter ----
            local_rv$observers$id <<- c(local_rv$observers$id, local_rv$count)
            # ** Save contentID ----
            local_rv$observers$inputID <<- c(local_rv$observers$inputID, .contentID)
            # ** Save path ----
            local_rv$observers$path <<- c(local_rv$observers$path, .path)
            # ** Save obs ----
            local_rv$observers$obs <<- c(
              local_rv$observers$obs,
              observeEvent(
                input[[.inputID]],
                if (any(torender.ui() %grep% .contentID)) {
                  # get tree
                  .tree <- input$tree
                  # path to node to change
                  .path <- paste(
                    attr(get_selected(.tree)[[1]], "ancestry"),
                    get_selected(.tree)[[1]][1],
                    gsub("[_0-9]*_input$", "", .inputID),
                    sep = "/"
                  ) |>
                    gsub(pattern = "^/|/$", replacement = "")
                  # get value
                  .value <- input[[.inputID]]
                  # assign value
                  local_rv$eml.list <- assignPath(local_rv$eml.list, .path, .value)
                  # browser()
                  # message("clicked ", .contentID)
                } else {
                  # get tree
                  .tree <- input$tree
                  # path to node to change
                  .path <- paste(
                    attr(get_selected(.tree)[[1]], "ancestry"),
                    get_selected(.tree)[[1]][1],
                    sep = "/"
                  ) |>
                    gsub(pattern = "^/|/$", replacement = "")
                  .current.node <- followPath(.tree, .path)
                  # change node to unselected
                  attr(.current.node, "stselected") <- NULL
                  attr(.current.node, "stopened") <- TRUE
                  # build path to child node
                  next.node.id <- gsub("^.*_(.*)_input$", "\\1", .inputID) |>
                    as.numeric()
                  # set child node to selected
                  attr(.current.node[[next.node.id]], "stselected") <- TRUE
                  attr(.current.node[[next.node.id]], "stopened") <- TRUE
                  # Assign finally
                  if (length(.path) > 0) {
                    # .path <- strsplit(.path, split = "/")[[1]]
                    .tree <- assignPath(x = .tree, path = .path, value = .current.node)
                  } else { # root
                    .tree <- .current.node
                  }
                  browser()
                  # Apply change
                  shinyTree::updateTree(session, "tree", .tree)
                },
                label = .inputID
              )
            )
            # Set html tag
            new_ui <- tags$div(
              id = session$ns(.contentID),
              if (any(torender.ui() %grep% .contentID)) { # Render Input
                if (is.numeric(subcontent)) {
                  numericInput(
                    session$ns(.inputID),
                    content_name,
                    value = subcontent
                  )
                } else {
                  textInput(
                    session$ns(.inputID),
                    content_name,
                    value = subcontent
                  )
                }
              } else { # Render Link to lower levels
                actionLink(
                  session$ns(.inputID),
                  tags$span(content_name, icon("chevron-right"))
                )
              }
            )
            # insert new_ui
            insertUI(
              selector = "#inserthere_mf_form",
              ui = new_ui,
              immediate = TRUE
            )
          }
        ) |> try()
      }
    )

    # # * browse buttons
    # output$links <- renderUI({
    #   if (length(torender.links()) > 0) {
    #     shinyWidgets::checkboxGroupButtons(
    #       session$ns("links"),
    #       "Other items",
    #       torender.links(),
    #       individual = TRUE
    #     )
    #   } else
    #     NULL
    # })

    # observeEvent(input$links, {
    #   req(isTruthy(input$links))
    #
    #   shinyWidgets::updateCheckboxGroupInput(
    #     session,
    #     "links",
    #     selected = NULL
    #   )
    #   browser()
    #   # shinyTree::updateTree(
    #   #   session,
    #   #   "tree",
    #   #
    #   # )
    # })

    # * xml attributes  ----
    collapsible("attributes")

    # Edit
    # * Select a node
    # * Update UI
    # ** edit content
    # ** edit attributes
    # ** remove children
    # ** add children
    # Save / quit
    # * turns back into XML
  })
}
