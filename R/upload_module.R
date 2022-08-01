#' @title uploadUI
#'
#' @description UI part for the upload module. Used to build and drop
#' data packages to a chosen metacat.
#'
#' @param id shiny module id
#'
#' @import shiny
#' @importFrom data.table fread
uploadUI <- function(id) {
  ns <- NS(id)
  # TODO add update module

  tagList(
    # select endpoint ----
    tags$h3("Select your MetaCat portal"),
    tags$div(
      tags$p(tags$code("dev"), "portals are under construction. No guarantee
            is given of their consistance.", tags$code("prod"), "portals are
            completely functional. Chosing 'Other' will ask you to input some
            technical information."),
      selectizeInput(
        ns("endpoint"),
        "Available metacats:",
        c()
      ),
      uiOutput(ns("actual_endpoint")),
      tagList(
        tags$p("Noticed a missing endpo? get in touch with the dev team !")
      ),
      class = "leftMargin inputBox"
    ),

    # check authentication token ----
    tags$h3("Get your authentication token"),
    tags$div(
      tags$p("The ", tags$b("authentication token"), " is a user-specific
      characters key. It allows the user to authenticate a connection
      between its current location and a distant server, actually the
      metadata catalog. To upload a data package, the authentication token is
      required."),
      actionButton(ns("toSettings"), "Go to settings", icon("cogs")),
      class = "leftMargin inputBox"
    ),

    # Upload or update ----
    shinyjs::hidden(
      wipRow(
        id = ns("action_box"),
        tags$h3("Action to perform"),
        tags$div(
          tags$p("Please point out whether this data package was never published
                  (prime upload) or this is meant to be updated (update). If you
                  are updating your data package, you will be asked to identify
                  the previous version of the data package."),
          radioButtons(
            ns("action"),
            "(required)",
            choiceNames = c("upload (first time)", "update"),
            choiceValues = c("upload", "update"),
            inline = TRUE
          ),
          shinyjs::hidden(
            selectInput(
              ns("online_dp"),
              "Online available data packages",
              c()
            )
          ),
          class = "leftMargin inputBox"
        )
      )
    ),

    # files input ----
    tags$h3("Select your data package files"),
    tags$div(
      # data package input
      tags$p("You can either select a data package from
            ~/dataPackagesOutput/emlAssemblyLine/ or pick up the files one by
            one.
            Selecting a data package will erase any previous selection."),
      fluidRow(
        column(
          9,
          # * DP ====
          tags$h4("Select a data package"),
          selectInput(
            ns("DP"),
            "Data package",
            choices = c(
              None = ""
            ),
            multiple = FALSE
          ),
          # individual files inputs
          tags$h4("Add or remove files"),
          # * Metadata ====
          fileInput(
            ns("metadata"),
            "EML-valid file (only one allowed)"
          ),
          textOutput(ns("warnings-metadata")),
          # * Data ====
          fileInput(
            ns("data"),
            multiple = TRUE,
            "Data files described in your EML file"
          ),
          textOutput(ns("warnings-data")),
          # * Scripts ====
          fileInput(
            ns("scripts"),
            multiple = TRUE,
            "Scripts used to produce or process data"
          ),
          textOutput(ns("warnings-scripts"))
        ),
        column(
          3,
          tags$h4("Files list"),
          uiOutput(ns("filesList"))
        )
      ),
      class = "leftMargin inputBox"
    ),

    # Constraints ----
    # TODO

    # Process -----
    tags$div(
      uiOutput(ns("last_uploaded")) |>
        shinyjs::hidden(),
      actionButton(
        ns("process"),
        "Process",
        icon = icon("rocket"),
        width = "100%"
      ),
      uiOutput(ns("required")) |>
        shinyjs::hidden(),
      class = "leftMargin inputBox"
    )
  ) # end of tagList
}

#' @title upload
#'
#' @describeIn uploadUI
#'
#' @param main_env inner global environment
#'
#' @import shiny
#' @importFrom dplyr filter select %>%
#' @importFrom shinyjs enable disable click
#' @importFrom data.table fread fwrite
#' @importFrom mime guess_type
upload <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    # dev
    if (main_env$dev) {
      observeEvent(main_env$dev_browse(), {
        if (main_env$current_tab() == "upload") {
          browser()
        }
      }, label = "Upload: dev")
    }

    registered_endpoints <- isolate(main_env$VALUES$dataone_endpoints)

    # Select endpoint ----
    # write choices
    choices <- registered_endpoints %>%
      split(.$cn) %>%
      sapply(select, "name") %>%
      setNames(nm = gsub(".name", "", names(.)))
    # update
    updateSelectInput(
      session,
      "endpoint",
      choices = c(choices, "Other")
    )

    endpoint <- reactive(input$endpoint)

    memberNode <- reactive({
      if (endpoint() != "Other") {
        registered_endpoints |>
          dplyr::filter(name == endpoint()) |>
          dplyr::select(mn)
      } else {
        input$other_endpoint
      }
    })

    output$actual_endpoint <- renderUI({
      if (endpoint() != "Other") {
        tags$span(
          tags$b("Current endpoint:"),
          memberNode(),
          HTML("\t"),
          tags$div(
            icon("question-circle"),
            style = "width:fit-content; display:inline-flex; margin-left: 5px",
            title = registered_endpoints |>
              filter(name == endpoint()) |>
              select(description) |>
              as.character()
          ),
          style = "display:inline-flex"
        )
      } else {
        textInput(
          session$ns("other_endpoint"),
          "Write the URL of the Member Node",
          placeholder = "http://your.server/d1/mn/v2"
        )
      }
    })

    # Token input ----
    observeEvent(input$toSettings, {
        shinyjs::addClass(
          selector = "aside.control-sidebar",
          class = "control-sidebar-open"
        )
        main_env$SETTINGS$side_tab <- "2"
      },
      ignoreInit = TRUE
    )

    observe({
      if (!is.character(options("dataone_token")) ||
        !is.character(options("dataone_test_token")) ||
        (is.null(options("dataone_token")) &&
          is.null(options("dataone_test_token")))
      ) {
        output$token_status <- renderUI({
          tags$div("UNFILLED", class = "danger")
        })
      } else {
        output$token_status <- renderUI({
          tags$div("FILLED", class = "valid")
        })
      }
    })

    # Update or upload ====
    if (main_env$wip) {
      shinyjs::show("action_box")
    }
    # get list of DP
    observeEvent(input$action, {
      req(isTruthy(input$action))

      shinyjs::toggle(
        "online_dp",
        condition = input$action == "update"
      )
    })

    # Files input ====
    # * DP list ----
    .files_poll <- reactiveDirReader(
      main_env$PATHS$eal_dp,
      session,
      pattern = "_emldp$",
      full_names = TRUE
    )

    observe({
        validate(
          need(isTruthy(.files_poll()), "No files found")
        )

        dp_list <- .files_poll() |>
          basename() |>
          gsub(pattern = "_emldp$", replacement = "") |>
          gsub(pattern = "//", replacement = "/")

        # update list
        updateSelectInput(
          session,
          "DP",
          choice = c("---", dp_list)
        )
      },
      label = "Upload: dp list"
    )

    # * Get files ----
    rv <- reactiveValues(
      md = data.frame(stringsAsFactors = FALSE),
      data = data.frame(stringsAsFactors = FALSE),
      scr = data.frame(stringsAsFactors = FALSE)
    )
    makeReactiveBinding("rv$md")
    makeReactiveBinding("rv$data")
    makeReactiveBinding("rv$scr")

    observeEvent(input$DP, {
        validate(
          need(input$DP != "---", "No dp selected")
        )

        .dir <- .files_poll()[grepl(input$DP, .files_poll())] |>
          gsub(pattern = "//", replacement = "/")
        .id <- input$DP
        # grab metadata files
        .eml_files <- sprintf("%s/%s/eml", .dir, .id) |>
          dir(full.names = TRUE)
        rv$md <- data.frame(
          name = basename(.eml_files),
          size = base::file.size(.eml_files),
          type = mime::guess_type(.eml_files),
          datapath = .eml_files
        )
        # grab data files
        .data_files <- sprintf("%s/%s/data_objects", .dir, .id) |>
          dir(full.names = TRUE)
        rv$data <- data.frame(
          name = basename(.data_files),
          size = base::file.size(.data_files),
          type = mime::guess_type(.data_files),
          datapath = .data_files
        )
        # no script directory -> do not grab scripts
      },
      ignoreInit = TRUE,
      label = "DPinput"
    )

    observeEvent(input$metadata, {
      if (nrow(rv$md) > 0) {
        showNotification(
          "Only one metadata file allowed. Replacing with new one.",
          type = "warning"
        )
      }
      rv$md <- input$metadata
      # rename file
      .new_name <- gsub("/..xml$", sprintf("/%s", rv$md$name), rv$md$datapath)
      file.rename(rv$md$datapath, .new_name)
      rv$md$datapath <- .new_name
    })

    observeEvent(input$data, {
      .add <- input$data
      req(isContentTruthy(.add))
      # Update list instead of erasing
      .new_names <- sapply(
        seq_row(.add),
        function(.rid) {
          gsub(
            pattern = "(.*/).*$",
            replacement = sprintf("\\1%s", .add$name[.rid]),
            .add$datapath[.rid]
          )
        }
      )
      file.rename(.add$datapath, .new_names)
      .add$datapath <- .new_names
      rv$data <- rbind(rv$data, .add)
    })

    observeEvent(input$scripts, {
      .add <- input$scripts
      req(isContentTruthy(.add))
      # Update list instead of erasing
      .new_names <- sapply(
        seq_row(rv$scr),
        function(.rid) {
          gsub(
            pattern = "(.*/).*$",
            replacement = sprintf("\\1%s", rv$scr$name[.rid]),
            rv$scr$datapath[.rid]
          )
        }
      )
      file.rename(rv$scr$datapath, .new_names)
      rv$scr$datapath <- .new_names
      rv$scr <- rbind(input$scripts, .add)
    })

    output$filesList <- renderUI({
      validate(
        need(
          dim(rv$md) > 0 ||
            dim(rv$data) > 0 ||
            dim(rv$scr) > 0,
          "No file selected"
        )
      )

      tagList(
        if (dim(rv$md)[1] > 0) {
          checkboxGroupInput(
            sessions$ns("md-files"),
            label = "EML file",
            choices = rv$md$name
          )
        },
        if (dim(rv$data)[1] > 0) {
          checkboxGroupInput(
            sessions$ns("data-files"),
            label = "Data files",
            choices = rv$data$name
          )
        },
        if (dim(rv$scr)[1] > 0) {
          checkboxGroupInput(
            sessions$ns("scr-files"),
            label = "Scripts",
            choices = rv$scr$name
          )
        },
        actionButton(sessions$ns("rmv"), "Remove", class = "danger")
      )
    })

    observeEvent(input$rmv, {
        .rmv <- input$`md-files`
        if (isContentTruthy(.rmv)) {
          .ind <- match(.rmv, rv$md$name)
          rv$md <- rv$md[-.ind, ]
        }
        .rmv <- input$`data-files`
        if (isContentTruthy(.rmv)) {
          .ind <- match(.rmv, rv$data$name)
          rv$data <- rv$data[-.ind, ]
        }
        .rmv <- input$`scr-files`
        if (isContentTruthy(.rmv)) {
          .ind <- match(.rmv, rv$scr$name)
          rv$scr <- rv$scr[-.ind, ]
        }
      },
      ignoreInit = TRUE
    )

    observe({
      if (
        dim(rv$scr)[1] == 0 ||
          dim(rv$data)[1] == 0
      ) {
        shinyjs::disable("add_constraint")
      } else {
        shinyjs::enable("add_constraint")
      }
    })

    # Process ----
    # * completeness ----
    are_required <- reactiveVal()
    observe({
      # Set conditions
      is_mn_selected <- isTruthy(memberNode())
      is_md_selected <- dim(rv$md)[1] == 1
      is_data_selected <- dim(rv$data)[1] > 0
      is_token_selected <- isTRUE(
        is.character(main_env$SETTINGS$metacat_token) &&
          !is.null(main_env$SETTINGS$metacat_token) &&
          main_env$SETTINGS$metacat_token != ""
      )
      is_action_selected <- input$action %in% c("upload", "update")

      # Switch process button
      shinyjs::toggleState(
        "process",
        condition = (
          is_mn_selected &&
            is_md_selected &&
            is_data_selected &&
            is_token_selected &&
            is_action_selected
        )
      )

      # Set list of required things
      # as.list used to avoid NULL values
      are_required(as.list(c(
        if (!is_mn_selected) {
          "No member node targeted"
        },
        if (!is_md_selected) {
          "One metadata file is expected"
        },
        if (!is_data_selected) {
          "At least one data file is expected"
        },
        if (!is_token_selected) {
          "No dataone token was set"
        },
        if (!is_action_selected) {
          "No action must be taken"
        }
      )))

      shinyjs::toggle(
        "required",
        condition = is.list(are_required()) && length(are_required()) > 0
      )
    })

    output$required <- renderUI({
      validate(
        need(length(are_required()) > 0, "no item required")
      )

      tagList(
        tags$b("Are required:"),
        tags$ul(lapply(are_required(), tags$li))
      )
    })

    # * pressed ----
    last_uploaded <- reactiveVal("")
    observeEvent(input$process, {
      shinyjs::disable("process")

      md_format <- EML::read_eml(as.character(rv$md$datapath))$`@context`$eml |>
        gsub(
          pattern = "https://eml.ecoinformatics.org/eml-",
          replacement = "eml://ecoinformatics.org/eml-"
        )

      out <- uploadDP(
        endpoint = registered_endpoints |>
          dplyr::filter(name == endpoint()),
        token = main_env$SETTINGS$metacat_token,
        eml = rv$md$datapath,
        data = rv$data$datapath,
        scripts = if (dim(rv$scr)[1] > 0) rv$scr$datapath else NULL,
        use.doi = FALSE
      )

      if (class(out$id) == "try-error") {
        showNotification(out$id, type = "error")
      } else {
        showNotification(sprintf("Uploaded DP (metadata id: %s)", out$id),
          type = "message"
        )
      }

      last_uploaded(out$adress)
      shinyjs::show("last_uploaded")

      shinyjs::enable("process")
    })

    output$last_uploaded <- renderUI({
      validate(
        need(last_uploaded() != "", "none"),
        need(class(last_uploaded()) != "try-error", last_uploaded())
      )

      tags$div(
        tags$b("Last uploaded:"),
        tags$a(href = last_uploaded(), last_uploaded(), icon("external-link"))
      )
    })
  })
}
