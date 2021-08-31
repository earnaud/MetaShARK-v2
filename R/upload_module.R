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
  
  # TODO add update module
  
  tagList(
    # wipRow(
    tabsetPanel(
      id = "upload",
      tabPanel(
        title = "upload",
        # select endpoint ----
        tags$h3("Select your MetaCat portal"),
        tags$div(
          tags$p(tags$code("dev"), "portals are under construction. No guarantee
            is given of their consistance.", tags$code("prod"), "portals are 
            completely functional. Chosing 'Other' will ask you to input some 
            technical information."),
          selectizeInput(
            NS(id, "endpoint"),
            "Available metacats:",
            c()
          ),
          uiOutput(NS(id, "actual-endpoint")),
          tagList(
            tags$p("Want your endpoint to be listed? get in touch with the dev team !")
          ),
          class = "leftMargin inputBox"
        ),
        
        # check authentication token ----
        tags$h3("Get your authentication token"),
        tags$div(
          tags$p("The ", tags$b("authentication token"), " is a user-specific characters key.
            It allows the user to authenticate a connection between its current location and
            a distant server, actually the metadata catalog. To upload a data package, 
            the authentication token is required."),
          actionButton(NS(id, "toSettings"), "Go to settings", icon("gear")),
          class = "leftMargin inputBox"
        ),
        
        # Upload or update ----
        shinyjs::hidden(
          wipRow(
            id = NS(id, "action_box"),
            tags$h3("Action to perform"),
            tags$div(
              tags$p("Please point out whether this data package was never published
                       (prime upload) or this is meant to be updated (update). If you
                       are updating your data package, you will be asked to identify
                       the previous version of the data package."),
              radioButtons(
                NS(id, "action"),
                "(required)",
                choiceNames = c("upload (first time)", "update"),
                choiceValues = c("upload", "update"),
                inline = TRUE
              ),
              shinyjs::hidden(
                selectInput(
                  NS(id, "online_dp"),
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
            ~/dataPackagesOutput/emlAssemblyLine/ or pick up the files one by one. 
            Selecting a data package will erase any previous selection."),
          fluidRow(
            column(
              9,
              # * DP ====
              tags$h4("Select a data package"),
              selectInput(
                NS(id, "DP"),
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
                NS(id, "metadata"),
                "EML-valid file (only one allowed)"
              ),
              textOutput(NS(id, "warnings-metadata")),
              # * Data ====
              fileInput(
                NS(id, "data"),
                multiple = TRUE,
                "Data files described in your EML file"
              ),
              textOutput(NS(id, "warnings-data")),
              # * Scripts ====
              fileInput(
                NS(id, "scripts"),
                multiple = TRUE,
                "Scripts used to produce or process data"
              ),
              textOutput(NS(id, "warnings-scripts"))
            ),
            column(
              3,
              tags$h4("Files list"),
              uiOutput(NS(id, "filesList"))
            )
          ),
          class = "leftMargin inputBox"
        ),
        
        # Constraints ----
        # div(id="constraints_div",
        #     tags$h4("Add constraints between script and data files"),
        #     actionButton(NS(id, "add_constraint"), "", icon = icon("plus"), width = "40px")
        # ),
        # tags$hr(),
        
        # Process -----
        tags$div(
          uiOutput(NS(id, "last_uploaded")) |>
            shinyjs::hidden(),
          actionButton(
            NS(id, "process"),
            "Process",
            icon = icon("rocket"),
            width = "100%"
          ),
          class = "leftMargin inputBox"
        ),
      ), # end of upload tab
      # Update ----
      tabPanel(
        title = "update",
        wipRow(
          "WIP"
          # 1. solr query
          # 2. select items to update
          # 3. select files
        )
      ) # end of update tab
    ) # end of tabSetPanel
    # )
  ) # end of tagList
}

#' @title upload
#'
#' @describeIn uploadUI
#'
#' @param main.env inner global environment
#'
#' @import shiny
#' @importFrom dplyr filter select %>%
#' @importFrom shinyjs enable disable click
#' @importFrom data.table fread fwrite
#' @importFrom mime guess_type
upload <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # dev
    if (main.env$dev){
      observeEvent(
        main.env$dev.browse(),
        {
          if (main.env$current.tab() == "upload") {
            browser()
          }
        },
        label = "Upload: dev"
      )
    }
    
    registeredEndpoints <- isolate(main.env$VALUES$dataone.endpoints)
    dev <- main.env$dev
    
    # Select endpoint ----
    # write choices
    choices = registeredEndpoints %>%
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
        registeredEndpoints |>
          dplyr::filter(name == endpoint()) |>
          dplyr::select(mn)
      } else {
        input$other_endpoint
      }
    })
    
    output$`actual-endpoint` <- renderUI({
      if (endpoint() != "Other") {
        tags$span(
          tags$b("Current endpoint:"), 
          memberNode(),
          HTML("\t"),
          tags$div(
            icon("question-circle"),
            style = "width:fit-content; display:inline-flex; margin-left: 5px",
            title = registeredEndpoints |>
              filter(name == endpoint()) |>
              select(description) |>
              as.character()
          ),
          style = "display:inline-flex"
        )
      } else {
        textInput(
          NS(id, "other_endpoint"),
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
    },
    ignoreInit = TRUE
    )
    
    observe({
      if (!is.character(options("dataone_token")) ||
          !is.character(options("dataone_test_token")) ||
          (is.null(options("dataone_token")) && is.null(options("dataone_test_token")))
      ) {
        output$token_status <- renderUI({
          tags$div("UNFILLED", class = "danger")
        })
        # shinyjs::disable("process")
      }
      else {
        output$token_status <- renderUI({
          tags$div("FILLED", class = "valid")
        })
        # shinyjs::enable("process")
      }
    })
    
    # Update or upload ====
    if(main.env$wip)
      shinyjs::show("action_box")
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
    .files.poll <- reactiveDirReader(
      main.env$PATHS$eal.dp,
      session,
      pattern = "_emldp$",
      full.names = TRUE
    )
    
    observe({
      validate(
        need(isTruthy(.files.poll()), "No files found")
      )
      
      dp.list <- .files.poll() |>
        basename() |>
        gsub(pattern = "_emldp$", replacement = "") |>
        gsub(pattern = "//", replacement = "/")
      
      # update list
      updateSelectInput(
        session,
        "DP",
        # choiceNames = c("None selected", dp.list),
        choice = c("---", gsub(" \\(.*\\)$", "", dp.list))
      )
    },
    label = "Upload: dp list")
    
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
      
      .dir <- .files.poll()[grepl(input$DP, .files.poll())]|>
        gsub(pattern = "//", replacement = "/")
      .id <- input$DP
      # grab metadata files
      .eml.files <- sprintf("%s/%s/eml", .dir, .id) |>
        dir(full.names = TRUE)
      rv$md <- data.frame(
        name = basename(.eml.files),
        size = base::file.size(.eml.files),
        type = mime::guess_type(.eml.files),
        datapath = .eml.files
      )
      # grab data files
      .data.files <- sprintf("%s/%s/data_objects", .dir, .id) |>
        dir(full.names = TRUE)
      rv$data <- data.frame(
        name = basename(.data.files),
        size = base::file.size(.data.files),
        type = mime::guess_type(.data.files),
        datapath = .data.files
      )
      # no script directory -> do not grab scripts
    },
    ignoreInit = TRUE,
    label = "DPinput"
    )
    
    observeEvent(input$metadata, {
      if(nrow(rv$md) > 0)
        showNotification(
          "Only one metadata file allowed. Replacing with new one.",
          type = "warning"
        )
      rv$md <- input$metadata
      # rename file
      .new.name <- gsub("/..xml$", sprintf("/%s", rv$md$name), rv$md$datapath)
      file.rename(rv$md$datapath, .new.name)
      rv$md$datapath <- .new.name
    })
    
    observeEvent(input$data, {
      .add <- input$data
      req(isContentTruthy(.add))
      # Update list instead of erasing
      .new.names <- sapply(
        1:nrow(.add),
        function(.rid) {
          gsub(
            pattern = "(.*/).*$",
            replacement = sprintf("\\1%s", .add$name[.rid]),
            .add$datapath[.rid]
          )
        }
      )
      file.rename(.add$datapath, .new.names)
      .add$datapath <- .new.names
      rv$data <- rbind(rv$data, .add)
    })
    
    observeEvent(input$scripts, {
      .add <- input$scripts
      req(isContentTruthy(.add))
      # Update list instead of erasing
      .new.names <- sapply(
        1:nrow(rv$scr),
        function(.rid) {
          gsub(
            pattern = "(.*/).*$",
            replacement = sprintf("\\1%s", rv$scr$name[.rid]),
            rv$scr$datapath[.rid]
          )
        }
      )
      file.rename(rv$scr$datapath, .new.names)
      rv$scr$datapath <- .new.names
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
            NS(id, "md-files"),
            label = "EML file",
            choices = rv$md$name
          )
        },
        if (dim(rv$data)[1] > 0) {
          checkboxGroupInput(
            NS(id, "data-files"),
            label = "Data files",
            choices = rv$data$name
          )
        },
        if (dim(rv$scr)[1] > 0) {
          checkboxGroupInput(
            NS(id, "scr-files"),
            label = "Scripts",
            choices = rv$scr$name
          )
        },
        actionButton(NS(id, "rmv"), "Remove", class = "danger")
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
    observe({
      shinyjs::toggleState(
        "process",
        condition = (
          isTruthy(memberNode())
        ) && (
          dim(rv$md)[1] == 1 && dim(rv$data)[1] > 0
        ) && (
          is.character(main.env$SETTINGS$metacat.token) &&
            !is.null(main.env$SETTINGS$metacat.token)
        ) && (
          input$action %in% c("upload", "update")
        )
      )
    })
    
    # * pressed ----
    last.uploaded <- reactiveVal("")
    observeEvent(input$process, {
      shinyjs::disable("process")
      
      md.format <- EML::read_eml(as.character(rv$md$datapath))$`@context`$eml |>
        gsub(
          pattern = "https://eml.ecoinformatics.org/eml-", 
          replacement = "eml://ecoinformatics.org/eml-"
        )
      
      out <- uploadDP(
        endpoint = registeredEndpoints |>
          dplyr::filter(name == endpoint()),
        token = main.env$SETTINGS$metacat.token,
        eml = rv$md$datapath,
        data = rv$data$datapath,
        scripts = if (dim(rv$scr)[1] > 0) rv$scr$datapath else NULL,
        use.doi = FALSE
      )
      
      if (class(out$id) == "try-error") {
        showNotification(out$id, type = "error")
      } else {
        showNotification(sprintf("Uploaded DP (metadata id: %s)", out$id), type = "message")
      }
      
      last.uploaded(out$adress)
      shinyjs::show("last_uploaded")
      
      shinyjs::enable("process")
    })
    
    output$last_uploaded <- renderUI({
      validate(
        need(last.uploaded() != "", "none"),
        need(class(last.uploaded()) != "try-error", last.uploaded())
      )
      
      tags$div(
        tags$b("Last uploaded:"),
        tags$a(href=last.uploaded(), last.uploaded())
      )
    })
  })
}
