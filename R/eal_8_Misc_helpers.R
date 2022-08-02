#' @import shiny
#' @importFrom htmltools tagAppendAttributes
#'
#' @noRd
MiscellaneousUI <- function(id, help_label = NULL) {
  ns <- NS(id)

  tagList(
    # file selection ----
    tags$b(paste0("Select '", unns(id), "' file.")),
    tags$br(),
    div(
      fileInput(
        ns("file"),
        "",
        multiple = FALSE,
        buttonLabel = span("Load file", icon("file")),
        placeholder = ""
      )
    ),
    tags$hr(),
    # Content edition ----
    tags$b(help_label),
    SummeRnote::summernoteInput(
      ns("content"),
      label = NULL,
      height = 200,
      toolbar = list(
        list("style", c("bold", "italic", "underline")),
        list("font", c("superscript", "subscript")),
        list("para", c("style", "ul", "ol", "paragraph")),
        list("insert", c("link", "table")),
        list("Misc", c("codeview", "undo", "redo"))
      )
    )
  ) # end of tagList
}

#' @import shiny
#' @importFrom SummeRnote summernoteInput
#' @importFrom readtext readtext
#' @importFrom dplyr select
#'
#' @noRd
Miscellaneous <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    # Get content ----
    # Debounced input in two steps
    .content <- reactive({
      req(main_env$EAL$page == 8)
      req("content" %in% names(input))

      input$content
    }) |>
      debounce(1000)

    observe({
      req(main_env$EAL$page == 8)
      devmsg(tag = "Misc", "save %s content", id)
      main_env$local_rv[[id]]$content <- .content()
    })

    # Get file ----
    observeEvent(input$file, {
      req(isTruthy(input$file))

      if (isContentTruthy(input$file)) {
        main_env$local_rv[[id]]$file <- input$file$datapath
      }
    })

    observeEvent({
      main_env$EAL$page
      main_env$local_rv[[id]]$file
    }, {
      req(main_env$EAL$page == 8)

      .tmp <- if (grepl(".md$", main_env$local_rv[[id]]$file)) {
        readHTMLfromMD(main_env$local_rv[[id]]$file)
      } else {
        main_env$local_rv[[id]]$file |>
          readtext::readtext() |>
          dplyr::select("text") |>
          unlist() |>
          HTML()
      }

      devmsg(
        tag = "Misc", "update note input %s (%s)",
        id, main_env$local_rv[[id]]$file
      )

      SummeRnote::updateSummernoteInput(
        session$ns("content"),
        value = .tmp,
        session = session
      )
    },
    label = session$ns("Misc update note"),
    priority = -1
    )

    # Verbose file selection
    # output$selected <- renderText({
    #   paste(
    #     basename(main_env$local_rv[[id]]$file),
    #     "\n(in:", dirname(main_env$local_rv[[id]]$file), ")"
    #   )
    # })
  })
}

# Keyword Sets ====

#' @import shiny
#' @importFrom dplyr filter select
#'
#' @noRd
insertKeywordSet <- function(id, main_env, .setup = FALSE) {
  # Add row
  kws <- unns(id)
  if (grepl("^_", kws)) {
    # already set local table
  } else {
    # add a new row to local table
    main_env$local_rv$keywords[nrow(main_env$local_rv$keywords) + 1, ] <-
      c(rep("", ncol(main_env$local_rv$keywords) - 1), keyword_set = kws)
  }
  # create UI
  new_ui <- keywordSetUI(
    id,
    kwt_value = main_env$local_rv$keywords |>
      dplyr::filter(keyword_set == kws) |>
      dplyr::select(keywordThesaurus) |>
      unlist()
  )
  # insert the UI
  insertUI(selector = "#inserthere_eal8", ui = new_ui, immediate = TRUE)
  # create the server
  keywordSet(kws, main_env, .setup = .setup)
}

#' @import shiny
#' @importFrom shinyWidgets searchInput
#'
#' @noRd
keywordSetUI <- function(id, kwt_value = NULL) {
  ns <- NS(id)

  # Get value
  if (isFALSE(
    isTruthy(kwt_value) &&
      is.character(kwt_value)
  )) {
    kwt_value <- ""
  }

  # UI output
  tags$div(
    id = ns("kws_div"),
    tags$hr(),
    fluidRow(
      # Remove UI
      column(1, actionButton(
        ns("remove"), "", icon("trash"),
        class = "redButton"
      )),
      column(5, textInput(ns("kwt"), "Keyword thesaurus", value = kwt_value)),
      column(
        5,
        shinyWidgets::searchInput(
          ns("add_kw"),
          "Add keyword",
          btnSearch = icon("plus")
        )
      )
    ),
    tags$div(id = paste0("inserthere_eal8_", unns(id)), class = "tag_sequence")
  )
}

# Auto save is performed here to allow quick comparisons between different
# keyword sets.
#' @import shiny
#' @importFrom dplyr filter select mutate
#' @importFrom shinyWidgets updateSearchInput
#'
#' @noRd
keywordSet <- function(id, main_env, .setup = FALSE) {
  moduleServer(id, function(input, output, session) {
    kws <- unns(id) # same id, different variable for legibility

    # Setup ====
    observeEvent(TRUE, {
      req(isTRUE(.setup))

      .keywords <- main_env$local_rv$keywords |>
        dplyr::filter(keyword_set == kws) |>
        dplyr::select(keyword) |>
        unlist() |>
        strsplit(",") |>
        unlist()
      sapply(.keywords, function(kw) {
        insertKeywordTag(session$ns(kw), kws, main_env)
      })
    },
    once = TRUE
    )

    # Add keyword ====
    observeEvent(input$add_kw, {
      # Get input
      new_kw <- input$add_kw
      req(isTruthy(new_kw))
      # Alert for ","
      if (grepl(",", new_kw)) {
        showNotification("BEWARE: using a ',' (comma) in a keyword will result
                         in splitting it.", duration = NULL, type = "warning")
      }
      # Insert tag for keyword
      insertKeywordTag(session$ns(new_kw), kws, main_env)
      # Clear input
      shinyWidgets::updateSearchInput(session, "add_kw", value = "")
    },
    ignoreInit = TRUE
    )

    # Set keyword thesaurus ====
    observeEvent(input$kwt, {
      # Get input
      kwt <- input$kwt
      req(isTruthy(kwt))
      # Save changes
      main_env$local_rv$keywords <- main_env$local_rv$keywords |>
        dplyr::mutate(
          keywordThesaurus = ifelse(
            keyword_set == kws,
            kwt,
            keywordThesaurus
          )
        )
      },
      ignoreInit = TRUE,
      label = session$ns("EAL8 set kwThesaurus")
    )

    # Remove UI ====
    observeEvent(input$remove, {
      # Remove UI
      removeUI(selector = paste0("#", session$ns("kws_div")), immediate = TRUE)
      # Remove keywords
      main_env$local_rv$keywords <- main_env$local_rv$keywords |>
        dplyr::filter(keyword_set != kws)
      # Clear modules
      remove_shiny_inputs(id, input)
    })
  })
}

# Keyword tags ====
#' @import shiny
#' @importFrom dplyr filter select mutate
#'
#' @noRd
insertKeywordTag <- function(id, kws, main_env) {
  # Add item
  new_kw <- unns(id)
  # Check if existing keyword
  # New keyword -- not in local_rv
  if (
    isFALSE(
      main_env$local_rv$keywords |>
        dplyr::filter(keyword_set == kws) |>
        dplyr::select(keyword) |>
        grepl(pattern = new_kw)
    )
  ) {
    main_env$local_rv$keywords <-
      main_env$local_rv$keywords |>
      dplyr::mutate(
        keyword = ifelse(
          keyword_set == kws,
          ifelse(
            keyword == "",
            new_kw,
            paste(keyword, new_kw, sep = ",")
          ),
          keyword
        )
      )
  }
  # Create UI
  new_ui <- keywordTagUI(id)
  # Insert UI
  insertUI(
    selector = paste0("#inserthere_eal8_", kws),
    ui = new_ui,
    immediate = TRUE
  )
  # Set server
  keywordTag(new_kw, kws, main_env)
}

#' @import shiny
#'
#' @noRd
keywordTagUI <- function(id) {
  tags$div(
    id = NS(id, "kw_tag"),
    unns(id),
    actionLink(NS(id, "remove"), "", icon("times"), style = "color: white")
  )
}

#' @import shiny
#' @importFrom dplyr mutate
#'
#' @noRd
keywordTag <- function(id, kws, main_env) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, {
      # remove the UI
      removeUI(selector = paste0("#", session$ns("kw_tag")), immediate = TRUE)
      # remove keyword
      main_env$local_rv$keywords <- main_env$local_rv$keywords |>
        dplyr::mutate(
          keyword = ifelse(
            keyword_set == kws,
            gsub(paste0("(", id, ")|(^,)|(,,)|(,$)"), "", keyword),
            keyword
          )
        )
      # Clear module
      remove_shiny_inputs(id, input)
    },
    once = TRUE,
    ignoreInit = TRUE,
    label = session$ns("remove")
    )
  })
}
