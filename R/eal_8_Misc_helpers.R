#' @import shiny
#' @importFrom htmltools tagAppendAttributes
#'
#' @noRd
MiscellaneousUI <- function(id, help.label = NULL) {
  tagList(
    # file selection ----
    tags$b(paste0("Select '", unns(id), "' file.")),
    tags$br(),
    div(
      fileInput(
        NS(id, "file"),
        "",
        multiple = FALSE,
        buttonLabel = span("Load file", icon("file")),
        placeholder = ""
      )
    ),
    tags$b("Selected file:"),
    textOutput(NS(id, "selected")) %>%
      htmltools::tagAppendAttributes(class = "ellipsis"),
    tags$hr(),
    # Content edition ----
    tags$b(help.label),
    SummeRnote::summernoteInput(
      NS(id, "content"),
      label = NULL,
      # value = HTML(main.env$local.rv[[id]]$content),
      height = 300,
      toolbar = list(
        list("style", c("bold", "italic", "underline")),
        list("font", c("superscript", "subscript")),
        list("para", c("style", "ul", "ol", "paragraph")),
        list("insert", c("link", "table")),
        list("Misc", c("codeview", "undo", "redo"))
      )
    )
    # uiOutput(NS(id, "content"))
  ) # end of tagList
}

#' @import shiny
#' @importFrom SummeRnote summernoteInput
#' @importFrom dplyr %>%
#'
#' @noRd
Miscellaneous <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Set UI ----
    output$content <- renderUI({
      req(main.env$EAL$page == 8)
    })
    
    # Get content ----
    # Debounced input in two steps
    .content <- reactive({
      req(main.env$EAL$page == 8)
      req("content" %in% names(input))
      
      input$content
    }) %>% 
      debounce(1000)
    
    observe({
      req(main.env$EAL$page == 8)
      
      main.env$local.rv[[id]]$content <- .content()
    })
    
    # Get file ----
    observeEvent(input$file, {
      req(isTruthy(input$file))
      
      if(isContentTruthy(input$file))
        main.env$local.rv[[id]]$file <- input$file$datapath
    })
    
    observeEvent({
      main.env$EAL$page
      main.env$local.rv[[id]]$file
    }, {
      req(main.env$EAL$page == 8)
      
      if(grepl(".md$", main.env$local.rv[[id]]$file)) {
        main.env$local.rv[[id]]$content <- readHTMLfromMD(main.env$local.rv[[id]]$file)
      } else # if(grepl(".txt$", main.env$local.rv[[id]]$file)) {
        main.env$local.rv[[id]]$content <- main.env$local.rv[[id]]$file %>%
          readtext %>%
          select("text") %>%
          unlist %>%
          HTML
      
      SummeRnote::updateSummernoteInput(
        session$ns("content"),
        value = main.env$local.rv[[id]]$content,
        session = session
      )
    },
    priority = -1
    )
    
    # Verbose file selection
    output$selected <- renderText({
      paste(
        basename(main.env$local.rv[[id]]$file),
        "\n(in:", dirname(main.env$local.rv[[id]]$file), ")"
      )
    })
  })
}

# Keyword Sets ====

#' @import shiny
#' @importFrom dplyr filter select %>%
#'
#' @noRd
insertKeywordSet <- function(id, main.env, .setup = FALSE) {
  # Add row
  kws <- unns(id)
  if(grepl("^_", kws)) {
    # already set local table
  } else {
    # add a new row to local table
    main.env$local.rv$keywords[nrow(main.env$local.rv$keywords)+1,] <- 
      c(rep("", ncol(main.env$local.rv$keywords)-1), keyword.set = kws)
  }
  # create UI
  new.ui <- keywordSetUI(
    id,
    kwt.value = main.env$local.rv$keywords %>% 
      dplyr::filter(keyword.set == kws) %>%
      dplyr::select(keyword.thesaurus) %>%
      unlist()
  )
  # insert the UI
  insertUI(selector = "#inserthere_eal8", ui = new.ui, immediate = TRUE)
  # create the server
  keywordSet(kws, main.env, .setup = .setup)
}

#' @import shiny
#' @importFrom shinyWidgets searchInput
#'
#' @noRd
keywordSetUI <- function(id, kwt.value = NULL) {
  # Get value
  if(isFALSE(
    isTruthy(kwt.value) &&
    is.character(kwt.value)
  ))
    kwt.value <- ""
  
  # UI output
  tags$div(
    id = NS(id, "kws_div"),
    tags$hr(),
    fluidRow(
      # Remove UI
      column(1,
        actionButton(NS(id, "remove"), "", icon("trash"), class = "redButton")
      ),
      column(5, textInput(NS(id, "kwt"), "Keyword thesaurus", value = kwt.value)),
      column(5,
        shinyWidgets::searchInput(
          NS(id, "add_kw"),
          "Add keyword",
          btnSearch = icon("plus")
        )
      )
    ),
    tags$div(id=paste0("inserthere_eal8_", unns(id)), class = "tag_sequence")
  )
}

# Auto save is performed here to allow quick comparisons between different 
# keyword sets.
#' @import shiny
#' @importFrom dplyr filter select %>% mutate
#' @importFrom shinyWidgets updateSearchInput
#'
#' @noRd
keywordSet <- function(id, main.env, .setup = FALSE) {
  moduleServer(id, function(input, output, session) {
    kws <- unns(id) # same id, different variable for legibility
    
    # Setup ====
    observeEvent(TRUE, {
      req(isTRUE(.setup))
      
      .keywords <- main.env$local.rv$keywords %>%
        dplyr::filter(keyword.set == kws) %>%
        dplyr::select(keyword) %>%
        unlist() %>%
        strsplit(",") %>%
        unlist()
      sapply(.keywords, function(kw) {
        insertKeywordTag(session$ns(kw), kws, main.env)
      })
    },
    once = TRUE
    )
    
    # Add keyword ====
    observeEvent(input$add_kw, {
      # Get input
      new.kw <- input$add_kw
      req(isTruthy(new.kw))
      # Alert for ","
      if(grepl(",", new.kw))
        showNotification("BEWARE: using a ',' (comma) in a keyword will result
                         in splitting it.", duration = NULL, type = "warning")
      # Insert tag for keyword
      insertKeywordTag(session$ns(new.kw), kws, main.env)
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
      main.env$local.rv$keywords <- main.env$local.rv$keywords %>%
        dplyr::mutate(
          keyword.thesaurus = ifelse(
            keyword.set == kws,
            kwt,
            keyword.thesaurus
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
      main.env$local.rv$keywords <- main.env$local.rv$keywords %>%
        dplyr::filter(keyword.set != kws)
      # Clear modules
      remove_shiny_inputs(id, input)
    })
  })
}

# Keyword tags ====
#' @import shiny
#' @importFrom dplyr filter select %>% mutate
#'
#' @noRd
insertKeywordTag <- function(id, kws, main.env) {
  # Add item
  new.kw <- unns(id)
  # Check if existing keyword
  # New keyword -- not in local.rv
  if(
    isFALSE(
      main.env$local.rv$keywords %>%
        dplyr::filter(keyword.set == kws) %>% 
        dplyr::select(keyword) %>%
        grepl(pattern = new.kw)
    )
  ) {
    main.env$local.rv$keywords <- 
      main.env$local.rv$keywords %>%
      dplyr::mutate(
        keyword = ifelse(
          keyword.set == kws,
          ifelse(
            keyword == "",
            new.kw,
            paste(keyword, new.kw, sep = ",")
          ),
          keyword
        )
      )
  } 
  # Create UI
  new.ui <- keywordTagUI(id)
  # Insert UI
  insertUI(
    selector = paste0("#inserthere_eal8_", kws),
    ui = new.ui,
    immediate = TRUE
  )
  # Set server
  keywordTag(new.kw, kws, main.env)
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
#' @importFrom dplyr %>% mutate
#'
#' @noRd
keywordTag <- function(id, kws, main.env) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove, {
      # remove the UI
      removeUI(selector = paste0("#", session$ns("kw_tag")), immediate = TRUE)
      # remove keyword
      main.env$local.rv$keywords <- main.env$local.rv$keywords %>%
        dplyr::mutate(
          keyword = ifelse(
            keyword.set == kws,
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