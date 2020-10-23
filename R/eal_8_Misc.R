#' @import shiny
#' @importFrom tagsinput tagsTextInput
#' @importFrom data.table fread
#'
#' @noRd
MiscUI <- function(id, main.env) {
  ns <- NS(id)
  
  .metadata.path <- isolate(main.env$save.variable$SelectDP$dp.metadata.path)
  
  if (file.exists(paste0(.metadata.path, "/keywords.txt"))) {
    keywords <- data.table::fread(
      paste0(.metadata.path, "/keywords.txt"),
      data.table = FALSE, stringsAsFactors = FALSE
    )
  } else {
    keywords <- ""
  }
  if (isContentTruthy(keywords)) {
    kw <- keywords$keyword %>%
      strsplit(split = ",") %>%
      unlist() %>%
      paste(collapse = ",")
  }
  
  return(
    fluidPage(
      HTML("
        <h5>DISCLAIMER</h5>
        <ul>
          <li>Do not use special characters, symbols, formatting, or hyperlinks (URLs are acceptable).</li>
          <li>Any file selected will not be overwritten but will be used to fill
          in content (except files originating from DP itself).</li>
        </ul>
        "),
      bsCollapse(
        id = NS(id, "Miscs"),
        # * Abstract ----
        bsCollapsePanel(
          title = withRedStar("Abstract"),
          value = 1,
          MiscellaneousUI(NS(id, "abstract"))
        ),
        
        # * Methods ----
        bsCollapsePanel(
          title = withRedStar("Methods"),
          value = 2,
          MiscellaneousUI(NS(id, "methods"))
        ),
        
        # * Keywords ----
        bsCollapsePanel(
          title = withRedStar("Keywords"),
          value = 3,
          tagList(
            tags$p("Organize your keywords in keyword sets. You can assign a
                     keyword thesaurus (controlled vocabulary or ontology) for 
                     each of your keyword set. Press RETURN to input a keyword."),
            actionButton(NS(id, "add_kws"), "New keyword set", icon = icon("plus")),
            tags$div(id="inserthere_eal8")
          )
        ),
        
        # * Temporal coverage ----
        bsCollapsePanel(
          title = withRedStar("Temporal coverage"),
          value = 4,
          fluidRow(
            column(10,
                   offset = 1,
                   dateRangeInput(
                     NS(id, "temporal_coverage"),
                     "Dates between which dataset's content was produced",
                     max = Sys.Date(),
                     autoclose = FALSE
                   )
            )
          )
        ),
        
        # * Additional Info ----
        bsCollapsePanel(
          title = "Additional Info - optional",
          value = 5,
          MiscellaneousUI(NS(id, "additional.information"))
        )
      )
    ) # end of fluidPage
  ) # end of return
}


#' @import shiny
#' @importFrom shinyjs onclick enable disable
#' @importFrom data.table fread
#'
#' @noRd
Misc <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Variable initialization (deprecated)
    
    # Fill ----
    # * Abstract ====
    Miscellaneous("abstract", main.env)
    
    # * Methods ====
    Miscellaneous("methods", main.env)
    
    # * Keywords ====
    observeEvent(input$add_kws, {
      insertKeywordSet(session$ns(input$add_kws), main.env)
    })
    # ** Setup ----
    # Initial UI
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page == 8)
      req(nrow(main.env$local.rv$keywords) > 0)
      
      sapply(seq(nrow(main.env$local.rv$keywords)), function(ind) {
        id <- main.env$local.rv$keywords$keyword.set[ind]
        
        insertKeywordSet(
          session$ns(id),
          main.env,
          .setup = TRUE
        )
      })
    }, priority = -1)
    
    
    observeEvent(input$keywords, {
      req(input$keywords)
      
      main.env$local.rv$keywords$keyword <- unique(input$keywords) %>%
        strsplit(",") %>%
        unlist()
      
      output$thesaurus <- renderUI({
        validate(
          need(isContentTruthy(main.env$local.rv$keywords$keyword), "No keyword input")
        )
        tagList(
          lapply(seq_along(main.env$local.rv$keywords$keyword), function(kid) {
            keyword <- main.env$local.rv$keywords$keyword[kid]
            .val <- main.env$local.rv$keywords$keyword.thesaurus[kid]
            
            textInput(
              session$ns(paste0("thesaurus-for-", keyword)),
              keyword,
              value = if (isTruthy(.val)) .val else ""
            )
          })
        )
      })
    },
    label = "EAL8: input keywords"
    )
    
    # NOTE observers are still active after being deleted
    observe({
      validate(
        need(main.env$local.rv$keywords$keyword, "No keyword input")
      )
      sapply(seq_along(main.env$local.rv$keywords$keyword), function(kid) {
        keyword <- main.env$local.rv$keywords$keyword[kid]
        input_id <- paste0("thesaurus-for-", keyword)
        .val <- if (isTruthy(input[[input_id]])) input[[input_id]] else ""
        
        main.env$local.rv$keywords$keyword.thesaurus[kid] <- .val
      })
    },
    label = "EAL8: more keywords"
    )
    
    # * Temporal coverage ====
    if (!is.null(isolate(main.env$save.variable$Misc$temporal.coverage))) {
      main.env$local.rv$temporal.coverage <- isolate(main.env$save.variable$Misc$temporal.coverage)
      updateDateRangeInput(
        session,
        "temporal.coverage",
        start = main.env$local.rv$temporal.coverage[1],
        end = main.env$local.rv$temporal.coverage[2]
      )
    }
    observeEvent(input$temporal_coverage, {
      main.env$local.rv$temporal.coverage <- input$temporal_coverage
    },
    label = "EAL8: input temporal coverage"
    )
    
    # * Additional information ====
    Miscellaneous(
      "additional.information",
      main.env,
      help.label = tags$p(
        "If you have additional information that doesn't fall under the scope of
          the abstract or methods (e.g. a list of research articles or thesis
          derived from this dataset) about your dataset, you may share it here."
      )
    )
    
    # Saves ----
    observe({
      req(main.env$EAL$page == 8)
      
      invalidateLater(1000)
      # * Checks----
      check.abstract <- isHTMLTruthy(main.env$local.rv$abstract$content)
      check.methods <- isHTMLTruthy(main.env$local.rv$methods$content)
      check.keywords <- isContentTruthy(main.env$local.rv$keywords$keyword)
      check.temporal.coverage <- isTruthy(main.env$local.rv$temporal.coverage)
      
      # * Feedback ----
      shinyBS::updateCollapse(
        session,
        "Miscs",
        style = list(
          "1" = ifelse(check.abstract, "success", "warning"),
          "2" = ifelse(check.methods, "success", "warning"),
          "3" = ifelse(check.keywords, "success", "warning"),
          "4" = ifelse(check.temporal.coverage, "success", "warning")
        )
      )
      
      # * Synthetize  ----
      main.env$EAL$completed <- all(
        check.abstract,
        check.methods,
        check.keywords,
        check.temporal.coverage
      )
    },
    label = "EAL8: saves"
    )
    
    # Process data (deprecated)
  })
}
