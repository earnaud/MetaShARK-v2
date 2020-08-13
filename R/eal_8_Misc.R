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
  if (checkTruth(keywords)) {
    kw <- keywords$keyword %>%
      strsplit(split = ",") %>%
      unlist() %>%
      paste(collapse = ",")
  }

  return(
    fluidPage(
      fluidRow(
        HTML("
            <h5>DISCLAIMER</h5>
            <ul>
              <li>Do not use special characters, symbols, formatting, or hyperlinks (URLs are acceptable).</li>
              <li>Any file selected will not be overwritten but will be used as a template.</li>
            </ul>
          "),
        bsCollapse(
          id = NS(id, "Miscs"),

          # * Abstract ----
          bsCollapsePanel(
            title = withRedStar("Abstract"),
            value = 1,
            MiscellaneousUI(
              NS(id, "abstract"),
              value = if (file.exists(paste0(.metadata.path, "/abstract.txt"))) {
                readPlainText(
                  paste0(.metadata.path, "/abstract.txt")
                )
              }
            )
          ),

          # * Methods ----
          bsCollapsePanel(
            title = withRedStar("Methods"),
            value = 2,
            MiscellaneousUI(
              NS(id, "methods"),
              value = if (file.exists(paste0(.metadata.path, "/methods.txt"))) {
                readPlainText(
                  paste0(.metadata.path, "/methods.txt")
                )
              }
            )
          ),

          # * Keywords ----
          bsCollapsePanel(
            title = withRedStar("Keywords"),
            value = 3,
            tagList(
              column(
                6,
                tagsinput::tagsTextInput(
                  NS(id, "keywords"),
                  tags$p("List the keywords that best describe your dataset.
                    Type a 'tab' to separate each keyword."),
                  value = if (checkTruth(keywords)) keywords[, 1] else c()
                )
              ),
              column(
                6,
                tags$h4("Associated thesaurus"),
                tags$p("NOTE: use of thesaurus will be improved. Currently,
                    no control is made about thesaurus input field and this
                    can be invalided."),
                tags$p("You may associate a thesaurus to each keyword."),
                uiOutput(NS(id, "thesaurus"))
              )
            )
          ),

          # * Temporal coverage ----
          bsCollapsePanel(
            title = "Temporal coverage",
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
            title = "Additional Info",
            value = 5,
            MiscellaneousUI(
              NS(id, "additional.information"),
              help.label = tags$p(
                "If you have additional information that doesn't fall under the scope of the abstract or methods (e.g. a list of research articles or theses derived from this dataset) about your dataset, you may share it here."
              ),
              value = if (file.exists(paste0(.metadata.path, "/additional_info.txt"))) {
                readPlainText(
                  paste0(.metadata.path, "/additional_info.txt")
                )
              }
            )
          )
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
Misc <- function(id, full.id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Variable initialization ----

    # Fill ----
    # * Abstract ====
    Miscellaneous(
      "abstract", 
      main.env$save.variable,
      rv = main.env$local.rv
    )

    # * Methods ====
    Miscellaneous(
      "methods",
      main.env$save.variable, 
      rv = main.env$local.rv
    )

    # * Keywords ====
    observeEvent(input$keywords, {
      req(input$keywords)

      main.env$local.rv$keywords$keyword <- unique(input$keywords) %>%
        strsplit(",") %>%
        unlist()

      output$thesaurus <- renderUI({
        validate(
          need(checkTruth(main.env$local.rv$keywords$keyword), "No keyword input")
        )
        tagList(
          lapply(seq_along(main.env$local.rv$keywords$keyword), function(kid) {
            keyword <- main.env$local.rv$keywords$keyword[kid]
            .val <- main.env$local.rv$keywords$keyword.thesaurus[kid]

            textInput(
              NS(full.id, paste0("thesaurus-for-", keyword)),
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
      main.env$local.rv$temporal.coverage <- main.env$save.variable$Misc$temporal.coverage
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
      main.env$save.variable,
      rv = main.env$local.rv
    )

    # Saves ----
    observe({
      req(main.env$EAL$page == 8)
      
      main.env$EAL$completed <- all(
        isTruthy(main.env$local.rv$abstract$content) &&
          isTruthy(main.env$local.rv$methods$content) &&
          isTruthy(main.env$local.rv$keywords$keyword) &&
          isTruthy(main.env$local.rv$temporal.coverage)
      )
    },
    label = "EAL8: saves"
    )

    # Process data ----
    observeEvent(main.env$EAL$.next,
      {
        req(main.env$EAL$current == "Miscellaneous")
        
        saveReactive(main.env)
      },
      label = "EAL8: process data",
      priority = 1,
      ignoreInit = TRUE
    )
  })
}
