#' @import shiny
#' @importFrom tagsinput tagsTextInput
#' @importFrom data.table fread
#' 
#' @noRd
MiscUI <- function(id, title, dev, save.variable) {
  ns <- NS(id)

  keywords <- fread(
    paste0(save.variable$emlal$SelectDP$dp.metadata.path, "/keywords.txt"),
    data.table = FALSE, stringsAsFactors = FALSE
  )
  if (checkTruth(keywords)) {
    kw <- keywords$keyword %>%
      strsplit(split = ",") %>%
      unlist %>%
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
          id = ns("Miscs"),

          # * Abstract -----------------------------------------------------
          bsCollapsePanel(
            title = withRedStar("Abstract"),
            value = 1,
            MiscellaneousUI(
              ns("abstract"),
              value = readPlainText(
                paste0(save.variable$emlal$SelectDP$dp.metadata.path, "/abstract.txt")
              )
            )
          ),

          # * Methods -----------------------------------------------------
          bsCollapsePanel(
            title = withRedStar("Methods"),
            value = 2,
            MiscellaneousUI(
              ns("methods"),
              value = readPlainText(
                paste0(save.variable$emlal$SelectDP$dp.metadata.path, "/methods.txt")
              )
            )
          ),

          # * Keywords -----------------------------------------------------
          bsCollapsePanel(
            title = withRedStar("Keywords"),
            value = 3,
            tagList(
              column(
                6,
                tagsinput::tagsTextInput(
                  ns("keywords"),
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
                uiOutput(ns("thesaurus"))
              )
            )
          ),

          # * Temporal coverage -----------------------------------------------------
          bsCollapsePanel(
            title = "Temporal coverage",
            value = 4,
            fluidRow(
              column(10,
                offset = 1,
                dateRangeInput(
                  ns("temporal_coverage"),
                  "Dates between which dataset's content was produced",
                  max = Sys.Date(),
                  autoclose = FALSE
                )
              )
            )
          ),

          # * Additional Info -----------------------------------------------------
          bsCollapsePanel(
            title = "Additional Info",
            value = 5,
            MiscellaneousUI(
              ns("additional.information"),
              help.label = tags$p(
                "If you have additional information that doesn't fall under the scope of the abstract or methods (e.g. a list of research articles or theses derived from this dataset) about your dataset, you may share it here."
              ),
              value = readPlainText(
                paste0(save.variable$emlal$SelectDP$dp.metadata.path, "/additional_info.txt")
              )
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
Misc <- function(input, output, session, save.variable, main.env, NSB) {
  ns <- session$ns

  if (main.env$DEV) {
    shinyjs::onclick("dev",
      {
        req(main.env$EAL$navigate == 8)
        browser()
      },
      asis = TRUE
    )
  }

  # Variable initialization -----------------------------------------------------
  kw <- fread(
    paste0(save.variable$emlal$SelectDP$dp.metadata.path, "/keywords.txt"),
    data.table = FALSE, stringsAsFactors = FALSE
  )

  rv <- reactiveValues(
    # Abstract
    abstract = reactiveValues(
      content = character(),
      file = paste(
        isolate(save.variable$emlal$SelectDP$dp.metadata.path),
        "abstract.txt",
        sep = "/"
      )
    ),
    # Methods
    methods = reactiveValues(
      content = character(),
      file = paste(
        isolate(save.variable$emlal$SelectDP$dp.metadata.path),
        "methods.txt",
        sep = "/"
      )
    ),
    # Keywords
    keywords = reactiveValues(
      keyword = kw$keyword,
      keyword.thesaurus = kw$keyword.thesaurus
    ),
    # Temporal coverage
    temporal.coverage = c(Sys.Date() - 1, Sys.Date()),
    # Additional information
    additional.information = reactiveValues(
      content = character(),
      file = paste(
        isolate(save.variable$emlal$SelectDP$dp.metadata.path),
        "additional_info.txt",
        sep = "/"
      )
    )
  )

  # Fill -----------------------------------------------------
  # * Abstract ====
  rv$abstract <- callModule(
    Miscellaneous,
    "abstract",
    save.variable,
    rv = rv$abstract
  )

  # * Methods ====
  rv$methods <- callModule(
    Miscellaneous,
    "methods",
    save.variable,
    rv = rv$methods
  )

  # * Keywords ====
  observeEvent(input$keywords, {
    req(input$keywords)

    rv$keywords$keyword <- unique(input$keywords)

    output$thesaurus <- renderUI({
      validate(
        need(checkTruth(rv$keywords$keyword), "No keyword input")
      )
      tagList(
        lapply(seq_along(rv$keywords$keyword), function(kid) {
          keyword <- rv$keywords$keyword[kid]
          .val <- rv$keywords$keyword.thesaurus[kid]

          textInput(
            ns(paste0("thesaurus-for-", keyword)),
            keyword,
            value = if (isTruthy(.val)) .val else ""
          )
        })
      )
    })
  })

  # NOTE observers are still active after being deleted
  observe({
    validate(
      need(rv$keywords$keyword, "No keyword input")
    )
    sapply(seq_along(rv$keywords$keyword), function(kid) {
      keyword <- rv$keywords$keyword[kid]
      input_id <- paste0("thesaurus-for-", keyword)
      .val <- if (isTruthy(input[[input_id]])) input[[input_id]] else ""

      rv$keywords$keyword.thesaurus[kid] <- .val
    })
  })

  # * Temporal coverage ====
  if (!is.null(save.variable$emlal$Misc$temporal.coverage)) {
    rv$temporal.coverage <- save.variable$emlal$Misc$temporal.coverage
    updateDateRangeInput(
      session,
      "temporal.coverage",
      start = rv$temporal.coverage[1],
      end = rv$temporal.coverage[2]
    )
  }
  observeEvent(input$temporal_coverage, {
    rv$temporal.coverage <- input$temporal_coverage
  })

  # * Additional information ====
  rv$additional.information <- callModule(
    Miscellaneous,
    "additional.information",
    save.variable,
    rv = rv$additional.information
  )

  # Saves -----------------------------------------------------
  observe({
    main.env$EAL$current[2] <- all(
      isTruthy(rv$abstract$content()) &&
        isTruthy(rv$methods$content()) &&
        isTruthy(rv$keywords$keyword) &&
        isTruthy(rv$temporal.coverage)
    )
  })

  observeEvent(NSB$SAVE,
    {
      req(main.env$EAL$current[1] == "Miscellaneous")

      save.variable <- saveReactive(
        save.variable = savevar,
        rv = list(Misc = rv)
      )
    },
    ignoreInit = TRUE
  )

  # Process data -----------------------------------------------------
  observeEvent(NSB$NEXT,
    {
      req(main.env$EAL$current[1] == "Miscellaneous")

      save.variable <- saveReactive(
        save.variable = savevar,
        rv = list(Misc = rv)
      )
    },
    priority = 1,
    ignoreInit = TRUE
  )

  # Output -----------------------------------------------------
  return(save.variable)
}
