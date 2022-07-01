#' @import shiny
#' @importFrom data.table fread
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#'
#' @noRd
MiscUI <- function(id) {
  ns <- NS(id)

  return(
    fluidPage(
      HTML("
        <h5>DISCLAIMER</h5>
        <ul>
          <li>Unsupported special characters, symbols, formatting, or hyperlinks
          (URLs are acceptable).</li>
          <li>Any file selected will not be overwritten but will be used to fill
          in content (except files originating from Data Package itself).</li>
        </ul>
        "),
      shinyBS::bsCollapse(
        id = ns("Miscs"),
        ## Abstract ----
        shinyBS::bsCollapsePanel(
          title = withRedStar("Abstract"),
          value = 1,
          MiscellaneousUI(ns("abstract"))
        ),

        ## Methods ----
        shinyBS::bsCollapsePanel(
          title = withRedStar("Methods"),
          value = 2,
          MiscellaneousUI(ns("methods"))
        ),

        ## Keywords ----
        shinyBS::bsCollapsePanel(
          title = withRedStar("Keywords"),
          value = 3,
          tagList(
            tags$p("Organize your keywords in keyword sets. You can assign a
                     keyword thesaurus (controlled vocabulary or ontology) for
                     each of your keyword set. Press RETURN to input a
                   keyword."),
            actionButton(ns("add_kws"), "New keyword set", icon = icon("plus")),
            tags$div(id = "inserthere_eal8")
          )
        ),

        ## Temporal coverage ----
        shinyBS::bsCollapsePanel(
          title = withRedStar("Temporal coverage"),
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

        ## Additional Info ----
        shinyBS::bsCollapsePanel(
          title = "Additional Info - optional",
          value = 5,
          MiscellaneousUI(
            ns("additional.information"),
            help_label = tags$p(
              "If you have additional information that doesn't fall under the
              scope of the abstract or methods (e.g. a list of research articles
              or thesis derived from this dataset) about your dataset, you may
              share it here."
            )
          )
        )
      )
    ) # end of fluidPage
  ) # end of return
}


#' @import shiny
#' @importFrom shinyBS updateCollapse
#'
#' @noRd
Misc <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    if (main_env$dev) .browse_dev(main_env, 8)

    # Fill ----
    ## Abstract ----
    Miscellaneous("abstract", main_env)

    ## Methods ----
    Miscellaneous("methods", main_env)

    ## Keywords ----
    observeEvent(input$add_kws, {
      insertKeywordSet(session$ns(input$add_kws), main_env)
    })

    ### Setup ----
    # Initial UI
    observeEvent(main_env$EAL$page, {
      req(main_env$EAL$page == 8)
      req(nrow(main_env$local_rv$keywords) > 0)
      sapply(seq_row(main_env$local_rv$keywords), function(ind) {
        id <- main_env$local_rv$keywords$keyword.set[ind]

        insertKeywordSet(
          session$ns(id),
          main_env,
          .setup = TRUE
        )
      })
    },
    priority = -1
    )

    observeEvent(input$keywords, {
      req(input$keywords)

      main_env$local_rv$keywords$keyword <- unique(input$keywords) |>
        strsplit(",") |>
        unlist()

      output$thesaurus <- renderUI({
        validate(
          need(
            isContentTruthy(main_env$local_rv$keywords$keyword),
            "No keyword input"
          )
        )
        tagList(
          lapply(seq_along(main_env$local_rv$keywords$keyword), \ (kid) {
            keyword <- main_env$local_rv$keywords$keyword[kid]
            .val <- main_env$local_rv$keywords[["keywordThesaurus"]][kid]

            textInput(
              session$ns(paste0("thesaurus_for_", keyword)),
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
        need(main_env$local_rv$keywords$keyword, "No keyword input")
      )
      sapply(seq_along(main_env$local_rv$keywords$keyword), function(kid) {
        keyword <- main_env$local_rv$keywords$keyword[kid]
        input_id <- paste0("thesaurus_for_", keyword)
        .val <- if (isTruthy(input[[input_id]])) input[[input_id]] else ""

        main_env$local_rv$keywords$keywordThesaurus[kid] <- .val
      })
    },
    label = "EAL8: more keywords"
    )

    ## Temporal coverage ----
    ### Setup ----
    observeEvent(main_env$EAL$page, {
      req(main_env$EAL$page == 8)

      if (!is.null(main_env$local_rv$temporal_coverage))
        updateDateRangeInput(
          session,
          "temporal_coverage",
          start = main_env$local_rv$temporal_coverage[1],
          end = main_env$local_rv$temporal_coverage[2]
        )

    },
    priority = -1
    )

    ### Get ----
    observeEvent(input$temporal_coverage, {
      main_env$local_rv$temporal_coverage <- input$temporal_coverage
    },
    label = "EAL8: input temporal coverage"
    )

    ## Additional information ----
    Miscellaneous("additional.information", main_env)

    # Checks ----
    observe({
      invalidateLater(1000) # to keep placed before req()
      req(main_env$EAL$page == 8)

      ## Checks----
      check_abstract <- isHTMLTruthy(main_env$local_rv$abstract$content)
      check_methods <- isHTMLTruthy(main_env$local_rv$methods$content)
      check_keywords <- isContentTruthy(main_env$local_rv$keywords$keyword)
      check_temporal_coverage <- isTruthy(main_env$local_rv$temporal_coverage)

      ## Feedback ----
      shinyBS::updateCollapse(
        session,
        "Miscs",
        style = list(
          "1" = ifelse(check_abstract, "success", "warning"),
          "2" = ifelse(check_methods, "success", "warning"),
          "3" = ifelse(check_keywords, "success", "warning"),
          "4" = ifelse(check_temporal_coverage, "success", "warning"),
          "5" = "default"
        )
      )

      ## Synthetize  ----
      main_env$EAL$completed <- all(
        check_abstract,
        check_methods,
        check_keywords,
        check_temporal_coverage
      )
    },
    label = "EAL8: checks"
    )
  })
}
