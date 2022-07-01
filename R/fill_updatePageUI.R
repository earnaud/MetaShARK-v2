# This function has only one use context
updatePageUI <- function(main_env, session) {
  devmsg("page %s", main_env$EAL$page, tag = "fill_updatePageUI.R")

  do.call(
    args = list(main_env = main_env, session = session),
    what = switch(
      main_env$EAL$page,
      # Data files ====
      `2` = \ (main_env, session) {

        if (isContentTruthy(main_env$local_rv$data_files) &&
            nrow(main_env$local_rv$data_files) > 0)
          sapply(seq_row(main_env$local_rv$data_files), function(row.id) {
            insertDataFileInput(
              session$ns(sprintf("_%s", as.character(row.id))),
              main_env
            )
          })

      },
      # Attributes ====
      `3` = \ (main_env, session) {
        ## Update tree ----
        if (isContentTruthy(main_env$local_rv$tree.content)) {
          if (main_env$dev)
            devmsg("update tree", tag = "attributes")

          shinyTree::updateTree(
            session = session,
            treeId = "tree",
            data = main_env$local_rv$tree.content
          )
        }

        # Custom units ----
        # setup a new server each time page 3 is up
        DataEditR::dataEditServer(
          "custom_units",
          data = reactive(main_env$local_rv$custom_units$table),
          col_edit = FALSE,
          # col_options ?
          col_names = FALSE,
          quiet = TRUE
        )
      },
      # Catvars ====
      `4` = \ (main_env, session) {
        updateSelectInput(
          session,
          "files",
          choices = names(main_env$local_rv$cv_tables)
        )
      },
      # Geo cov ====
      `5` = \ (main_env, session) {
        if (isContentTruthy(main_env$local_rv$method)) {
          shinyWidgets::updateMaterialSwitch(
            session,
            "method",
            switch(
              main_env$local_rv$method,
              columns = FALSE,
              custom = TRUE
            )
          )

          # Display correct UI
          shinyjs::toggle(
            "columns_input",
            condition = main_env$local_rv$method == "columns"
          )

          shinyjs::toggle(
            "custom_input",
            condition = main_env$local_rv$method == "custom"
          )
        }
      },
      # Tax cov ====
      `6` = \ (main_env, session) {

        ## setup taxa column choices ----
        # shortcut for attributes content
        .att <- main_env$save_variable$Attributes$content

        # Set choices for selectInput -- reuse & filter Attributes
        .choice <- main_env$local_rv$taxa_choices <- list()
        sapply(names(.att), function(.md_file) {
          .data_file <- main_env$save_variable$DataFiles |>
            filter(grepl(.md_file, metadatapath)) |>
            select(datapath) |>
            unlist() |>
            basename()
          # Set sites
          .choice[[.data_file]] <<- .att[[.md_file]] |>
            as.data.frame() |>
            dplyr::filter(class %in% c("character", "categorical")) |>
            dplyr::select(attributeName) |>
            unlist()
          .choice[[.data_file]] <<- paste(
            .data_file, .choice[[.data_file]],
            sep = "/"
          ) |>
            setNames(nm = .choice[[.data_file]])
        })

        # Set value -- read from saved
        .value <- if (isContentTruthy(main_env$save_variable$TaxCov)) {
          paste(
            main_env$local_rv$taxa_col,
            main_env$local_rv$taxa_col,
            sep = "/"
          ) |>
            setNames(nm = main_env$local_rv$taxa_col)
        }

        # UI itself
        updateSelectInput(
          session,
          "taxa.col",
          choices = .choice,
          selected = .value
        )

        # setup taxa name type ----
        .value <- main_env$local_rv$taxa_name_type
        if (isTRUE(.value == "both")) {
          .value <- c("scientific", "common")
        } else if (isFALSE(.value %in% c("scientific", "common"))) {
          .value <- NULL
        }

        updateCheckboxGroupInput(
          session, "taxa_name_type",
          selected = .value
        )

        # setup taxa authorities ----
        .taxa_authorities <- main_env$FORMATS$taxa_authorities
        .choices <- .taxa_authorities$authority
        .value <- if (isTruthy(main_env$local_rv$taxa_authority))
          .taxa_authorities |>
            dplyr::filter(id == main_env$local_rv$taxa_authority) |>
            dplyr::select(authority)

        updateSelectInput(
          session,
          "taxa_authority",
          "Select taxonomic authority.ies",
          choices = .choices,
          selected = .value
        )

      },
      # Personnel ====
      `7` = \ (main_env, session) {

        # Remove inserted UIs
        sapply(seq_row(main_env$local_rv$Personnel), function(ind) {
          sapply(
            paste0(main_env$local_rv$Personnel$id, "-container"),
            function(id) {
              removeUI(sprintf("#%s", session$ns(id)), immediate = TRUE)
            }
          )
        })

        # Insert fresh new UIs
        if (nrow(main_env$local_rv$Personnel) > 0) {
          sapply(seq_row(main_env$local_rv$Personnel), function(ind) {
            row <- main_env$local_rv$Personnel[ind, ]

            insertPersonnelInput(session$ns(row$id), main_env)
          })
        }

      },
      # Misc ====
      `8` = \ (main_env, session) {
        ## Add keywords UIs ----
        if (nrow(main_env$local_rv$keywords) > 0)
          sapply(seq_row(main_env$local_rv$keywords), function(ind) {
            id <- main_env$local_rv$keywords$keyword.set[ind]

            insertKeywordSet(
              session$ns(id),
              main_env,
              .setup = TRUE
            )
          })

        # Update temporal coverage ----
        if (!is.null(main_env$local_rv$temporal_coverage))
          updateDateRangeInput(
            session,
            "temporal_coverage",
            start = main_env$local_rv$temporal_coverage[1],
            end = main_env$local_rv$temporal_coverage[2]
          )

      },
      `9` = {
        if (isTRUE(main_env$local_rv$eml_written)) {
          shinyjs::enable("publish")
          shinyjs::enable("download_data_package")
        }
      }
    )
  )
}