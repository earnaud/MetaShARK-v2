#' @import shiny
#' @importFrom shinyjs hidden disabled
#'
#' @noRd
SelectDPUI <- function(id) {
  ns <- NS(id)

  # UI output
  return(
    fluidPage(
      title = "Organize data packages",
      tags$div(
        class = "headband",
        collapsibleUI(
          ns("usage"),
          "TUTORIAL: EML Assembly Line workflow",
          ... = tagList(
            tags$p(tags$b("Welcome in the EML Assembly line."), "This tool is
              basically designed as a package, embedded in Shiny within
              MetaShARK. This little helper aims to show you what awaits you in
              the further steps."),
            tags$p("After loading/creating a data package, a navigation bar will
              appear on your right. There features the following buttons:"),
            tags$ul(
              tags$li(tags$b("Quit: "), "click this to leave the edition of the
                current data package. You will be asked if you wanted to save
                the current changes. You can switch to other section of the app
                (e.g. documentation) without losing the current metadata."),
              tags$li(tags$b("Save: "), "click this to save the current changes
                in the filled metadata. This will write a save file in the data
                package directory. Then, metadata will only be lost with data
                package removal."),
              tags$li(tags$b("Next: "), "click this to continue your metadata
                filling. It will bring you to the next step."),
              tags$li(
                tags$b("Previous:"), "click this to come back to the
                previous step."
                # You can also use the steps",
                # tags$span(icon("circle"),style = "color: dodgerblue;"), "
                # markers to get to the desired step.")
              )
            )
          )
        )
      ),
      fluidRow(
        # Load existing DP ----
        column(
          6,
          tags$fieldset(
            tags$legend(
              tags$h4("Edit existing data package",
                style = "text-align:center"
              )
            ),
            tagList(
              # DP list
              textInput(ns("dp_search"), "", placeholder = "plants_project"),
              shinyjs::hidden(
                tags$div(
                  id = ns("dp_list_empty"),
                  helpText("No data package has been found.")
                )
              ),
              tags$div(
                radioButtons(
                  ns("dp_list"),
                  NULL,
                  choiceNames = c("None selected"),
                  choiceValues = c("")
                ),
                style = "overflow: scroll; width: auto; max-height: 500px;"
              ),
              # Load button
              actionButton(
                ns("dp_load"),
                "Load",
                icon = icon("folder-open")
              ),
              # Delete button
              actionButton(
                ns("dp_delete"),
                "Delete",
                icon = icon("minus-circle"),
                class = "redButton"
              ),
              # Download button
              downloadButton(
                ns("dp_download"),
                label = "Download .zip",
                icon = icon("file-download")
              )
            ) # end of tagList
          ) # end of fieldset
        ), # end of column
        # Create DP ----
        column(
          6,
          tags$fieldset(
            tags$legend(
              tags$h4("Create new data package",
                style = "text-align:center"
              )
            ),
            tagList(
              # Data package name
              textInput(
                ns("dp_name"),
                helpLabel("Data package name", "Files will be saved under this
                          name; e.g. 'france_plants'"),
                value = paste0(Sys.Date(), "_project")
              ),
              # Title
              textInput(
                ns("dp_title"),
                helpLabel("Dataset title", "The data published title (for
                          articles and so on) will be this one; e.g.
                          'Measures of petals size in Angiospermae of France'"),
                placeholder = "Plants of France"
              ),
              tags$p(
                "Only use alphanumerics, or one of:",
                HTML(paste(
                  tags$code("  "), tags$code("."), tags$code(","),
                  tags$code(":"), tags$code("_"), tags$code("-"),
                  sep = "&nbsp&nbsp"
                ))
              ),
              # License
              tags$div(
                id = "license-help",
                selectInput(
                  ns("license"),
                  helpLabel(
                    "Select an Intellectual Rights License:",
                    "Currently, CC-BY-4.0 ensures the author to be named when
                    her data is reused, while CC0 allows a totally free reuse of
                    the data."
                  ),
                  c("CC-BY-4.0" = "CCBY", "CC0" = "CC️0"),
                  multiple = FALSE
                )
              ),
              # DP creation
              shinyjs::disabled(
                actionButton(ns("dp_create"), "Create")
              )
            ) # end of taglist
          ) # end of fieldset
        ) # end column2
      ) # end fluidRow
    ) # end fluidPage
  ) # end return
}

#' @import shiny
#' @importFrom shinyjs toggle toggleState disable enable
#' @importFrom shinyFeedback hideFeedback showFeedbackDanger showFeedbackSuccess
#' @importFrom utils zip
#' @importFrom jsonlite read_json unserializeJSON
#'
#' @noRd
SelectDP <- function(id, main_env) {
  moduleServer(id, function(input, output, session) {
    # Dev zone
    if (main_env$dev) .browse_dev(main_env, 1)

    # Help button -- server call
    collapsible("usage")

    # variable initialization ----
    observeEvent(main_env$EAL$page, {
      req(main_env$EAL$page == 1)

      main_env$local_rv$dp_name <- reactive(input$dp_name)
      main_env$local_rv$dp_title <- reactive(input$dp_title)
      main_env$local_rv$dp_license <- reactive(input$license)
    },
    priority = -1,
    label = "EAL1: set DP"
    )

    # DP Load ====

    ## Render DP list ----
    # get files list
    files_poll <- reactiveDirReader(
      main_env$PATHS$eal_dp,
      session,
      pattern = "_emldp$"
    )

    # Extra reactive: makes sure files are returned reactively
    files_poll_reactive <- reactive(files_poll())

    # get search items
    search_pattern <- reactive(input$dp_search) |>
      debounce(300)

    # set it up
    observe({
        dp_list <- gsub(
          files_poll_reactive(),
          pattern = "_emldp$",
          replacement = ""
        )

        if (isContentTruthy(search_pattern())) {
          # do the filter
          dp_list <- dp_list[which(agrepl(search_pattern(), dp_list))]
        }
        changed <- isFALSE(identical(dp_list, main_env$local_rv$dp_list))

        # Changed and files present: update file list
        if (changed) {
          main_env$local_rv$dp_list <- dp_list

          if (isContentTruthy(dp_list)) {
            # update list
            updateRadioButtons(
              session,
              "dp_list",
              choiceNames = c("None selected", main_env$local_rv$dp_list),
              choiceValues = c("", gsub(
                " \\ (.*\\)$",
                "",
                main_env$local_rv$dp_list
              ))
            )
          }

          # toggle messages
          shinyjs::toggle("dp_list_empty", condition = !isContentTruthy(dp_list))
          shinyjs::toggle("dp_list", condition = isContentTruthy(dp_list))
        }
      },
      label = "EAL 1: dp list"
    )

    # toggle Load and Delete buttons
    observeEvent(input$dp_list, {
        valid_selected <- input$dp_list != ""
        shinyjs::toggleState("dp_load", condition = valid_selected)
        shinyjs::toggleState("dp_delete", condition = valid_selected)
        shinyjs::toggleState("dp_download", condition = valid_selected)
      },
      label = "EAL1: toggle dp buttons"
    )

    # DP create ====
    ## Check name ----
    observeEvent(input$dp_name, {
        shinyjs::disable("dp_create") # default
        shinyFeedback::hideFeedback("dp_name")

        # Ask for > 3 characters
        if (nchar(input$dp_name) <= 3) {
          shinyFeedback::showFeedbackDanger(
            "dp_name",
            "Not enough characters."
          )
        } else # Asks only for valid characters
        if (isFALSE(grepl("^[[:alnum:]_-]+$", input$dp_name))) {
          shinyFeedback::showFeedbackDanger(
            "dp_name",
            "Only use alphanumeric, '_' and '-' characters."
          )
        } else # dp already exists
        if (input$dp_name %in% main_env$local_rv$dp_list) {
          shinyFeedback::showFeedbackDanger(
            "dp_name",
            "Already used."
          )
        } else {
          shinyFeedback::showFeedbackSuccess("dp_name")
          shinyjs::enable("dp_create")
        }
      },
      label = "EAL1: dp name input"
    )

    ## Check title ----
    observeEvent(input$dp_title, {
        shinyjs::disable("dp_create")
        shinyFeedback::hideFeedback("dp_title")

        # Ask for > 3 characters
        if (nchar(input$dp_title) <= 3) {
          shinyFeedback::showFeedbackDanger("dp_title", "Not enough characters.")
        } else # Asks only for valid characters
        if (isFALSE(grepl("^[[:alnum:]\\ \\.,:_-]+$", input$dp_title))) {
          shinyFeedback::showFeedbackDanger(
            "dp_title",
            "Invalid characters used."
          )
        } else { # do not check against other titles # NOTE shall we ?
          shinyFeedback::showFeedbackSuccess(
            "dp_title"
          )
          shinyjs::enable("dp_create")
        }
      },
      label = "EAL1: dp title input"
    )

    # DP management - on clicks ----
    ## Check Create DP ----
    observe({
        shinyjs::toggleState(
          "dp_create",
          condition = isContentTruthy(main_env$local_rv$dp_name()) &&
            isContentTruthy(main_env$local_rv$dp_title()) &&
            main_env$local_rv$dp_license() %in% c("CC0", "CCBY")
        )
      },
      priority = -1
    )

    ## Create DP ----
    observeEvent(input$dp_create, {
        req(input$dp_create)
        req(main_env$local_rv$dp_name())

        # save in empty dedicated variable
        main_env$save_variable <- initReactive(
          "emlal",
          main_env$save_variable,
          main_env
        )
        # Next page will be triggered in this particular saveReactive
        saveReactive(main_env, main_env$EAL$page)
      },
      label = "EAL1: create DP"
    )

    ## Load DP ----
    observeEvent(input$dp_load, {
      req(input$dp_list)
      shinyjs::disable("dp_load")

      # variable operation - legibility purpose
      dp <- input$dp_list
      path <- paste0(main_env$PATHS$eal_dp, dp, "_emldp")

      # verbose
      showNotification(
        paste("Loading:", dp),
        type = "message"
      )

      # read variables
      main_env$save_variable <- initReactive(
        "emlal",
        main_env$save_variable,
        main_env
      )
      # Read json in a tmp variable to let it be curated
      .tmp <- jsonlite::read_json(paste0(path, "/", dp, ".json"))[[1]] |>
        jsonlite::unserializeJSON()

      # save_variable adaptations
      # TODO remove this later
      .tmp <- renameList(.tmp, "\\.(?!txt$)", "_")

      # - keywords.thesaurus replaced by keywordThesaurus
      if ("Misc" %in% names(.tmp) &&
          "keyword.thesaurus" %in% names(.tmp$Misc$keywords)) {
        names(.tmp$Misc$keywords)["keyword.thesaurus"] <- "keywordThesaurus"
      }

      # - eal/metafin difference
      if (identical(names(.tmp), c("metafin", "emlal"))) {
        .tmp <- .tmp$emlal
      }

      # - creator
      if (isFALSE("creator" %in% names(.tmp) && isTruthy(.tmp$creator))) {
        .tmp$creator <- main_env$SETTINGS$user
      }

      # - history
      .tmp$history <- sapply(.tmp$history, \ (h) {
        switch(h,
               create = "SelectDP",
               DataFiles = "Data Files",
               attributes = "Attributes",
               CustomUnits = NULL,
               CatVars = "Categorical Variables",
               GeoCov = "Geographic Coverage",
               TaxCov = "Taxonomic Coverage",
               h # unchanged
        )
      }) |>
        unname()

      # - for elder DP, add use_catvars boolean variable
      if ("Attributes" %in% .tmp$history &&
          isFALSE("use_catvars" %in% names(.tmp$Attributes))) {
        .tmp$Attributes$use_catvars <- any(
          sapply(
            main_env$save_variable$Attributes$content,
            \ (.table) any(.table$class == "categorical")
          )
        )
      }

      # Once prepared, properly merge tmp and save_variables
      main_env$save_variable <- setSaveVariable(.tmp, main_env$save_variable)

      # Update paths from another file system
      ## selectDP
      sapply(names(main_env$save_variable$SelectDP), \ (.dp_item) {
        main_env$save_variable$SelectDP[[.dp_item]] <- gsub(
          pattern = "^.*/dataPackagesOutput/emlAssemblyLine/",
          replacement = main_env$PATHS$eal_dp,
          main_env$save_variable$SelectDP[[.dp_item]]
        )
      })

      ## datafiles
      if (isContentTruthy(main_env$save_variable$DataFiles)) {
        sapply(names(main_env$save_variable$DataFiles), \ (col) {
          main_env$save_variable$DataFiles[, col] <- gsub(
            pattern = ".*/dataPackagesOutput/emlAssemblyLine/",
            replacement = main_env$PATHS$eal_dp,
            main_env$save_variable$DataFiles[, col]
          )
          if (col == "size") {
            main_env$save_variable$DataFiles[, col] <- as.integer(
              main_env$save_variable$DataFiles[, col]
            )
          }
        })
      }

      ## miscellaneous
      if (isContentTruthy(main_env$save_variable$Misc$abstract)) {
        main_env$save_variable$Misc$abstract <- gsub(
          ".*/dataPackagesOutput/emlAssemblyLine/",
          main_env$PATHS$eal_dp,
          main_env$save_variable$Misc$abstract
        )
        main_env$save_variable$Misc$methods <- gsub(
          ".*/dataPackagesOutput/emlAssemblyLine/",
          main_env$PATHS$eal_dp,
          main_env$save_variable$Misc$methods
        )
        main_env$save_variable$Misc$additional_information <- gsub(
          ".*/dataPackagesOutput/emlAssemblyLine/",
          main_env$PATHS$eal_dp,
          main_env$save_variable$Misc$additional_information
        )
      }

      # resume at saved page
      if (main_env$save_variable$step == 1) { # crashed on going to next
        main_env$EAL$page <- main_env$save_variable$step + 1
        main_env$EAL$history <- main_env$VALUES$steps[1:main_env$EAL$page]
      } else { # expected normal way
        main_env$EAL$page <- main_env$save_variable$step
        main_env$EAL$history <- main_env$save_variable$history
      }
      main_env$EAL$old_page <- 1

      shinyjs::enable("dp_load")
    },
    ignoreInit = TRUE,
    label = "EAL1: load DP"
    )

    ## Delete DP ----
    observeEvent(input$dp_delete, {
        req(isTruthy(input$dp_list))

        # variable operation - legibility purpose
        dp <- input$dp_list

        # actions
        showModal(
          modalDialog(
            title = "Delete data package?",
            ... = paste("Are you sure to delete", dp, "?"),
            easyClose = FALSE,
            footer = tagList(
              modalButton("No"),
              actionButton(
                session$ns("delete_confirm"), "Yes",
                class = "redButton"
              )
            ) # end footer
          ) # end modalDialog
        ) # end showModal
      },
      label = "EAL1: delete DP"
    )

    # If deletion is confirmed
    observeEvent(input$delete_confirm, {
        # variable operation - legibility purpose
        dp <- gsub(" \\ (public\\)", "", input$dp_list)
        path <- paste0(main_env$PATHS$eal_dp, dp, "_emldp")

        # verbose
        removeModal()
        showNotification(
          paste("Deleting:", dp, sep = "")
        )

        # actions
        unlink(path, recursive = TRUE)
      },
      label = "EAL1: confirm delete DP"
    )

    ## Manage DP download ----
    output$dp_download <- downloadHandler(
      filename = function() {
        paste0(input$dp_list, "_emldp.zip")
      },
      content = function(file) {
        .path <- getwd()
        setwd(main_env$PATHS$eal_dp)
        utils::zip(
          zipfile = file,
          files = dir(
            gsub(
              "/+",
              "/",
              dir(
                ".",
                full.names = TRUE,
                pattern = input$dp_list
              )
            ),
            recursive = TRUE,
            full.names = TRUE
          )
        )
        setwd(.path)
      },
      contentType = "application/zip"
    )
  })
}
