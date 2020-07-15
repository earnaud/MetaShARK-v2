#' @import shiny
#' @importFrom shinyFiles shinyDirButton
#' 
#' @noRd
SelectDPUI <- function(id, main.env) {
  ns <- NS(id)
  
  # UI output
  return(
    fluidPage(
      title = "Organize data packages",
      fluidRow(
        collapsibleUI(
          NS(id, "usage"),
          "TUTORIAL: EML Assembly Line workflow",
          ... = tagList(
            tags$p(tags$b("Welcome in the EML Assembly line."), "This tool is
              basically designed as a package, embedded in Shiny within MetaShARK.
              This little helper aims to show you what awaits you in the further 
              steps."),
            tags$hr(),
            tags$p("After loading/creating a data package, a navigation bar will 
              appear on your right. There features the following buttons:"),
            tags$ul(
              tags$li(tags$b("Quit: "), "click this to leave the edition of the
                current data package. You will be asked if you wanted to save the
                current changes. You can switch to other section of the app (e.g.
                documentation) without losing the current metadata."),
              tags$li(tags$b("Save: "), "click this to save the current changes 
                in the filled metadata. This will write a save file in the data
                package directory. Then, metadata will only be lost with data 
                package removal."),
              tags$li(tags$b("Next: "), "click this to continue your metadata
                filling. It will bring you to the next step."),
              tags$li(tags$b("Previous:"), "click this to come back to one of
                the previous steps. You can also use the steps", tags$span(
                  icon("circle"),
                  style = "color: dodgerblue;"
                ), " markers to get
                to the desired step.")
            )
          )
        )
      ),
      # Data package location ----
      # if (!isTRUE(server)) {
      #   tagList(
      #     fluidRow(
      #       column(4,
      #         if (isTRUE(server)) {
      #           tags$b("Data Package will be saved in:")
      #         } else {
      #           shinyDirButton(
      #             NS(id, "dp_location"),
      #             "Choose directory",
      #             "DP save location",
      #             icon = icon("folder-open")
      #           )
      #         }
      #       ),
      #       column(8,
      #         textOutput(NS(id, "dp_location")),
      #         style = "text-align: left;"
      #       ),
      #       class = "inputBox"
      #     ),
      #     fluidRow(
      #       tags$p("This is the location where your data packages will be
      #     saved. A folder will be created, respectively named
      #     after your input.")
      #     )
      #   )
      # } else {
      hr(),
      # },
      fluidRow(
        # Load existing DP ----
        column(6,
          tags$h4("Edit existing data package",
            style = "text-align:center"
          ),
          uiOutput(NS(id, "dp_list"))
        ),
        # Create DP ----
        column(6,
          tags$h4("Create new data package",
            style = "text-align:center"
          ),
          checkboxInput(
            NS(id, "quick"),
            tagList(
              tags$b("Quick mode"),
              "Most fields will be automatically filled"
            ),
            value = TRUE
          ),
          # Data package title
          textInput(
            NS(id, "dp_name"),
            "Data package name",
            placeholder = paste0(Sys.Date(), "_project")
          ),
          textInput(
            NS(id, "dp_title"),
            "Dataset title",
            placeholder = "Any title is a title"
          ),
          tags$div(
            id = "license-help",
            selectInput(
              NS(id, "license"),
              "Select an Intellectual Rights License:",
              c("CCBY", "CC0"),
              multiple = FALSE
            )
          ),
          HTML("License: <br>
               <b>CC0:</b> public domain. <br>
               <b>CC-BY-4.0:</b> open source with authorship. <br>
               For more details, visit Creative Commons."),
          # DP creation
          uiOutput(NS(id, "dp_create"))
        ) # end column2
      ) # end fluidRow
    ) # end fluidPage
  ) # end return
}

#' @import shiny
#' @importFrom shinyFiles getVolumes shinyDirChoose parseDirPath
#' @importFrom shinyjs enable disable onclick
#' @importFrom EMLassemblyline template_directories template_core_metadata
#' @importFrom jsonlite read_json unserializeJSON
#' 
#' @noRd
SelectDP <- function(id, main.env){
  moduleServer(id, function(input, output, session) {
    save.variable <- main.env$save.variable
    ns <- session$ns
    
    collapsible("usage")
    
    # variable initialization ----
    rv <- reactiveValues(
      dp.location = main.env$PATHS$eal.dp,
      dp.name = character(),
      dp.title = character(),
      dp.list = NULL,
      dp.license = NULL
    )
    
    # DP location ----
    observeEvent(input$dp_location,
      {
        req(input$dp_location)
        rv$dp.location <- input$dp_location
      },
      label = "EAL1: input dp location"
    )
    
    # Render selected DP location
    output$dp_location <- renderText({
      rv$dp.location
    })
    
    # DP load ----
    # reset input if user comes back on this screen
    # fetch list of DP at selected location
    observeEvent({
      rv$dp.location
      main.env$SETTINGS$logged
    }, {
      # Build full DP list
      .dp.path <- gsub(
        "/+",
        "/",
        list.files(
          rv$dp.location,
          pattern = "_emldp$", 
          full.names = TRUE
        )
      )
      .dp.list <- basename(.dp.path)
      
      # Note for local version: shall check for index (create it if not exists)
      # Index non-indexed DP
      if(length(.dp.list) > nrow(main.env$DP.LIST)){
        sapply(.dp.list, function(dp){
          .index <- main.env$DP.LIST
          if(!dp %in% .index$name){
            .tmp <- data.frame(
              creator.orcid = main.env$SETTINGS$user,
              name = dp,
              title = dp,
              path = gsub("/+","/", .dp.path[which(.dp.list == dp)]),
              stringsAsFactors = FALSE
            )
            
            .index <- rbind(.index, .tmp)
            assign("DP.LIST", .index, envir = main.env)
          }
        })
      }
      
      # Reduce list by property
      # - gather public and user's list = retrieve non-public, non-user DP
      .out <- main.env$DP.LIST %>% 
        dplyr::filter(creator.orcid == "public") %>%
        dplyr::select(name) %>% 
        lapply(paste, "(public)") %>% 
        unlist
      if(isTRUE(main.env$SETTINGS$logged)){
        .out <- c(
          .out,
          main.env$DP.LIST %>% 
            dplyr::filter(creator.orcid == main.env$SETTINGS$user) %>%
            dplyr::select(name) %>% 
            lapply(paste, "(public)") %>% 
            unlist
        )
      }
      
      if(length(.out) != 0)
        rv$dp.list <- sub("_emldp", "", .out) %>% unname
      else
        rv$dp.list <- NULL
    },
      label = "EAL1: build dp list"
    )
    
    # Render list of DP at selected location
    output$dp_list <- renderUI({
      validate(
        need(
          isTruthy(rv$dp.list),
          "No data package has been written."
        )
      )
      tagList(
        radioButtons(
          NS(id, "dp_list"),
          NULL,
          choiceNames = c("None selected", rv$dp.list),
          choiceValues = c("", rv$dp.list)
        ),
        actionButton(
          NS(id, "dp_load"),
          "Load", 
          icon = icon("folder-open")
        ),
        actionButton(
          NS(id, "dp_delete"), 
          "Delete", 
          icon = icon("minus-circle"),
          class = "redButton"
        ),
        downloadButton(
          NS(id, "dp_download"), 
          label = "Download .zip",
          icon = icon("file-download")
        )
      )
    })
    
    # Save updated index ----
    observeEvent(main.env$DP.LIST, {
      data.table::fwrite(
        main.env$DP.LIST, 
        isolate(main.env$PATHS$eal.dp.index), 
        sep = "\t"
      )
    })
    
    # Manage DP download ----
    output$dp_download <- downloadHandler(
      filename = function() {
        paste0(input$dp_list, "_emldp.zip")
      },
      content = function(file) {
        .path <- getwd()
        setwd(main.env$PATHS$eal.dp)
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
    
    # toggle Load and Delete buttons
    observeEvent(input$dp_list,
      {
        if (input$dp_list != "") {
          shinyjs::enable("dp_load")
          shinyjs::enable("dp_delete")
          shinyjs::enable("dp_download")
        }
        else {
          shinyjs::disable("dp_load")
          shinyjs::disable("dp_delete")
          shinyjs::disable("dp_download")
        }
      },
      label = "EAL1: UX hs"
    )
    
    # DP create ----
    # check name input
    rv$valid.name <- FALSE
    output$dp_create <- renderUI({
      rv$valid.name <- FALSE
      validate(
        need(
          nchar(input$dp_name) > 3,
          "Please type a name with at least 3 characters."
        ),
        need(
          grepl("^[[:alnum:]_-]+$", input$dp_name)
          && nzchar(input$dp_name),
          "Only authorized characters are alphanumeric, '_' (underscore) and '-' (hyphen)."
        ),
        need(
          input$dp_name != ""
          && !(input$dp_name %in% rv$dp.list),
          "This name is already used: change either save directory or data package name."
        ),
        need(
          input$dp_title != ""
          && grepl("^[[:alnum:]\\ \\.,:_-]+$", input$dp_title),
          "This title has invalid character: use alphanumerics, or one of:
        ' '  '.'  ','  ':'  '_'  '-'  "
          # \" \", \".\", \",\", \":\", \"_\" or \"-\"."
        )
      )
      rv$valid.name <- TRUE
      return(actionButton(NS(id, "dp_create"), "Create"))
    })
    
    observeEvent(input$quick,
      {
        req(input$dp_name %in% c("", paste0(Sys.Date(), "_project"))) # Do not change a yet changed name
        if (input$quick) {
          updateTextInput(session, "dp_name", value = paste0(Sys.Date(), "_project"))
        } else {
          updateTextInput(session, "dp_name", placeholder = paste0(Sys.Date(), "_project"))
        }
      },
      label = "EAL1: quick"
    )
    
    observeEvent(input$dp_name,
      {
        rv$dp.name <- input$dp_name
      },
      label = "EAL1: save dp name"
    )
    
    observeEvent(input$dp_title,
      {
        rv$dp.title <- input$dp_title
      },
      label = "EAL1: save dp title"
    )
    
    # license choice
    observeEvent(input$license,
      {
        rv$dp.license <- input$license
      },
      label = "EAL1: save dp license"
    )
    
    # DP management - on clicks ----
    # * Create DP ----
    onclick("dp_create", {
      req(input$dp_create)
      req(input$dp_name)
      req(rv$valid.name)
      
      # variable operation - legibility purpose
      dp <- input$dp_name
      path <- paste0(rv$dp.location, dp, "_emldp")
      title <- input$dp_title
      license <- rv$dp.license
      
      # verbose
      withProgress(
        {
          # save in empty dedicated variable
          save.variable$emlal <- initReactive("emlal", savevar, main.env$EAL)
          save.variable$emlal$SelectDP$dp.name <- dp
          save.variable$emlal$SelectDP$dp.path <- path
          save.variable$emlal$SelectDP$dp.metadata.path <- paste(path, dp, "metadata_templates", sep = "/")
          save.variable$emlal$SelectDP$dp.data.path <- paste(path, dp, "data_objects", sep = "/")
          save.variable$emlal$SelectDP$dp.eml.path <- paste(path, dp, "eml", sep = "/")
          save.variable$emlal$SelectDP$dp.title <- title
          save.variable$emlal$quick <- input$quick
          incProgress(0.2)
          
          dir.create(path, recursive = TRUE)
          # add DP to index
          main.env$DP.LIST <- rbind(
            main.env$DP.LIST,
            data.frame(
              creator.orcid = main.env$SETTINGS$user,
              name = dp,
              title = title,
              path = path,
              stringsAsFactors = FALSE
            )
          )
          incProgress(0.2)
          
          # EAL template import
          try(
            EMLassemblyline::template_directories(
              save.variable$emlal$SelectDP$dp.path,
              save.variable$emlal$SelectDP$dp.name
            )
          )
          incProgress(0.2)
          x <- try(
            EMLassemblyline::template_core_metadata(
              save.variable$emlal$SelectDP$dp.metadata.path,
              license
            )
          )
          incProgress(0.2)
          
          if (class(x) != "try-error") {
            rv$dp.list <- c(rv$dp.list, dp)
            main.env$EAL$page <- main.env$EAL$page + 1
            saveReactive(save.variable)
            incProgress(0.2)
          } else {
            unlink(path, recursive = TRUE)
            save.variable <- initReactive(main.env = main.env$EAL)
            incProgress(0.2)
            showNotification(x, type = "error")
          }
        },
        message = paste("Creating:", path, "\n", sep = "")
      )
    })
    
    # * Load DP ----
    shinyjs::onclick("dp_load", {
      req(input$dp_list)
      shinyjs::disable("dp_load")
      # variable operation - legibility purpose
      dp <- input$dp_list
      path <- paste0(rv$dp.location, dp, "_emldp")
      
      # verbose
      showNotification(
        paste("Loading:", path, "\n", sep = ""),
        type = "message"
      )
      
      # actions
      save.variable$emlal <- initReactive("emlal", savevar, main.env$EAL)
      
      .save.variable <- jsonlite::read_json(paste0(path, "/", dp, ".json"))[[1]] %>%
        jsonlite::unserializeJSON()
      save.variable$emlal <- setSavevar(.savevar$emlal, savevar$emlal)
      
      # Update paths from another file system
      # * selectDP
      sapply(
        names(save.variable$emlal$SelectDP),
        function(.dp.item){
          save.variable$emlal$SelectDP[[.dp.item]] <- gsub(
            pattern=".*/dataPackagesOutput/emlAssemblyLine/", 
            replacement = rv$dp.location,
            save.variable$emlal$SelectDP[[.dp.item]]
          )
        }
      )
      # * datafiles
      if(isTruthy(save.variable$emlal$DataFiles)){
        sapply(names(save.variable$emlal$DataFiles), function(col){
          save.variable$emlal$DataFiles[,col] <- gsub(
            pattern=".*/dataPackagesOutput/emlAssemblyLine/", 
            replacement = rv$dp.location,
            save.variable$emlal$DataFiles[,col]
          )
          if(col == "size")
            save.variable$emlal$DataFiles[,col] <- as.integer(
              save.variable$emlal$DataFiles[,col]
            )
        })
      }
      # * misc
      if(isTruthy(save.variable$emlal$Misc$abstract)){
        save.variable$emlal$Misc$abstract <- gsub(
          ".*/dataPackagesOutput/emlAssemblyLine/",
          rv$dp.location,
          save.variable$emlal$Misc$abstract
        )
        save.variable$emlal$Misc$methods <- gsub(
          ".*/dataPackagesOutput/emlAssemblyLine/",
          rv$dp.location,
          save.variable$emlal$Misc$methods
        )
        save.variable$emlal$Misc$additional.information <- gsub(
          ".*/dataPackagesOutput/emlAssemblyLine/",
          rv$dp.location,
          save.variable$emlal$Misc$additional.information
        )
      }
      
      # TODO remove this later : update history
      save.variable$emlal$history <- sapply(savevar$emlal$history, function(h) {
        switch(h,
          create = "Select Data Package",
          DataFiles = "Data Files",
          attributes = "Attributes",
          CustomUnits = NULL,
          CatVars = "Categorical Variables",
          GeoCov = "Geographic Coverage",
          TaxCov = "Taxonomic Coverage",
          h
        )
      }) %>% unname()
      save.variable$emlal$quick <- isTRUE(savevar$emlal$quick)
      
      # resume where max reached
      main.env$EAL$page <- if(save.variable$emlal$step > 1)
        -1
      else
        main.env$EAL$page + 1
      main.env$EAL$history <- save.variable$emlal$history
      shinyjs::enable("dp_load")
    })
    
    # * Delete DP ----
    shinyjs::onclick("dp_delete", {
      req(input$dp_list)
      
      # variable operation - legibility purpose
      dp <- input$dp_list
      
      # actions
      showModal(
        modalDialog(
          title = "Delete data package?",
          paste("Are you sure to delete", dp, "?"),
          footer = tagList(
            modalButton("No"),
            actionButton(
              NS(id, "delete_confirm"), "Yes",
              class = "redButton"
            )
          ) # end footer
        ) # end modalDialog
      ) # end showModal
    })
    
    # If deletion is confirmed
    shinyjs::onclick("delete_confirm", {
      # variable operation - legibility purpose
      dp <- input$dp_list
      if(grepl("\\(public\\)", dp))
        dp <- gsub(" \\(public\\)", "", dp)
      path <- paste0(rv$dp.location, dp, "_emldp")
      
      # verbose
      showNotification(
        paste("Deleting:", path, "\n", sep = "")
      ) # to replace by deleting DP
      
      # actions
      unlink(path, recursive = TRUE)
      rv$dp.list <- rv$dp.list[rv$dp.list != dp]
      main.env$DP.LIST <- main.env$DP.LIST[main.env$DP.LIST$name != dp]
      removeModal()
    })
    
    # Output ----
    return(save.variable)
  })
}
