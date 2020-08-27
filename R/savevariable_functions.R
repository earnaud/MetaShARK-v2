# Main save variable ====

#' @import shiny
#'
#' @noRd
initReactive <- function(sub.list = NULL, save.variable = NULL, main.env) {
  if (!is.null(sub.list) && is.null(save.variable)) {
    stop("Attempt to initialize save.variable's sub.list without save.variable.")
  }
  if (!(is.null(sub.list) || sub.list %in% c("emlal", "metafin"))) {
    stop("Attempt to initialize save.variable with inconsistent arguments")
  }
  
  # re-creates a whole save.variable
  if (is.null(sub.list)) {
    save.variable <- reactiveValues()
  }
  # emlal reactivelist management
  else if (sub.list == "emlal") {
    save.variable <- reactiveValues(
      step = 1,
      quick = FALSE,
      creator = character(),
      history = isolate(main.env$EAL$history),
      SelectDP = reactiveValues(
        dp.name = NULL,
        dp.path = NULL,
        dp.metadata.path = NULL,
        dp.title = NULL
      ),
      DataFiles = data.frame(stringsAsFactors = FALSE),
      CatVars = reactiveValues(),
      GeoCov = reactiveValues(),
      TaxCov = reactiveValues(
        taxa.table = NULL,
        taxa.col = NULL,
        taxa.name.type = NULL,
        taxa.authority = NULL
      ),
      Personnel = reactiveValues(
        personnel = NULL
      ),
      Misc = reactiveValues(
        abstract = reactiveValues(
          content = character(),
          file = character()
        ),
        methods = reactiveValues(
          content = character(),
          file = character()
        ),
        keywords = reactiveValues(
          keyword = character(),
          keyword.thesaurus = character()
        ),
        temporal_coverage = NULL,
        additional.information = reactiveValues(
          content = character(),
          file = character()
        )
      )
    )
  }
  # metafin reactivelist management
  else if (sub.list == "metafin") {
    save.variable <- reactiveValues()
  }
  
  # differential returns
  return(save.variable)
}

#' @import shiny
#' @importFrom jsonlite write_json serializeJSON
#'
#' @noRd
saveReactive <- function(main.env) {
  if(is.null(main.env$local.rv))
    stop("No content provided.")
  if(is.null(main.env$save.variable))
    stop("No save variable provided")
  
  withProgress({
    setProgress(1 / 3, "Module save")
    
    # Save local variable content ----
    main.env$save.variable <- do.call(
      switch(main.env$EAL$current,
             "SelectDP" = .saveSelectDP,
             "Data Files" = .saveDataFiles,
             "Attributes" = .saveAttributes,
             "Categorical Variables" = .saveCatVars,
             "Geographic Coverage" = .saveGeoCov,
             "Taxonomic Coverage" = .saveTaxCov,
             "Personnel" = .savePersonnel,
             "Miscellaneous" = .saveMisc
      ),
      args = list(
        main.env
      )
    )
    
    setProgress(2 / 3, "Global save")
    
    # Save JSON ----
    # Get + create path
    path <- main.env$save.variable$SelectDP$dp.path
    if(!dir.exists(path))
      dir.create(path)
    # Get + write file
    filename <- main.env$save.variable$SelectDP$dp.name
    location <- paste0(path, "/", filename, ".json")
    if (file.exists(location))
      file.remove(location)
    jsonlite::write_json(
      jsonlite::serializeJSON(
        listReactiveValues(main.env$save.variable)
      ),
      location
    )
    
    incProgress(1 / 3)
  }) %>% isolate()
  
  showNotification(
    paste("Saved:", main.env$EAL$current, "."),
    duration = 2.5,
    type = "message"
  )
}

#' @import shiny
setSaveVariable <- function(content, save.variable, lv = 1, root = "root") {
  lapply(
    names(content),
    function(label) {
      sub.content <- content[[label]]
      type.content <- typeof(sub.content)
      sub.save.variable <- save.variable[[label]]
      type.save.variable <- typeof(sub.save.variable)
      
      if (is.reactivevalues(sub.save.variable)) {
        if (!is.data.frame(sub.content) &&
            is.list(sub.content)) {
          x <- setSaveVariable(content[[label]], save.variable[[label]], lv = lv + 1, root = label)
        }
        else {
          x <- sub.content
        }
      }
      else {
        x <- sub.content
      }
      
      isolate(save.variable[[label]] <- x)
      return(NULL)
    }
  )
  
  return(save.variable)
}

# Local save variable ====
setLocalRV <- function(main.env){
  # Set variable ====
  main.env$local.rv <- switch(
    main.env$EAL$page,
    # * SelectDP ----
    reactiveValues(
      dp.name = character(),
      dp.title = character(),
      dp.list = listDP(main.env),
      dp.license = NULL
    ),
    # * DataFiles ----
    reactiveValues(
      data.files = if (isContentTruthy(main.env$save.variable$DataFiles)) { # from create button in SelectDP
        .ind <- which(file.exists(main.env$save.variable$DataFiles$datapath))
        .col <- which(names(main.env$save.variable$DataFiles) != "metadatapath")
        main.env$save.variable$DataFiles[.ind, .col]
      }
      else
        data.frame(stringsAsFactors = FALSE)
    ),
    # * Attributes ----
    reactiveValues(
      current = reactiveValues(
        update.view = makeReactiveTrigger(),
        file = as.numeric(isContentTruthy(main.env$save.variable$DataFiles$datapath)),
        preview = NULL # first 5 of table: useful?
      ),
      custom.units = reactiveValues(
        trigger = makeReactiveTrigger(),
        table = data.frame(stringsAsFactors = FALSE),
        values = rep(NA, 5),
        modal.state = "closed",
        unit.id = character()  # inputID for CU modal
      ),
      data.filepath = main.env$save.variable$DataFiles$datapath
      # annotations = reactiveValues(
      #   values = data.frame(stringsAsFactors = FALSE),
      #   count = 0
      # ),
    ),
    # * CatVars ----
    reactiveValues(
      current.index = 0
    ),
    # * GeoCov ----
    reactiveValues(
      columns = reactiveValues(
        choices = reactiveValues(
          files = "all",
          sites = NA_character_,
          coords = NA_character_
        ),
        site = reactiveValues(
          col = character(),
          file = character()
        ),
        lat = reactiveValues(
          col = character(),
          file = character()
        ),
        lon = reactiveValues(
          col = character(),
          file = character()
        ),
        complete = FALSE
      ),
      custom = reactiveValues(
        id = numeric(),
        coordinates = data.frame(
          geographicDescription = character(),
          northBoundingCoordinate = numeric(),
          southBoundingCoordinate = numeric(),
          eastBoundingCoordinate = numeric(),
          westBoundingCoordinate = numeric(),
          stringsAsFactors = FALSE
        ),
        complete = FALSE
      )
    ),
    # * TaxCov ----
    reactiveValues(
      taxa.table = character(),
      taxa.col = character(),
      taxa.name.type = character(),
      taxa.authority = character(),
      complete = FALSE
    ),
    # * Personnel ----
    reactiveValues(
      Personnel = data.frame(
        id = numeric(),
        # Basic Identity
        givenName = character(),
        middleInitial = character(),
        surName = character(),
        # Contact
        organizationName = character(),
        electronicMailAddress = character(),
        # Personnel information
        userId = character(),
        role = character(),
        # Project information
        projectTitle = character(),
        fundingAgency = character(),
        fundingNumber = character(),
        stringsAsFactors = FALSE
      )
    ),
    # * Misc ----
    {
      # Get keywords
      if (isContentTruthy(isolate(main.env$save.variable$SelectDP$dp.metadata.path))) {
        kw <- fread(
          paste0(isolate(main.env$save.variable$SelectDP$dp.metadata.path), "/keywords.txt"),
          data.table = FALSE, stringsAsFactors = FALSE
        )
      } else {
        kw <- data.frame(
          keyword = character(),
          keyword.thesaurus = character()
        )
      }
      # Define reactiveValues
      reactiveValues(
        # Abstract
        abstract = reactiveValues(
          content = character(),
          file = paste(
            isolate(main.env$save.variable$SelectDP$dp.metadata.path),
            "abstract.txt",
            sep = "/"
          )
        ),
        # Methods
        methods = reactiveValues(
          content = character(),
          file = paste(
            isolate(main.env$save.variable$SelectDP$dp.metadata.path),
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
            isolate(main.env$save.variable$SelectDP$dp.metadata.path),
            "additional_info.txt",
            sep = "/"
          )
        )
      )
    }
  )
  
  # Post-modifications ====
  # * Attributes ----
  if(main.env$EAL$page == 3) {
    # Path to metadata templates
    if (isContentTruthy(main.env$save.variable$DataFiles$metadatapath)) {
      # Set metadata
      main.env$local.rv$md.filenames <- basename(main.env$save.variable$DataFiles$metadatapath)
      main.env$local.rv$md.tables <- lapply(
        main.env$save.variable$DataFiles$metadatapath,
        readDataTable,
        data.table = FALSE, stringsAsFactors = FALSE
      )
      # Set custom units
      main.env$local.rv$custom.units$table <- readDataTable(
        dir(
          isolate(main.env$save.variable$SelectDP$dp.metadata.path),
          pattern = "ustom",
          full.names = TRUE
        ),
        stringsAsFactors = FALSE
      )
      # Set completed
      main.env$local.rv$current$completed <- reactiveValues()
      
      lapply(seq_along(main.env$save.variable$DataFiles$metadatapath), function(file.index) {
        file <- basename(main.env$save.variable$DataFiles$metadatapath)[file.index]
        main.env$local.rv$completed[[file]] <- reactiveValues()
        
        lapply(seq(nrow(main.env$local.rv$md.tables[[file.index]])), function(row.index) {
          # Set completed per row by class
          row <- main.env$local.rv$md.tables[[file.index]][row.index, 1]
          main.env$local.rv$completed[[file]][[row]] <- reactiveValues(
            # attributeDefinition
            attributeDefinition = reactive(isTruthy(
              main.env$local.rv$md.tables[[file.index]][row.index, "attributeDefinition"]
            )),
            # class
            class = reactive(isTruthy(
              main.env$local.rv$md.tables[[file.index]][row.index, "class"]
            )),
            # dateTimeFormatString
            dateTimeFormatString = reactive({
              if(main.env$local.rv$md.tables[[file.index]][row.index, "class"] == "Date") 
                isTruthy(main.env$local.rv$md.tables[[file.index]][row.index, "dateTimeFormatString"]) &&
                isFALSE(grepl(
                  "^!.*!$", 
                  main.env$local.rv$md.tables[[file.index]][row.index, "dateTimeFormatString"]
                ))
              else
                TRUE
            }),
            # unit
            unit = reactive({
              if(main.env$local.rv$md.tables[[file.index]][row.index, "class"] == "numeric") 
                isTruthy(main.env$local.rv$md.tables[[file.index]][row.index, "unit"]) &&
                main.env$local.rv$md.tables[[file.index]][row.index, "unit"] != "custom" &&
                isFALSE(grepl(
                  "^!.*!$",
                  main.env$local.rv$md.tables[[file.index]][row.index, "unit"]
                ))
              else
                TRUE
            })
          )
        }) # end lapply:row
      }) # end lapply:file
    }
    else
      stop("Incorrect value for variable:
           main.env$save.variable$DataFiles$metadatapath")
    
    # Fill 
    if ((isTRUE(main.env$dev) || isTRUE(main.env$save.variable$quick)) &&
        length(main.env$save.variable$history) < 4) {
        lapply(seq(main.env$local.rv$md.tables), function(ind) {
          
          sapply(colnames(main.env$local.rv$md.tables[[ind]]), function(col) {
            # local shortcut
            .table <- main.env$local.rv$md.tables[[ind]]
            
            # Set values
            if (col == "attributeDefinition") {
              .table[[col]] <- paste("Description for", .table[["attributeName"]])
            }
            
            if (col == "dateTimeFormatString") {
              .date.row <- which(.table$class == "Date")
              .table[[col]] <- rep("", nrow(.table))
              if (isTruthy(.date.row)) {
                .table[.date.row, col] <- rep(main.env$FORMATS$dates[3], length(.date.row))
              }
            }
            
            if (col == "unit") {
              .unit.rows <- which(.table$class == "numeric")
              .table[[col]] <- rep("", nrow(.table))
              if (isTruthy(.unit.rows)) {
                .table[.unit.rows, col] <- rep(main.env$FORMATS$units[2], length(.unit.rows))
              }
            }
            
            .table[is.na(.table)] <- ""
            main.env$local.rv$md.tables[[ind]] <- .table
          }) # end of sapply:col
        }) # end of lapply:files
    } # end filling
  } 
  # (End) ----
  
  return(main.env$local.rv)
}