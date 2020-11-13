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
  
  # re-creates a whole empty save.variable
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
      Personnel = reactiveValues(),
      Misc = reactiveValues(
        abstract = reactiveValues(
          content = character(),
          file = character()
        ),
        methods = reactiveValues(
          content = character(),
          file = character()
        ),
        keywords = data.frame(
          keyword = character(),
          keyword.thesaurus = character(),
          keyword.set = character()
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
#'
#' @noRd
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
#' @import shiny
#' @importFrom dplyr filter select %>% mutate
#' @importFrom data.table fread
#' @importFrom rmarkdown pandoc_convert
#' @importFrom xml2 read_html
#' @importFrom textutils HTMLdecode
#'
#' @noRd
setLocalRV <- function(main.env){
  message(sprintf("Setting local var for %s", main.env$EAL$page))
  
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
        cbind(
          id = paste0("_", seq(.ind)),
          main.env$save.variable$DataFiles[.ind, .col]
        )
      }
      else
        data.frame(stringsAsFactors = FALSE),
      counter = 1
    ),
    # * Attributes ----
    reactiveValues(
      current = reactiveValues(
        file = as.numeric(isContentTruthy(main.env$save.variable$DataFiles$datapath))
      ),
      custom.units = reactiveValues(
        modal.state = "closed",
        table = data.frame(stringsAsFactors = FALSE),
        values = rep(NA, 5),
        unit.id = character()  # inputID for CU modal
      ),
      completed = reactiveValues(),
      data.filepath = main.env$save.variable$DataFiles$datapath,
      preview = sapply(
        main.env$save.variable$DataFiles$datapath,
        readDataTable,
        stringsAsFactors = FALSE,
        nrows = 5
      )
      # annotations = reactiveValues(
      #   values = data.frame(stringsAsFactors = FALSE),
      #   count = 0
      # ),
    ),
    # * CatVars ----
    reactiveValues(
      current = reactiveValues(
        index = numeric(),
        file = character()
      ),
      trigger = makeReactiveTrigger(),
      cv.files = character(),
      cv.tables = reactiveValues(),
      completed = reactiveValues()
    ),
    # * GeoCov ----
    reactiveValues(
      method = "columns",
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
        )
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
        )
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
      role.choices = list(Base = list("creator", "contact", "PI"), Other = list("Other")),
      last.modified = 0,
      Personnel = data.frame(
        # id = numeric(),
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
      kw <- data.frame()
      if (isContentTruthy(isolate(main.env$save.variable$SelectDP$dp.metadata.path))) {
        kw <- readDataTable(
          paste0(isolate(main.env$save.variable$SelectDP$dp.metadata.path), "/keywords.txt"),
          data.table = FALSE, stringsAsFactors = FALSE
        )
        # Collapse --get by same thesaurus -- set the keyword set
        if(isContentTruthy(kw))
          kw <- data.frame(
            keyword = sapply(unique(kw$keyword.thesaurus), function(kwt) {
              paste(kw %>% 
                      dplyr::filter(keyword.thesaurus == kwt) %>% 
                      dplyr::select(keyword) %>% 
                      unlist(),
                    collapse = ",")
            }),
            keyword.thesaurus = unique(kw$keyword.thesaurus),
            keyword.set = paste0("_", seq(unique(kw$keyword.thesaurus))),
            stringsAsFactors = FALSE,
            row.names = c()
          )
      } 
      if (!isContentTruthy(kw)) {
        kw <- data.frame(
          keyword = character(),
          keyword.thesaurus = character(),
          keyword.set = character(),
          stringsAsFactors = FALSE
        )
      }
      
      # Define reactiveValues
      reactiveValues(
        # Abstract
        abstract = reactiveValues(
          content = character(),
          file = paste(
            isolate(main.env$save.variable$SelectDP$dp.metadata.path),
            "abstract.md",
            sep = "/"
          )
        ),
        # Methods
        methods = reactiveValues(
          content = character(),
          file = paste(
            isolate(main.env$save.variable$SelectDP$dp.metadata.path),
            "methods.md",
            sep = "/"
          )
        ),
        # Keywords
        keywords = kw,
        # Temporal coverage
        temporal.coverage = c(Sys.Date() - 1, Sys.Date()),
        # Additional information
        additional.information = reactiveValues(
          content = character(),
          file = paste(
            isolate(main.env$save.variable$SelectDP$dp.metadata.path),
            "additional_info.md",
            sep = "/"
          )
        )
      )
    },
    # * Make EML ----
    # empty RV to be able at last to save the step
    {
      reactiveValues(empty = NULL)
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
      names(main.env$local.rv$md.tables) <- main.env$save.variable$DataFiles$name
      
      # Curates tables
      sapply(main.env$local.rv$md.tables, function(table){
        table[is.na(table)] <- ""
        if(isTRUE(main.env$save.variable$quick) || isTRUE(main.env$dev))
          table$attributeDefinition <- paste("Description for:", table$attributeName)
      })
      
      # Set custom units
      main.env$local.rv$custom.units$trigger <- reactive({
        main.env$local.rv$custom.units$modal.state
      })
      main.env$local.rv$custom.units$table <- readDataTable(
        dir(
          isolate(main.env$save.variable$SelectDP$dp.metadata.path),
          pattern = "ustom",
          full.names = TRUE
        ),
        stringsAsFactors = FALSE
      )
      
      # Set completed
      lapply(main.env$save.variable$DataFiles$name, function(file.name) {
        main.env$local.rv$completed[[file.name]] <- reactiveValues()
        
        lapply(seq(nrow(main.env$local.rv$md.tables[[file.name]])), function(row.index) {
          # Set completed per row by class
          row <- main.env$local.rv$md.tables[[file.name]][row.index, 1]
          main.env$local.rv$completed[[file.name]][[row]] <- reactiveValues(
            # default is TRUE because it is re-evaluated as user reaches Attributes
            attributeName = TRUE,
            attributeDefinition = TRUE,
            class = TRUE,
            dateTimeFormatString = TRUE,
            unit = TRUE,
            missingValueCode = TRUE,
            missingValueCodeExplanation = TRUE
          )
        }) # end lapply:row
      }) # end lapply:file
    }
    else
      stop("[savevariable_functions.R]
      Incorrect value for variable:
      main.env$save.variable$DataFiles$metadatapath")
    
    # Fill 
    if (isTRUE(main.env$dev) || isTRUE(main.env$save.variable$quick)) {
      lapply(names(main.env$local.rv$md.tables), function(table.name) {
        sapply(colnames(main.env$local.rv$md.tables[[table.name]]), function(col) {
          # local shortcut
          .table <- main.env$local.rv$md.tables[[table.name]]
          
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
          main.env$local.rv$md.tables[[table.name]] <- .table
        }) # end of sapply:col
      }) # end of lapply:files
    } # end filling
  } 
  
  # * Catvars ----
  if(main.env$EAL$page == 4) {
    # read metadata folder path
    .md.path <- isolate(main.env$save.variable$SelectDP$dp.metadata.path)
    req(isContentTruthy(.md.path))
    main.env$local.rv$cv.files <- list.files(
      .md.path,
      pattern = "catvar",
      full.names = TRUE
    )
    
    # update current file
    req(isContentTruthy(main.env$local.rv$cv.files))
    main.env$local.rv$current$index <- as.numeric(isContentTruthy(main.env$local.rv$cv.files))
    main.env$local.rv$current$file <- basename(main.env$local.rv$cv.files[main.env$local.rv$current.index])
    
    # Set reactiveValues per file
    sapply(main.env$local.rv$cv.files, function(file.path) {
      # Set each table per file
      file.name <- basename(file.path)
      main.env$local.rv$cv.tables[[file.name]] <- data.table::fread(
        file.path,
        data.table = FALSE, stringsAsFactors = FALSE,
        na.strings = "NA"
      ) %>%
        dplyr::mutate(
          definition = if (definition == "NA" || !isTruthy(definition)) {
            paste("Value:", code, "for attribute:", attributeName)
          } else {
            definition
          }
        ) %>%
        dplyr::mutate(
          code = gsub("^$","NA", code)
        )
      
      # Set completed
      main.env$local.rv$completed[[file.name]] <- reactiveValues()
      attributes <- main.env$local.rv$cv.tables[[file.name]]$attributeName %>%
        unique
      lapply(attributes, function(attribute) {
        main.env$local.rv$completed[[file.name]][[attribute]] <- reactiveValues()
        codes <- main.env$local.rv$cv.tables[[file.name]] %>%
          filter(attributeName == attribute) %>%
          select(code) %>%
          unlist
        main.env$local.rv$completed[[file.name]][[attribute]] <- rep(FALSE, length(codes)) %>%
          setNames(nm = codes)
      })
      
      # Old 
      # 
      # main.env$local.rv$completed[[file.name]] <- reactiveValues()
      # 
      # lapply(seq(nrow(main.env$local.rv$cv.tables[[file.name]])), function(row.index) {
      #   row <- main.env$local.rv$cv.tables[[file.name]][row.index,]
      #   # Set completed per row by class
      #   row.id <- paste(row[1:2], collapse = ":")
      #   main.env$local.rv$completed[[file.name]][[row.id]] <- FALSE # one item per row
      # }) # end lapply:row.index
      
    }) # end lapply:file
  }
  
  # * GeoCov ----
  if(main.env$EAL$page == 5) {
    # Set choices for selectInput -- reuse Attributes
    .att <- main.env$save.variable$Attributes
    .site <- main.env$local.rv$columns$choices$sites <- list()
    .col <- main.env$local.rv$columns$choices$coords <- list()
    sapply(names(.att), function(.file) {
      # Set sites
      .site[[.file]] <<- .att[[.file]] %>% 
        dplyr::filter(class %in% c("character", "categorical")) %>% 
        dplyr::select(attributeName) %>%
        unlist
      .site[[.file]] <<- paste(.file, .site[[.file]], sep="/") %>%
        setNames(nm = .site[[.file]])
      # Set columns
      .col[[.file]] <<- .att[[.file]] %>% 
        dplyr::filter(class == "numeric") %>%
        dplyr::select(attributeName) %>%
        unlist
      .col[[.file]] <<- paste(.file, .col[[.file]], sep="/") %>%
        setNames(nm = .col[[.file]])
    })
    main.env$local.rv$columns$choices$sites <- .site
    main.env$local.rv$columns$choices$coords <- .col
    
    # Read saved values
    if(isContentTruthy(listReactiveValues(main.env$save.variable$GeoCov))) {
      main.env$local.rv$method <- main.env$save.variable$GeoCov$method

      # * Columns
      if(main.env$local.rv$method == "columns") {
        site.name <- main.env$save.variable$GeoCov$columns$site$col
        lat.col <- main.env$save.variable$GeoCov$columns$lat$col
        lon.col <- main.env$save.variable$GeoCov$columns$lon$col
        
        if (site.name %grep% main.env$local.rv$columns$choices$sites) {
          main.env$local.rv$columns$site <- main.env$save.variable$GeoCov$columns$site
        }
        if (lat.col %grep% main.env$local.rv$columns$choices$coords) {
          main.env$local.rv$columns$lat$col <- main.env$save.variable$GeoCov$columns$lat$col
          main.env$local.rv$columns$lat$file <- main.env$save.variable$GeoCov$columns$lat$file
        }
        if (lon.col %grep% main.env$local.rv$columns$choices$coords) {
          main.env$local.rv$columns$lon$col <- main.env$save.variable$GeoCov$columns$lon$col
          main.env$local.rv$columns$lon$file <- main.env$save.variable$GeoCov$columns$lon$file
        }
      }
      # * Custom
      if (main.env$local.rv$method == "custom") {
        saved_table <- main.env$save.variable$GeoCov$custom$coordinates
        if (isContentTruthy(saved_table)) 
          main.env$local.rv$custom$coordinates <- saved_table
      }
    }
    
    # Set completeness
    main.env$local.rv$columns$complete <- reactive(
      isTruthy(main.env$local.rv$columns$site$col) &&
      isTruthy(main.env$local.rv$columns$lat$col) &&
      isTruthy(main.env$local.rv$columns$lon$col)
    )
    main.env$local.rv$custom$complete <-reactive(isContentTruthy(main.env$local.rv$custom$coordinates))
  }
  
  # * TaxCov ----
  if(main.env$EAL$page == 6) {
    # File
    if(isTruthy(main.env$save.variable$TaxCov$taxa.table) && 
       main.env$save.variable$TaxCov$taxa.table %in%
       names(main.env$save.variable$Attributes))
      main.env$local.rv$taxa.table <- unlist(main.env$save.variable$TaxCov$taxa.table)
    # Col
    if(isTruthy(main.env$save.variable$TaxCov$taxa.col) &&
       main.env$save.variable$TaxCov$taxa.col %in% 
       main.env$save.variable$Attributes[[main.env$local.rv$taxa.table]]$attributeName)
      main.env$local.rv$taxa.col <- main.env$save.variable$TaxCov$taxa.col
    # Name type
    if(isTruthy(main.env$save.variable$TaxCov$taxa.name.type) &&
       main.env$save.variable$TaxCov$taxa.name.type %in% 
       c("both", "scientific", "common"))
      main.env$local.rv$taxa.name.type <- main.env$save.variable$TaxCov$taxa.name.type
    # Authority
    if(isTruthy(main.env$save.variable$TaxCov$taxa.authority))
      main.env$local.rv$taxa.authority <- main.env$save.variable$TaxCov$taxa.authority
  }
  
  # * Personnel ----
  if(main.env$EAL$page == 7) {
    # Read template
    {
      # Here, do not read from file: format for 'role' is not the same
      saved.table <- if (main.env$save.variable$Personnel %>%
                         listReactiveValues() %>%
                         isContentTruthy())
          isolate(main.env$save.variable$Personnel) else
            NULL
      if(!is.null(saved.table)) {
        # Remove NA
        saved.table[is.na(saved.table)] <- ""
        # Save
        main.env$local.rv$Personnel <- saved.table
      }
    }
    
    # Add id column -- specific id foor pre-generated input
    if(nrow(main.env$local.rv$Personnel) > 0) {
      main.env$local.rv$Personnel$id <- paste0("_", seq(nrow(main.env$local.rv$Personnel)))
    } else 
      main.env$local.rv$Personnel$id <- character()
    
    # Add trigger inter-roleInputs
    main.env$local.rv$trigger <- reactive({
      req(main.env$EAL$page == 7)
      main.env$local.rv$last.modified
    })
  }
  
  # * Misc ----
  if (main.env$EAL$page == 8) {
    readHTMLfromMD <- function(file) {
      if(isFALSE(file.exists(file)))
        return("<p></p>")
      
      .tmp.file <- tempfile(fileext = ".html")
      rmarkdown::pandoc_convert(
        file,
        from = "markdown_strict",
        to = "html",
        output = .tmp.file
      )
      .out <- xml2::read_html(.tmp.file) %>% 
        textutils::HTMLdecode()
      .out <- ifelse(
        grepl(pattern = "<body>.*</body>", .out),
        gsub(".*<body>(.*)</body>.*", "\\1", .out),
        gsub(".*", "", .out)
      )
      return(.out)
    }
    
    sapply(c("abstract", "methods", "additional.information"), function(x) {
      isolate({main.env$local.rv[[x]]$content <- readHTMLfromMD(main.env$local.rv[[x]]$file)})
    })
  }
  
  # (End) ====
  
  return(main.env$local.rv)
}