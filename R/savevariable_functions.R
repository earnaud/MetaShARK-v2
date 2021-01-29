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
      sub.save.variable <- save.variable[[gsub("_", ".", label)]]
      type.save.variable <- typeof(sub.save.variable)
      if (is.reactivevalues(sub.save.variable)) {
        if (!is.data.frame(sub.content) &&
            is.list(sub.content)) {
          x <- setSaveVariable(
            content[[label]], 
            save.variable[[gsub("_", ".", label)]],
            lv = lv + 1, 
            root = label
          )
        }
        else {
          x <- sub.content
        }
      }
      else {
        x <- sub.content
      }
      
      isolate(save.variable[[gsub("_", ".", label)]] <- x)
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
  if(isContentTruthy(main.env$save.variable$SelectDP$dp.metadata.path))
    checkTemplates(main.env)
  
  # Set variable ====
<<<<<<< HEAD
=======
  message(names(main.env$save.variable))
>>>>>>> 21780e3c7e17505ab12284e63b960fbb7e749dc8
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
      md.tables = reactiveValues(),
      rv.tables = reactiveValues(),
      checked = FALSE,
      completed = reactiveValues(),
      data.filepath = main.env$save.variable$DataFiles$datapath,
      md.filenames = basename(main.env$save.variable$DataFiles$metadatapath),
      custom.units = reactiveValues(
        table = readDataTable(
          dir(
            isolate(main.env$save.variable$SelectDP$dp.metadata.path),
            pattern = "^custom_units",
            full.names = TRUE
          ),
          stringsAsFactors = FALSE
        )
      ),
      preview = {
        out <- sapply(
          main.env$save.variable$DataFiles$datapath,
          readDataTable,
          stringsAsFactors = FALSE
        ) %>% sapply(
          function(df) {
            lapply(df, function(col) {
              out <- col[which(sapply(col, isContentTruthy))]
              if(length(out) < 5){
                out <- c(out, rep("", 5-length(out)))
              } else
                out <- out[1:5]
              return(out)
            })
          }
        ) %>% 
          setNames(
            nm = basename(main.env$save.variable$DataFiles$metadatapath) %>%
              gsub("attributes_", "", .)
          )
      }
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
    # Path to metadata templates empty?
    if (isContentTruthy(main.env$save.variable$DataFiles$metadatapath)) {
      # Set content
      lapply(
        main.env$save.variable$DataFiles$metadatapath,
        function(path){
          # Use data file name as reference
          .rv.name <- gsub("attributes_", "", basename(path))
          # Populate metadata tables
          main.env$local.rv$md.tables[[.rv.name]] <<- readDataTable(
            path, data.table = FALSE, stringsAsFactors = FALSE
          )
          # Set a shortcut for the remain of this iteration
          .table <- main.env$local.rv$md.tables[[.rv.name]]
          # Curates table content
          .table[is.na(.table)] <- ""
          if(isTRUE(main.env$save.variable$quick) || isTRUE(main.env$dev))
            .table$attributeDefinition <- paste("Description for:", .table$attributeName)
          # Add units for 'latitude' and 'longitude'
          .degree.attributes <- .table$attributeName %in% c("latitude", "longitude")
          if(any(.degree.attributes))
            .table$unit[.degree.attributes] <- "degree"
          main.env$local.rv$md.tables[[.rv.name]] <<- .table
          # Add reactivity to each table (test)
          makeReactiveBinding(
            sprintf(
              "main.env$local.rv$md.tables$%s", 
              .rv.name
            )
          )
          # Add a reactive to read each table (test)
          main.env$local.rv$rv.tables[[.rv.name]] <- reactive({
            devmsg("%s", "-- df reactive test")
            .table
          })
          # Add completed status for each attribute of each table
          main.env$local.rv$completed[[.rv.name]] <- reactiveValues()
          lapply(seq(nrow(.table)), function(row.index) {
            # Set completed per row by class
            .attribute <- .table[row.index, 1]
            main.env$local.rv$completed[[.rv.name]][[.attribute]] <- reactiveValues(
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
        }
      )
      
      # Custom units
      makeReactiveBinding("main.env$local.rv$custom.units$table")
      main.env$local.rv$custom.units$reactive <- reactive({
        main.env$local.rv$custom.units$table
      })
      main.env$local.rv$custom.units$cancel <- reactiveVal(0)
    } else
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
            .nonunit.rows <- which(.table$class != "numeric")
            .unit.rows <- which(.table$class == "numeric")
            if (isTruthy(.nonunit.rows))
              .table[[col]][.nonunit.rows] <- rep("", length(.nonunit.rows))
            if (isTruthy(.unit.rows)){
              .val <- .table[.unit.rows, col]
              .val[sapply(.val, function(v) 
                v == main.env$FORMATS$units[2] ||
                  !isTruthy(v) ||
                  grepl("!Ad.*ere!", v)
              )] <- main.env$FORMATS$units[2]
              .table[.unit.rows, col] <- .val
            }
          }
          .table[is.na(.table)] <- ""
          main.env$local.rv$md.tables[[table.name]] <<- .table
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
    sapply(names(.att), function(.md.file) {
      .data.file <- main.env$save.variable$DataFiles %>%
        filter(grepl(.md.file, metadatapath)) %>%
        select(datapath) %>%
        unlist %>%
        basename
      # Set sites
      .site[[.data.file]] <<- .att[[.md.file]] %>% 
        dplyr::filter(class %in% c("character", "categorical")) %>% 
        dplyr::select(attributeName) %>%
        unlist
      .site[[.data.file]] <<- paste(.data.file, .site[[.data.file]], sep="/") %>%
        setNames(nm = .site[[.data.file]])
      # Set columns
      .col[[.data.file]] <<- .att[[.md.file]] %>% 
        dplyr::filter(class == "numeric") %>%
        dplyr::select(attributeName) %>%
        unlist
      .col[[.data.file]] <<- paste(.data.file, .col[[.data.file]], sep="/") %>%
        setNames(nm = .col[[.data.file]])
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
       gsub("\\..*","", main.env$save.variable$TaxCov$taxa.table) %grep%
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