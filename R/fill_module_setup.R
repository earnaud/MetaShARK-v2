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
        dp.data.path = NULL,
        dp.title = NULL
      ),
      DataFiles = data.frame(stringsAsFactors = FALSE),
      Attributes = reactiveValues(
        content = NA, # list of data tables
        use.catvars = FALSE
      ),
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
          keywordThesaurus = character(),
          keyword.set = character()
        ),
        temporal.coverage = c(NA, NA),
        additional_information = reactiveValues(
          content = character(),
          file = character()
        )
      ),
      Annotations = reactiveValues(
        annot.table = data.frame(stringsAsFactors = FALSE)
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
      
      isolate({save.variable[[gsub("_", ".", label)]] <- x})
      return(NULL)
    }
  )
  
  return(save.variable)
}

# Local save variable ====
#' @import shiny
#' @importFrom dplyr filter select mutate
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
  main.env$VALUES$last.timer = Sys.time()
  devmsg(tag = "fill-module-setup.R", "set variable", timer.env = main.env)
  main.env$local.rv <- switch(
    main.env$EAL$page,
    ##SelectDP ----
    reactiveValues(
      dp.name = character(),
      dp.title = character(),
      dp.list = listDP(main.env),
      dp.license = NULL
    ),
    ##DataFiles ----
    {
      reactiveValues(
        data.files = if (isContentTruthy(main.env$save.variable$DataFiles) &&
                         any(file.exists(main.env$save.variable$DataFiles$datapath))) { # from create button in SelectDP
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
      )
    },
    ## Attributes ----
    reactiveValues(
      md.tables = reactiveValues(),
      completed = reactiveValues(),
      data.filepath = main.env$save.variable$DataFiles$datapath,
      md.filenames = basename(main.env$save.variable$DataFiles$metadatapath),
      tree.content = c(),
      use.catvars = reactive({FALSE}),
      custom.units = reactiveValues(
        table = readDataTable(
          dir(
            isolate(main.env$save.variable$SelectDP$dp.metadata.path),
            pattern = "^custom_units",
            full.names = TRUE
          )
        )
      ),
      preview = {
        out <- lapply(
          main.env$save.variable$DataFiles$datapath,
          function(file.path) {
            table <- readDataTable(file.path)
            out <- lapply(colnames(table), function(col) {
              .out <- table[,col][which(sapply(table[,col], isContentTruthy))]
              if(length(.out) < 5){
                .out <- c(.out, rep("", 5-length(.out)))
              } else
                .out <- .out[1:5]
              return(.out)
            }) |>
              setNames(nm = colnames(table))
            return(out)
          }
        ) |>
          setNames(nm = gsub(
            "(.*)\\..*$",
            "\\1.txt",
            basename(main.env$save.variable$DataFiles$datapath)
          )
          )
        out
      }
    ),
    ## CatVars ----
    reactiveValues(
      current = reactiveValues(
        index = numeric(),
        file = character()
      ),
      trigger = makeReactiveTrigger(),
      cv.files = character(),
      cv.tables = reactiveValues(),
      completed = reactiveValues(),
      tree.content = c()
    ),
    ## GeoCov ----
    reactiveValues(
      method = "columns",
      ### Columns ----
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
      ### Custom ----
      custom = reactiveValues(
        # will be inserted reactiveValues() named as numbers
        count = 0
      )
      # custom = reactiveValues(
      #   id = numeric(),
      #   coordinates = data.frame(
      #     geographicDescription = character(),
      #     northBoundingCoordinate = numeric(),
      #     southBoundingCoordinate = numeric(),
      #     eastBoundingCoordinate = numeric(),
      #     westBoundingCoordinate = numeric(),
      #     stringsAsFactors = FALSE
      #   )
      # )
    ),
    ## TaxCov ----
    reactiveValues(
      taxa.table = character(),
      taxa.col = character(),
      taxa.name.type = character(),
      taxa.authority = character(),
      complete = FALSE
    ),
    ## Personnel ----
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
    ##Misc ----
    {
      # Get keywords
      kw <- data.frame()
      if (isContentTruthy(isolate(main.env$save.variable$SelectDP$dp.metadata.path))) {
        kw <- readDataTable(
          paste0(isolate(main.env$save.variable$SelectDP$dp.metadata.path), "/keywords.txt")
        )
        if("keywordThesaurus" %in% names(kw))
          colnames(kw)[2] <- "keywordThesaurus"
        
        # Collapse --get by same thesaurus -- set the keyword set
        if(isContentTruthy(kw))
          kw <- data.frame(
            keyword = sapply(unique(kw$keywordThesaurus), function(kwt) {
              paste(kw |> 
                      dplyr::filter(identical(keywordThesaurus,kwt)) |> 
                      dplyr::select(keyword) |> 
                      unlist(),
                    collapse = ",")
            }),
            keywordThesaurus = unique(kw$keywordThesaurus),
            keyword.set = paste0("_", seq(unique(kw$keywordThesaurus))),
            stringsAsFactors = FALSE,
            row.names = c()
          )
      } 
      if (!isContentTruthy(kw)) {
        kw <- data.frame(
          keyword = character(),
          keywordThesaurus = character(),
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
    ##Make EML ----
    # empty RV to be able at last to save the step
    {
      reactiveValues(
        # empty = NULL,
        eml.written = length(dir(main.env$save.variable$SelectDP$dp.eml.path)) > 0
      )
    }
  )
  
  # Post-modifications ====
  devmsg(tag = "fill-module-setup.R", "post-modification", timer.env = main.env)
  
  ## Attributes ----
  if(main.env$EAL$page == 3) {
    devmsg(tag = "setup", "3")
    
    # Path to metadata templates empty?
    ### Set variable ----
    if (isContentTruthy(main.env$save.variable$DataFiles$metadatapath)) {
      # Set content
      lapply( # iterate over number of data files
        1:length(main.env$save.variable$DataFiles$datapath),
        function(.ind){
          
          # local shortcuts for data path and metadata path
          .md.path <- main.env$save.variable$DataFiles$metadatapath[.ind]
          .data.path <- main.env$save.variable$DataFiles$datapath[.ind]
          # Use data file name as reference
          .rv.name <- gsub("^attributes_", "", basename(.md.path))
          # Load metadata table
          .md.table <- readDataTable(.md.path)
          # Curates table content
          .md.table[is.na(.md.table)] <- ""
          
          #### Attribute Definition ----
          .to.fill <- which(!sapply(.md.table$attributeDefinition, isContentTruthy))
          .md.table$attributeDefinition[.to.fill] <- paste("Description for:", .md.table$attributeName[.to.fill])
          
          #### Curate date ----
          .date.row <- which(.md.table$class == "Date")
          # Ensure removing "!Add.*here!"
          .to.replace <- which(grepl("!Add.*here!", .md.table$dateTimeFormatString))
          if(isTruthy(.to.replace))
            .md.table$dateTimeFormatString[.to.replace] <- ""
          
          # If any date, fill rows
          if (isTruthy(.date.row)) {
            # default option
            # .md.table$dateTimeFormatString[.date.row] <- rep(main.env$FORMATS$dates[3], length(.date.row))
            # let's do it better:
            
            # do not work on date row filled (except by !Add.*here!)
            .filled.date <- (sapply(.md.table$dateTimeFormatString, isContentTruthy) & 
                               !sapply(.md.table$dateTimeFormatString, grepl, "!Add.*here!")
            )[.date.row]
            if(any(!.filled.date)){ # if any date is not filled
              # Read 100 first rows
              .data.table <- readDataTable(.data.path, nrows = 100)
              # Guess date for date rows not filled
              .md.table$dateTimeFormatString[.date.row] <- sapply(
                .date.row[!.filled.date],
                function(.row) {
                  # .row is attributes row and data column
                  .dates <- .data.table[[.row]] |> as.character()
                  .date.formats <- guessDateTimeFormat(
                    .dates, main.env$FORMATS$lubridate_formats) |>
                    convertLubridateFormat() |>
                    unique()
                  
                  # If no result, default format string
                  if(length(.date.formats) == 0)
                    .date.formats <- main.env$FORMATS$dates[1]
                  
                  return(.date.formats[1])
                }
              )
            }
          }
          
          #### Curate unit ----
          .nonunit.rows <- which(.md.table$class != "numeric")
          .unit.rows <- which(.md.table$class == "numeric")
          if (isTruthy(.nonunit.rows))
            .md.table$unit[.nonunit.rows] <- rep("", length(.nonunit.rows))
          if (isTruthy(.unit.rows)){
            .val <- .md.table$unit[.unit.rows]
            .val[sapply(.val, function(v) 
                !isTruthy(v) || 
                  grepl("!Ad.*ere!", v) ||
                  isFALSE(v %in% unlist(c(
                    main.env$FORMATS$units, 
                    main.env$local.rv$custom.units$table$id
                  )))
            )] <- main.env$FORMATS$units$dimensionless[1]
            .md.table$unit[.unit.rows] <- .val
          }
          
          # Add units for 'latitude' and 'longitude'
          .degree.attributes <- .md.table$attributeName %in% c("latitude", "longitude")
          if(any(.degree.attributes))
            .md.table$unit[.degree.attributes] <- "degree"
          
          # Set md table
          main.env$local.rv$md.tables[[.rv.name]] <<- .md.table
          
          # Add reactivity to each table
          makeReactiveBinding(
            sprintf(
              "main.env$local.rv$md.tables$%s", 
              .rv.name
            )
          )
          
          # Add completed status for each attribute of each table
          main.env$local.rv$completed[[.rv.name]] <- reactiveValues()
          lapply(seq(nrow(.md.table)), function(row.index) {
            # Set completed per row by class
            .attribute <- .md.table$attributeName[row.index]
            # Just use the attribute since checking the attribute makes checking all fields
            main.env$local.rv$completed[[.rv.name]][[.attribute]] <- TRUE
          }) # end lapply:row
          
          # Setup tree
          main.env$local.rv$tree.content <- buildAttributesTree(main.env)
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
    
    ### Fill variable
    # lapply(names(main.env$local.rv$md.tables), function(table.name) {
    #   sapply(colnames(main.env$local.rv$md.tables[[table.name]]), function(col) {
    #     # local shortcut
    #     .table <- main.env$local.rv$md.tables[[table.name]]
    #     
    #     # Set values
    #     if (col == "attributeDefinition") {
    #       .tofill <- which(!isContentTruthy(.table[[col]]))
    #       .table[.tofill, col] <- paste("Description for", .table[["attributeName"]])
    #     }
    #     
    #     if (col == "dateTimeFormatString") {
    #       .date.row <- which(.table$class == "Date")
    #       .table[[col]] <- rep("", nrow(.table))
    #       if (isTruthy(.date.row)) {
    #         .table[.date.row, col] <- rep(main.env$FORMATS$dates[3], length(.date.row))
    #       }
    #     }
    #     
    #     if (col == "unit") {
    #       .nonunit.rows <- which(.table$class != "numeric")
    #       .unit.rows <- which(.table$class == "numeric")
    #       if (isTruthy(.nonunit.rows))
    #         .table[[col]][.nonunit.rows] <- rep("", length(.nonunit.rows))
    #       if (isTruthy(.unit.rows)){
    #         .val <- .table[.unit.rows, col]
    #         .val[sapply(.val, function(v) 
    #           v == main.env$FORMATS$units$dimensionless[1] ||
    #             !isTruthy(v) ||
    #             grepl("!Ad.*ere!", v)
    #         )] <- main.env$FORMATS$units$dimensionless[1]
    #         .table[.unit.rows, col] <- .val
    #       }
    #     }
    #     .table[is.na(.table)] <- ""
    #     main.env$local.rv$md.tables[[table.name]] <<- .table
    #   }) # end of sapply:col
    # }) # end of lapply:md.tables
    
    ### Set catvar need ----
    # Add reactive check for catvars templating
    main.env$local.rv$use.catvars <- reactive({
      # Shortcut variable
      .md.tables <- listReactiveValues(main.env$local.rv$md.tables)
      # check for direction: CustomUnits or CatVars
      .check <- sapply(.md.tables, function(.table) {
        "class" %in% colnames(.table) &&
          isTRUE("categorical" %in% .table[,"class"])
      }) |>
        unlist() |>
        any()
      return(.check)
    })
    
    ### Init completeness ----
    main.env$EAL$completed <- main.env$local.rv$completed |>
      listReactiveValues() |>
      unlist() |>
      all()
    # Set tag list rv
    main.env$local.rv$tag.list <- reactiveValues()
  }
  ##Catvars ----
  if(main.env$EAL$page == 4) {
    devmsg(tag = "setup", "4")
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
      main.env$local.rv$cv.tables[[file.name]] <- readDataTable(file.path)
      # Compare saved categorical variables to actual ones, if they changed
      .catvar.attributes.names <- c()
      sapply(main.env$save.variable$Attributes$content, function(md.table) {
        .catvar.attributes.names <<- c(
          .catvar.attributes.names,
          md.table$attributeName[md.table$class == "categorical"]
        )
      })
      
      # Curate catvars
      main.env$local.rv$cv.tables[[file.name]] <- main.env$local.rv$cv.tables[[file.name]] |> 
        # keep attributes actually found as categorical
        dplyr::filter(attributeName %in% .catvar.attributes.names) |>
        # Fill in automated definitions
        dplyr::mutate(
          definition = if (definition == "NA" || !isTruthy(definition)) {
            paste("Value:", code, "for attribute:", attributeName)
          } else {
            definition
          }
        ) |>
        # avoid "" in code
        dplyr::mutate(
          code = gsub("^$","\"\"", code)
        )
      
      makeReactiveBinding(
        sprintf("main.env$local.rv$cv.tables$%s", file.name)
      )
      
      # Set completed
      main.env$local.rv$completed[[file.name]] <- reactiveValues()
      attributes <- main.env$local.rv$cv.tables[[file.name]]$attributeName |>
        unique()
      lapply(attributes, function(attribute) {
        main.env$local.rv$completed[[file.name]][[attribute]] <- FALSE
      })
      
      # Setup tree
      main.env$local.rv$tree.content <- {
        .tables <- isolate(main.env$local.rv$cv.tables)
        
        lapply(
          names(.tables),
          function(.file.name) {
            # files
            structure(lapply(
              unique(.tables[[.file.name]]$attributeName),
              file.name = .file.name,
              function(.attribute.name, file.name){
                codes <- .tables[[file.name]] |> 
                  filter(attributeName == .attribute.name) |> 
                  select(code) |> 
                  unlist()
                untruthy.codes <- which(!sapply(codes, isContentTruthy))
                codes.names <- replace(
                  codes, 
                  untruthy.codes,
                  sprintf("[%s:empty]", untruthy.codes)
                )
                structure(lapply(
                  codes,
                  function(.code) {
                    return(
                      structure(
                        .code,
                        # sttype="default",
                        sticon=""
                      )
                    )
                  }
                ),
                sticon = "fa fa-columns"
                ) |> 
                  setNames(codes.names)
              }
            ) |>
              setNames(nm = unique(.tables[[.file.name]]$attributeName)), 
            # sttype = "root",
            sticon = "fa fa-file",
            stopened = TRUE
            )
          }
        ) |> 
          setNames(nm = names(.tables))
        
      }
    }) # end lapply:file
  }
  
  ## GeoCov ----
  if(main.env$EAL$page == 5) {
    devmsg(tag = "setup", "5")
    
    ### Columns ----
    #### Set choices ----
    # for selectInput -- reuse Attributes
    .att <- main.env$save.variable$Attributes$content
    .site <- main.env$local.rv$columns$choices$sites <- list()
    .col <- main.env$local.rv$columns$choices$coords <- list()
    sapply(names(.att), function(.md.file) {
      .data.file <- main.env$save.variable$DataFiles |>
        filter(grepl(.md.file, metadatapath)) |> # full metadata path of attributes
        select(datapath) |> # full matching data path
        unlist() |>
        basename()
      # Set potential sites choices from attributes
      .site[[.data.file]] <<- .att[[.md.file]] |> 
        dplyr::filter(class %in% c("character", "categorical")) |> 
        dplyr::select(attributeName) |>
        unlist()
      .site[[.data.file]] <<- paste(.data.file, .site[[.data.file]], sep="/") |>
        setNames(nm = .site[[.data.file]])
      # Set columns choices
      .col[[.data.file]] <<- .att[[.md.file]] |> 
        dplyr::filter(class == "numeric") |> 
        dplyr::select(attributeName) |> 
        unlist()
      .col[[.data.file]] <<- paste(.data.file, .col[[.data.file]], sep="/") |>
        setNames(nm = .col[[.data.file]])
    })
    main.env$local.rv$columns$choices$sites <- .site
    main.env$local.rv$columns$choices$coords <- .col
    
    #### Read saved values ----
    if(isContentTruthy(listReactiveValues(main.env$save.variable$GeoCov))) {
      if(is.null(main.env$save.variable$GeoCov$method)) {
        main.env$local.rv$method <- names(main.env$save.variable$GeoCov)
      } else
        main.env$local.rv$method <- main.env$save.variable$GeoCov$method
      
      ## Columns
      if(main.env$local.rv$method == "columns" && 
         isContentTruthy(main.env$save.variable$GeoCov$columns)) {
        site.name <- main.env$save.variable$GeoCov$columns$site$col
        lat.col <- main.env$save.variable$GeoCov$columns$lat$col
        lon.col <- main.env$save.variable$GeoCov$columns$lon$col
        
        # Each time, only set previous values if they are matched in the data 
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
      
      ### Custom ----
      if (main.env$local.rv$method == "custom" &&
          isContentTruthy(main.env$save.variable$GeoCov$custom)) {
        # shortcut save variable
        saved_tables <- main.env$save.variable$GeoCov$custom
        
        if (isContentTruthy(saved_tables)) {
          count <- 0
          
          try(sapply(
            names(saved_tables)[!(names(saved_tables) %in% c("count", "complete"))], 
            function(row.ind) {
              row <- saved_tables[[row.ind]]
              # Recount number of items from 1:n
              count <<- count+1
              main.env$local.rv$custom[[as.character(count)]] <<- reactiveValues(
                # number of locationInputs
                count = nrow(row$points), 
                # Values 
                type = row$type,
                description = row$description, # length == 1
                points = row$points,
                color = row$color # length == 1
              )
              return()
            }
          ))
          main.env$local.rv$custom$count <- count
        }
      }
    }
    
    makeReactiveBinding("main.env$local.rv$custom")
    
    #### Set completeness ----
    main.env$local.rv$columns$complete <- reactive(
      isTruthy(main.env$local.rv$columns$site$col) &&
        isTruthy(main.env$local.rv$columns$lat$col) &&
        isTruthy(main.env$local.rv$columns$lon$col)
    )
    main.env$local.rv$custom$complete <-reactive({
      .nm <- names(main.env$local.rv$custom)[!names(main.env$local.rv$custom) %in% c("complete", "count")]
      if(length(.nm) == 0) FALSE else
        all(sapply(.nm, function(name) {
          isContentTruthy(main.env$local.rv$custom[[name]])
        }))
    })
  }
  
  ## TaxCov ----
  if(main.env$EAL$page == 6) {
    devmsg(tag = "setup", "6")
    # File
    if(isTruthy(main.env$save.variable$TaxCov$taxa.table) && 
       gsub("\\..*","", main.env$save.variable$TaxCov$taxa.table) %grep%
       names(main.env$save.variable$Attributes$content))
      main.env$local.rv$taxa.table <- unlist(main.env$save.variable$TaxCov$taxa.table)
    # Column
    # Get equivalent name for attributes
    .att.table.name <- gsub("\\..*",".txt", main.env$local.rv$taxa.table)
    if(isTruthy(main.env$save.variable$TaxCov$taxa.col) &&
       main.env$save.variable$TaxCov$taxa.col %in% 
       main.env$save.variable$Attributes$content[[.att.table.name]]$attributeName)
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
  
  ##Personnel ----
  if(main.env$EAL$page == 7) {
    devmsg(tag = "setup", "7")
    # Read template
    message("passing here")
    # Here, do not read from file: format for 'role' is not the same
    saved.table <- if(
      main.env$save.variable$Personnel |>
      listReactiveValues() |>
      isContentTruthy()
    )
      isolate(main.env$save.variable$Personnel) else
        NULL
    if(!is.null(saved.table)) {
      # Remove NA
      saved.table[is.na(saved.table)] <- ""
      # Save
      main.env$local.rv$Personnel <- saved.table
    }
    
    # Add id column -- specific id for pre-generated input
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
  
  ##Misc ----
  if (main.env$EAL$page == 8) {
    devmsg(tag = "setup", "8")
    # markdown files
    sapply(c("abstract", "methods", "additional.information"), function(x) {
      isolate({main.env$local.rv[[x]]$content <- readHTMLfromMD(main.env$local.rv[[x]]$file)})
    })
    # temporal coverage
    if(isContentTruthy(main.env$save.variable$Misc$temporal.coverage))
      main.env$local.rv$temporal.coverage <- main.env$save.variable$Misc$temporal.coverage
  }
  
  # (End) ====
  devmsg(tag = "fill-module-setup", "passed", timer.env = main.env)
  
  return(main.env)
}