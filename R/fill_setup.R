# Main save_variable ====

#' @import shiny
#'
#' @noRd
initReactive <- function(sub.list = NULL, save_variable = NULL, main_env) {
  if (!is.null(sub.list) && is.null(save_variable)) {
    stop("Attempt to initialize save_variable's sub.list without save_variable.")
  }
  if (!(is.null(sub.list) || sub.list %in% c("emlal", "metafin"))) {
    stop("Attempt to initialize save_variable with inconsistent arguments")
  }

  # re-creates a whole empty save_variable
  if (is.null(sub.list)) {
    save_variable <- reactiveValues()
  }
  # emlal reactivelist management
  else if (sub.list == "emlal") {
    save_variable <- reactiveValues(
      step = 1,
      # quick = FALSE,
      creator = character(),
      history = isolate(main_env$EAL$history),
      SelectDP = reactiveValues(
        dp_name = NULL,
        dp_path = NULL,
        dp_metadata_path = NULL,
        dp_data_path = NULL,
        dp_title = NULL
      ),
      DataFiles = data.frame(stringsAsFactors = FALSE),
      Attributes = reactiveValues(
        content = NA, # list of data tables
        use_catvars = FALSE
      ),
      CatVars = reactiveValues(),
      GeoCov = reactiveValues(),
      TaxCov = reactiveValues(
        taxa_table = NULL,
        taxa_col = NULL,
        taxa_name.type = NULL,
        taxa_authority = NULL
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
          keyword_set = character()
        ),
        temporal_coverage = c(NA, NA),
        additional_information = reactiveValues(
          content = character(),
          file = character()
        )
      ),
      Annotations = reactiveValues(
        annot_table = data.frame(stringsAsFactors = FALSE)
      )
    )
  }
  # metafin reactivelist management
  else if (sub.list == "metafin") {
    save_variable <- reactiveValues()
  }

  # differential returns
  return(save_variable)
}

#' @import shiny
#'
#' @noRd
setSaveVariable <- function(content, save_variable, lv = 1, root = "root") {
  lapply(
    names(content),
    function(label) {
      sub_content <- content[[label]]
      # type_content <- typeof(sub_content)
      # sub_save_variable <- save_variable[[gsub("_", ".", label)]]
      sub_save_variable <- save_variable[[label]]
      # type_save_variable <- typeof(sub_save_variable)
      if (is.reactivevalues(sub_save_variable)) {
        if (!is.data.frame(sub_content) &&
          is.list(sub_content)) {
          x <- setSaveVariable(
            content[[label]],
            save_variable[[label]],
            lv = lv + 1,
            root = label
          )
        } else {
          x <- sub_content
        }
      } else {
        x <- sub_content
      }

      isolate({
        save_variable[[label]] <- x
      })
      return(NULL)
    }
  )

  return(save_variable)
}

#' # Local save_variable ====
#' #' @import shiny
#' #' @importFrom dplyr filter select mutate
#' #' @importFrom data.table fread
#' #' @importFrom rmarkdown pandoc_convert
#' #' @importFrom xml2 read_html
#' #' @importFrom textutils HTMLdecode
#' #'
#' #' @noRd
#' setLocalRV <- function(main_env) {
#'   if (isContentTruthy(main_env$save_variable$SelectDP$dp_metadata_path)) {
#'     checkTemplates(main_env)
#'   }
#'
#'   # Set variable ====
#'   main_env$VALUES$last_timer <- Sys.time()
#'   devmsg(tag = "fill-module-setup.R", "set variable", timer_env = main_env)
#'   main_env$local_rv <- switch(main_env$EAL$page,
#'     ## SelectDP ----
#'     reactiveValues(
#'       dp.name = character(),
#'       dp.title = character(),
#'       dp.list = listDP(main_env),
#'       dp.license = NULL
#'     ),
#'     ## DataFiles ----
#'     {
#'       reactiveValues(
#'         data.files = if (isContentTruthy(main_env$save_variable$DataFiles) &&
#'           any(file.exists(main_env$save_variable$DataFiles$datapath))) { # from create button in SelectDP
#'           .ind <- which(file.exists(main_env$save_variable$DataFiles$datapath))
#'           .col <- which(names(main_env$save_variable$DataFiles) != "metadatapath")
#'           cbind(
#'             id = paste0("_", seq(.ind)),
#'             main_env$save_variable$DataFiles[.ind, .col]
#'           )
#'         } else {
#'           data.frame(stringsAsFactors = FALSE)
#'         },
#'         counter = 1
#'       )
#'     },
#'     ## Attributes ----
#'     reactiveValues(
#'       md.tables = reactiveValues(),
#'       # checked = FALSE,
#'       completed = reactiveValues(),
#'       data.filepath = main_env$save_variable$DataFiles$datapath,
#'       md.filenames = basename(main_env$save_variable$DataFiles$metadatapath),
#'       tree.content = c(),
#'       use_catvars = reactive({
#'         FALSE
#'       }),
#'       custom.units = reactiveValues(
#'         table = readDataTable(
#'           dir(
#'             isolate(main_env$save_variable$SelectDP$dp_metadata_path),
#'             pattern = "^custom_units",
#'             full.names = TRUE
#'           )
#'         )
#'       ),
#'       preview = {
#'         out <- lapply(
#'           main_env$save_variable$DataFiles$datapath,
#'           function(file.path) {
#'             table <- readDataTable(file.path)
#'             out <- lapply(colnames(table), function(col) {
#'               .out <- table[, col][which(sapply(table[, col], isContentTruthy))]
#'               if (length(.out) < 5) {
#'                 .out <- c(.out, rep("", 5 - length(.out)))
#'               } else {
#'                 .out <- .out[1:5]
#'               }
#'               return(.out)
#'             }) |>
#'               setNames(nm = colnames(table))
#'             return(out)
#'           }
#'         ) |>
#'           setNames(nm = gsub(
#'             "(.*)\\..*$",
#'             "\\1.txt",
#'             basename(main_env$save_variable$DataFiles$datapath)
#'           ))
#'         out
#'       }
#'     ),
#'     ## CatVars ----
#'     reactiveValues(
#'       current = reactiveValues(
#'         index = numeric(),
#'         file = character()
#'       ),
#'       trigger = makeReactiveTrigger(),
#'       cv.files = character(),
#'       cv.tables = reactiveValues(),
#'       completed = reactiveValues(),
#'       tree.content = c()
#'     ),
#'     ## GeoCov ----
#'     reactiveValues(
#'       method = "columns",
#'       ### Columns ----
#'       columns = reactiveValues(
#'         choices = reactiveValues(
#'           files = "all",
#'           sites = NA_character_,
#'           coords = NA_character_
#'         ),
#'         site = reactiveValues(
#'           col = character(),
#'           file = character()
#'         ),
#'         lat = reactiveValues(
#'           col = character(),
#'           file = character()
#'         ),
#'         lon = reactiveValues(
#'           col = character(),
#'           file = character()
#'         )
#'       ),
#'       ### Custom ----
#'       custom = reactiveValues(
#'         # will be inserted reactiveValues() named as numbers
#'         count = 0
#'       )
#'       # custom = reactiveValues(
#'       #   id = numeric(),
#'       #   coordinates = data.frame(
#'       #     geographicDescription = character(),
#'       #     northBoundingCoordinate = numeric(),
#'       #     southBoundingCoordinate = numeric(),
#'       #     eastBoundingCoordinate = numeric(),
#'       #     westBoundingCoordinate = numeric(),
#'       #     stringsAsFactors = FALSE
#'       #   )
#'       # )
#'     ),
#'     ## TaxCov ----
#'     reactiveValues(
#'       taxa_table = character(),
#'       taxa.col = character(),
#'       taxa.name.type = character(),
#'       taxa.authority = character(),
#'       complete = FALSE
#'     ),
#'     ## Personnel ----
#'     reactiveValues(
#'       role.choices = list(Base = list("creator", "contact", "PI"), Other = list("Other")),
#'       last.modified = 0,
#'       Personnel = data.frame(
#'         # id = numeric(),
#'         # Basic Identity
#'         givenName = character(),
#'         middleInitial = character(),
#'         surName = character(),
#'         # Contact
#'         organizationName = character(),
#'         electronicMailAddress = character(),
#'         # Personnel information
#'         userId = character(),
#'         role = character(),
#'         # Project information
#'         projectTitle = character(),
#'         fundingAgency = character(),
#'         fundingNumber = character(),
#'         stringsAsFactors = FALSE
#'       )
#'     ),
#'     ## Misc ----
#'     {
#'       # Get keywords
#'       kw <- data.frame()
#'       if (isContentTruthy(isolate(main_env$save_variable$SelectDP$dp_metadata_path))) {
#'         kw <- readDataTable(
#'           paste0(isolate(main_env$save_variable$SelectDP$dp_metadata_path), "/keywords.txt")
#'         )
#'         if ("keywordThesaurus" %in% names(kw)) {
#'           colnames(kw)[2] <- "keywordThesaurus"
#'         }
#'
#'         # Collapse --get by same thesaurus -- set the keyword set
#'         if (isContentTruthy(kw)) {
#'           kw <- data.frame(
#'             keyword = sapply(unique(kw$keywordThesaurus), function(kwt) {
#'               paste(kw |>
#'                 dplyr::filter(identical(keywordThesaurus, kwt)) |>
#'                 dplyr::select(keyword) |>
#'                 unlist(),
#'               collapse = ","
#'               )
#'             }),
#'             keywordThesaurus = unique(kw$keywordThesaurus),
#'             keyword.set = paste0("_", seq(unique(kw$keywordThesaurus))),
#'             stringsAsFactors = FALSE,
#'             row.names = c()
#'           )
#'         }
#'       }
#'       if (!isContentTruthy(kw)) {
#'         kw <- data.frame(
#'           keyword = character(),
#'           keywordThesaurus = character(),
#'           keyword.set = character(),
#'           stringsAsFactors = FALSE
#'         )
#'       }
#'
#'       # Define reactiveValues
#'       reactiveValues(
#'         # Abstract
#'         abstract = reactiveValues(
#'           content = character(),
#'           file = paste(
#'             isolate(main_env$save_variable$SelectDP$dp_metadata_path),
#'             "abstract.md",
#'             sep = "/"
#'           )
#'         ),
#'         # Methods
#'         methods = reactiveValues(
#'           content = character(),
#'           file = paste(
#'             isolate(main_env$save_variable$SelectDP$dp_metadata_path),
#'             "methods.md",
#'             sep = "/"
#'           )
#'         ),
#'         # Keywords
#'         keywords = kw,
#'         # Temporal coverage
#'         temporal_coverage = c(Sys.Date() - 1, Sys.Date()),
#'         # Additional information
#'         additional.information = reactiveValues(
#'           content = character(),
#'           file = paste(
#'             isolate(main_env$save_variable$SelectDP$dp_metadata_path),
#'             "additional_info.md",
#'             sep = "/"
#'           )
#'         )
#'       )
#'     },
#'     ## Make EML ----
#'     # empty RV to be able at last to save the step
#'     {
#'       reactiveValues(
#'         # empty = NULL,
#'         eml.written = length(dir(main_env$save_variable$SelectDP$dp_eml_path)) > 0
#'       )
#'     }
#'   )
#'
#'   # Post-modifications ====
#'   devmsg(tag = "fill-module-setup.R", "post-modification", timer_env = main_env)
#'   ## Attributes ----
#'   if (main_env$EAL$page == 3) {
#'     devmsg(tag = "setup", "3")
#'
#'     # Path to metadata templates empty?
#'     ### Set variable ----
#'     if (isContentTruthy(main_env$save_variable$DataFiles$metadatapath)) {
#'       # Set content
#'       lapply( # iterate over number of data files
#'         1:length(main_env$save_variable$DataFiles$datapath),
#'         function(.ind) {
#'
#'           # local shortcuts for data path and metadata path
#'           .md.path <- main_env$save_variable$DataFiles$metadatapath[.ind]
#'           .data.path <- main_env$save_variable$DataFiles$datapath[.ind]
#'           # Use data file name as reference
#'           .rv.name <- gsub("^attributes_", "", basename(.md.path))
#'           # Load metadata table
#'           .md.table <- readDataTable(.md.path)
#'           # Curates table content
#'           .md.table[is.na(.md.table)] <- ""
#'
#'           #### Attribute Definition ----
#'           .to.fill <- which(!sapply(.md.table$attributeDefinition, isContentTruthy))
#'           .md.table$attributeDefinition[.to.fill] <- paste("Description for:", .md.table$attributeName[.to.fill])
#'
#'           #### Curate date ----
#'           .date.row <- which(.md.table$class == "Date")
#'           # Ensure removing "!Add.*here!"
#'           .to.replace <- which(grepl("!Add.*here!", .md.table$dateTimeFormatString))
#'           if (isTruthy(.to.replace)) {
#'             .md.table$dateTimeFormatString[.to.replace] <- ""
#'           }
#'
#'           # If any date, fill rows
#'           if (isTruthy(.date.row)) {
#'             # default option
#'             # .md.table$dateTimeFormatString[.date.row] <- rep(main_env$FORMATS$dates[3], length(.date.row))
#'             # let's do it better:
#'
#'             # do not work on date row filled (except by !Add.*here!)
#'             .filled.date <- (sapply(.md.table$dateTimeFormatString, isContentTruthy) &
#'               !sapply(.md.table$dateTimeFormatString, grepl, "!Add.*here!")
#'             )[.date.row]
#'             if (any(!.filled.date)) { # if any date is not filled
#'               # Read 100 first rows
#'               .data.table <- readDataTable(.data.path, nrows = 100)
#'               # Guess date for date rows not filled
#'               .md.table$dateTimeFormatString[.date.row] <- sapply(
#'                 .date.row[!.filled.date],
#'                 function(.row) {
#'                   # .row is attributes row and data column
#'                   .dates <- .data.table[[.row]] |> as.character()
#'                   .date.formats <- guessDateTimeFormat(
#'                     .dates, main_env$FORMATS$lubridate_formats
#'                   ) |>
#'                     convertLubridateFormat() |>
#'                     unique()
#'
#'                   # If no result, default format string
#'                   if (length(.date.formats) == 0) {
#'                     .date.formats <- main_env$FORMATS$dates[1]
#'                   }
#'
#'                   return(.date.formats[1])
#'                 }
#'               )
#'             }
#'           }
#'
#'           #### Curate unit ----
#'           .nonunit.rows <- which(.md.table$class != "numeric")
#'           .unit.rows <- which(.md.table$class == "numeric")
#'           if (isTruthy(.nonunit.rows)) {
#'             .md.table$unit[.nonunit.rows] <- rep("", length(.nonunit.rows))
#'           }
#'           if (isTruthy(.unit.rows)) {
#'             .val <- .md.table$unit[.unit.rows]
#'             .val[sapply(.val, function(v) {
#'               v == main_env$FORMATS$units$dimensionless[1] ||
#'                 !isTruthy(v) ||
#'                 grepl("!Ad.*ere!", v)
#'             })] <- main_env$FORMATS$units$dimensionless[1]
#'             .md.table$unit[.unit.rows] <- .val
#'           }
#'           # Add units for 'latitude' and 'longitude'
#'           .degree.attributes <- .md.table$attributeName %in% c("latitude", "longitude")
#'           if (any(.degree.attributes)) {
#'             .md.table$unit[.degree.attributes] <- "degree"
#'           }
#'
#'           # Commit changes
#'           main_env$local_rv$md.tables[[.rv.name]] <<- .md.table
#'
#'           # Add reactivity to each table
#'           makeReactiveBinding(
#'             sprintf(
#'               "main_env$local_rv$md.tables$%s",
#'               .rv.name
#'             )
#'           )
#'
#'           # Add completed status for each attribute of each table
#'           main_env$local_rv$completed[[.rv.name]] <- reactiveValues()
#'           lapply(seq_row(.md.table)), function(row.index) {
#'             # Set completed per row by class
#'             .attribute <- .md.table$attributeName[row.index]
#'             # Just use the attribute since checking the attribute makes checking all fields
#'             main_env$local_rv$completed[[.rv.name]][[.attribute]] <- TRUE
#'           }) # end lapply:row
#'
#'           # Setup tree
#'           main_env$local_rv$tree.content <- build_attributes_tree(main_env)
#'         }
#'       )
#'
#'       # Custom units
#'       makeReactiveBinding("main_env$local_rv$custom.units$table")
#'       main_env$local_rv$custom.units$reactive <- reactive({
#'         main_env$local_rv$custom.units$table
#'       })
#'       main_env$local_rv$custom.units$cancel <- reactiveVal(0)
#'     } else {
#'       stop("[savevariable_functions.R]
#'       Incorrect value for variable:
#'       main_env$save_variable$DataFiles$metadatapath")
#'     }
#'
#'     ### Fill variable
#'     # lapply(names(main_env$local_rv$md.tables), function(table.name) {
#'     #   sapply(colnames(main_env$local_rv$md.tables[[table.name]]), function(col) {
#'     #     # local shortcut
#'     #     .table <- main_env$local_rv$md.tables[[table.name]]
#'     #
#'     #     # Set values
#'     #     if (col == "attributeDefinition") {
#'     #       .tofill <- which(!isContentTruthy(.table[[col]]))
#'     #       .table[.tofill, col] <- paste("Description for", .table[["attributeName"]])
#'     #     }
#'     #
#'     #     if (col == "dateTimeFormatString") {
#'     #       .date.row <- which(.table$class == "Date")
#'     #       .table[[col]] <- rep("", nrow(.table))
#'     #       if (isTruthy(.date.row)) {
#'     #         .table[.date.row, col] <- rep(main_env$FORMATS$dates[3], length(.date.row))
#'     #       }
#'     #     }
#'     #
#'     #     if (col == "unit") {
#'     #       .nonunit.rows <- which(.table$class != "numeric")
#'     #       .unit.rows <- which(.table$class == "numeric")
#'     #       if (isTruthy(.nonunit.rows))
#'     #         .table[[col]][.nonunit.rows] <- rep("", length(.nonunit.rows))
#'     #       if (isTruthy(.unit.rows)) {
#'     #         .val <- .table[.unit.rows, col]
#'     #         .val[sapply(.val, function(v)
#'     #           v == main_env$FORMATS$units$dimensionless[1] ||
#'     #             !isTruthy(v) ||
#'     #             grepl("!Ad.*ere!", v)
#'     #         )] <- main_env$FORMATS$units$dimensionless[1]
#'     #         .table[.unit.rows, col] <- .val
#'     #       }
#'     #     }
#'     #     .table[is.na(.table)] <- ""
#'     #     main_env$local_rv$md.tables[[table.name]] <<- .table
#'     #   }) # end of sapply:col
#'     # }) # end of lapply:md.tables
#'
#'     ### Set catvar need ----
#'     # Add reactive check for catvars templating
#'     main_env$local_rv$use_catvars <- reactive({
#'       # Shortcut variable
#'       .md.tables <- main_env$local_rv$md.tables
#'       # check for direction: CustomUnits or CatVars
#'       .check <- sapply(.md.tables, function(.table) {
#'         isTRUE("categorical" %in% .table[, "class"])
#'       }) |>
#'         unlist() |>
#'         any()
#'       return(.check)
#'     })
#'
#'     ### Init completeness ----
#'     main_env$EAL$completed <- main_env$local_rv$completed |>
#'       listReactiveValues() |>
#'       unlist() |>
#'       all()
#'     # Set tag_list rv
#'     main_env$local_rv$tag_list <- reactiveValues()
#'   }
#'   ## Catvars ----
#'   if (main_env$EAL$page == 4)
#'
#'   ## GeoCov ----
#'   if (main_env$EAL$page == 5) {
#'     devmsg(tag = "setup", "5")
#'
#'     ### Columns ----
#'     #### Set choices ----
#'     # for selectInput -- reuse Attributes
#'     .att <- main_env$save_variable$Attributes$content
#'     .site <- main_env$local_rv$columns$choices$sites <- list()
#'     .col <- main_env$local_rv$columns$choices$coords <- list()
#'     sapply(names(.att), function(.md.file) {
#'       .data.file <- main_env$save_variable$DataFiles |>
#'         filter(grepl(.md.file, metadatapath)) |> # full metadata path of attributes
#'         select(datapath) |> # full matching data path
#'         unlist() |>
#'         basename()
#'       # Set potential sites choices from attributes
#'       .site[[.data.file]] <<- .att[[.md.file]] |>
#'         dplyr::filter(class %in% c("character", "categorical")) |>
#'         dplyr::select(attributeName) |>
#'         unlist()
#'       .site[[.data.file]] <<- paste(.data.file, .site[[.data.file]], sep = "/") |>
#'         setNames(nm = .site[[.data.file]])
#'       # Set columns choices
#'       .col[[.data.file]] <<- .att[[.md.file]] |>
#'         dplyr::filter(class == "numeric") |>
#'         dplyr::select(attributeName) |>
#'         unlist()
#'       .col[[.data.file]] <<- paste(.data.file, .col[[.data.file]], sep = "/") |>
#'         setNames(nm = .col[[.data.file]])
#'     })
#'     main_env$local_rv$columns$choices$sites <- .site
#'     main_env$local_rv$columns$choices$coords <- .col
#'
#'     #### Read saved values ----
#'     if (isContentTruthy(listReactiveValues(main_env$save_variable$GeoCov))) {
#'       if (is.null(main_env$save_variable$GeoCov$method)) {
#'         main_env$local_rv$method <- names(main_env$save_variable$GeoCov)
#'       } else {
#'         main_env$local_rv$method <- main_env$save_variable$GeoCov$method
#'       }
#'
#'       ## Columns
#'       if (main_env$local_rv$method == "columns" &&
#'         isContentTruthy(main_env$save_variable$GeoCov$columns)) {
#'         site.name <- main_env$save_variable$GeoCov$columns$site$col
#'         lat.col <- main_env$save_variable$GeoCov$columns$lat$col
#'         lon.col <- main_env$save_variable$GeoCov$columns$lon$col
#'
#'         # Each time, only set previous values if they are matched in the data
#'         if (site.name %grep% main_env$local_rv$columns$choices$sites) {
#'           main_env$local_rv$columns$site <- main_env$save_variable$GeoCov$columns$site
#'         }
#'         if (lat.col %grep% main_env$local_rv$columns$choices$coords) {
#'           main_env$local_rv$columns$lat$col <- main_env$save_variable$GeoCov$columns$lat$col
#'           main_env$local_rv$columns$lat$file <- main_env$save_variable$GeoCov$columns$lat$file
#'         }
#'         if (lon.col %grep% main_env$local_rv$columns$choices$coords) {
#'           main_env$local_rv$columns$lon$col <- main_env$save_variable$GeoCov$columns$lon$col
#'           main_env$local_rv$columns$lon$file <- main_env$save_variable$GeoCov$columns$lon$file
#'         }
#'       }
#'
#'       ### Custom ----
#'       if (main_env$local_rv$method == "custom" &&
#'         isContentTruthy(main_env$save_variable$GeoCov$custom)) {
#'         # shortcut save_variable
#'         saved_tables <- main_env$save_variable$GeoCov$custom
#'
#'         if (isContentTruthy(saved_tables)) {
#'           count <- 0
#'
#'           try(sapply(
#'             names(saved_tables)[!(names(saved_tables) %in% c("count", "complete"))],
#'             function(row.ind) {
#'               row <- saved_tables[[row.ind]]
#'               # Recount number of items from 1:n
#'               count <<- count + 1
#'               main_env$local_rv$custom[[as.character(count)]] <<- reactiveValues(
#'                 # number of locationInputs
#'                 count = nrow(row$points),
#'                 # Values
#'                 type = row$type,
#'                 description = row$description, # length == 1
#'                 points = row$points,
#'                 color = row$color # length == 1
#'               )
#'               return()
#'             }
#'           ))
#'           main_env$local_rv$custom$count <- count
#'         }
#'       }
#'     }
#'
#'     makeReactiveBinding("main_env$local_rv$custom")
#'
#'     #### Set completeness ----
#'     main_env$local_rv$columns$complete <- reactive(
#'       isTruthy(main_env$local_rv$columns$site$col) &&
#'         isTruthy(main_env$local_rv$columns$lat$col) &&
#'         isTruthy(main_env$local_rv$columns$lon$col)
#'     )
#'     main_env$local_rv$custom$complete <- reactive({
#'       .nm <- names(main_env$local_rv$custom)[!names(main_env$local_rv$custom) %in% c("complete", "count")]
#'       if (length(.nm) == 0) {
#'         FALSE
#'       } else {
#'         all(sapply(.nm, function(name) {
#'           isContentTruthy(main_env$local_rv$custom[[name]])
#'         }))
#'       }
#'     })
#'   }
#'
#'   ## TaxCov ----
#'   if (main_env$EAL$page == 6) {
#'     devmsg(tag = "setup", "6")
#'     # File
#'     if (isTruthy(main_env$save_variable$TaxCov$taxa_table) &&
#'       gsub("\\..*", "", main_env$save_variable$TaxCov$taxa_table) %grep%
#'         names(main_env$save_variable$Attributes$content)) {
#'       main_env$local_rv$taxa_table <- unlist(main_env$save_variable$TaxCov$taxa_table)
#'     }
#'     # Column
#'     # Get equivalent name for attributes
#'     .att.table.name <- gsub("\\..*", ".txt", main_env$local_rv$taxa_table)
#'     if (isTruthy(main_env$save_variable$TaxCov$taxa.col) &&
#'       main_env$save_variable$TaxCov$taxa.col %in%
#'         main_env$save_variable$Attributes$content[[.att.table.name]]$attributeName) {
#'       main_env$local_rv$taxa.col <- main_env$save_variable$TaxCov$taxa.col
#'     }
#'     # Name type
#'     if (isTruthy(main_env$save_variable$TaxCov$taxa.name.type) &&
#'       main_env$save_variable$TaxCov$taxa.name.type %in%
#'         c("both", "scientific", "common")) {
#'       main_env$local_rv$taxa.name.type <- main_env$save_variable$TaxCov$taxa.name.type
#'     }
#'     # Authority
#'     if (isTruthy(main_env$save_variable$TaxCov$taxa.authority)) {
#'       main_env$local_rv$taxa.authority <- main_env$save_variable$TaxCov$taxa.authority
#'     }
#'   }
#'
#'   ## Personnel ----
#'   if (main_env$EAL$page == 7) {
#'     devmsg(tag = "setup", "7")
#'     # Read template
#'     message("passing here")
#'     # Here, do not read from file: format for 'role' is not the same
#'     saved.table <- if (
#'       main_env$save_variable$Personnel |>
#'         listReactiveValues() |>
#'         isContentTruthy()
#'     ) {
#'       isolate(main_env$save_variable$Personnel)
#'     } else {
#'       NULL
#'     }
#'     if (!is.null(saved.table)) {
#'       # Remove NA
#'       saved.table[is.na(saved.table)] <- ""
#'       # Save
#'       main_env$local_rv$Personnel <- saved.table
#'     }
#'
#'     # Add id column -- specific id for pre-generated input
#'     if (nrow(main_env$local_rv$Personnel) > 0) {
#'       main_env$local_rv$Personnel$id <- paste0(
#'         "_", seq_row(main_env$local_rv$Personnel))
#'     } else {
#'       main_env$local_rv$Personnel$id <- character()
#'     }
#'
#'     # Add trigger inter-roleInputs
#'     main_env$local_rv$trigger <- reactive({
#'       req(main_env$EAL$page == 7)
#'       main_env$local_rv$last.modified
#'     })
#'   }
#'
#'   ## Misc ----
#'   if (main_env$EAL$page == 8) {
#'     devmsg(tag = "setup", "8")
#'     # markdown files
#'     sapply(c("abstract", "methods", "additional.information"), function(x) {
#'       isolate({
#'         main_env$local_rv[[x]]$content <- readHTMLfromMD(
#'           main_env$local_rv[[x]]$file)
#'       })
#'     })
#'     # temporal coverage
#'     if (isContentTruthy(main_env$save_variable$Misc$temporal_coverage)) {
#'       main_env$local_rv$temporal_coverage <-
#'         main_env$save_variable$Misc$temporal_coverage
#'     }
#'   }
#'
#'   # (End) ====
#'   devmsg(tag = "fill-module-setup", "passed", timer_env = main_env)
#'
#'   return(main_env)
#' }
