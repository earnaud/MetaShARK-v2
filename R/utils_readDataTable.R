#' Read Data Tables
#'
#' Disclaimer: this function is still maturing. There is no guarantee about its
#' performance.
#' Guess the type of the data file (e.g. .xls* or not) and loads it accordingly.
#'
#' @return a data.frame
#'
#' @param file path to the data table file. For Excel files,
#' supports "http://", "https://", and "ftp://" URLS.
#' @param data.table (from `fread()`) TRUE returns a data.table.
#' FALSE returns a data.frame. The default for this argument can
#' be changed with options(datatable.fread.datatable=FALSE).
#' @param ... additional arguments to read.table.
#'
#' @importFrom readxl read_excel
#' @importFrom data.table fread
#'
# @export
readDataTable <- function(file, data.table = FALSE, ...) {
  if (missing(file) || length(file) == 0) {
    stop("Provide a file for this function.")
  }

  if (grepl("xlsx?$", file)) {
    options(show.error.messages = FALSE)
    sheet <- 1
    x <- TRUE
    content <- list()
    while (class(x)[1] != "try-error") {
      x <- try(as.data.frame(readxl::read_excel(file, sheet, na = "NA", ...)))
      if (class(x)[1] != "try-error") {
        content[[sheet]] <- x
      }
      sheet <- sheet + 1
    }
    options(show.error.messages = FALSE)
    df <- content
  } else {
    df <- try(data.table::fread(
      file,
      data.table = data.table,
      stringsAsFactors = FALSE,
      na.strings = "NA",
      ...
    )) |>
      suppressWarnings()
    
    if(any(names(df) == "V1"))
      suppressWarnings(
        for(.sep in strsplit("\t;|,: ","")[[1]]) {
          .df <- try(data.table::fread(
            file,
            data.table = data.table,
            stringsAsFactors = FALSE,
            na.strings = "NA",
            sep = .sep
          ))
          if(all(names(.df) != "V1")){
            df <- .df
            break
          }
        }
      )
  }

  return(df)
}
