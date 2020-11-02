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
#' @importFrom gdata read.xls
#' @importFrom data.table fread
#' 
# @export
readDataTable <- function(file, data.table = FALSE, ...) {
  if (missing(file) || length(file) == 0) {
    stop("Provide a file for this function.")
  }

  if (grepl("xlsx?$", file)) {
    df <- gdata::read.xls(file, ...)
  } else {
    df <- data.table::fread(file, data.table = data.table, ...)
  }

  return(df)
}
