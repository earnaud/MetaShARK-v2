#' @title readDataTable
#'
#' @description Guess the type of the data file (e.g. .xls* or not) and loads it accordingly
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
readDataTable <- function(file, data.table = FALSE, dev = FALSE, ...) {
  if (missing(file)) {
    stop("Provide a file for this function.")
  }

  if (isTRUE(dev)) browser()

  if (grepl("xlsx?$", file)) {
    df <- gdata::read.xls(file, ...)
  } else {
    df <- fread(file, data.table = data.table, ...)
  }

  return(df)
}
