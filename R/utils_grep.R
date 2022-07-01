#' shorthand for base::grepl
#'
#' This function is designed to be a shorthand for base::grepl, thought to be
#' used as `%in%`.
#'
#' @param x character. Pattern(s) to be matched against `table`.
#' @param table any unlisted data. A collection of data among which to find
#' `pattern`-matching elements.
#'
#' @export
`%grep%` <- function(x, table, ...) {
  sapply(x, \ (y) isTRUE(any(grepl(y, table, ...))))
}
