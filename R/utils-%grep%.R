#' %grep%
#' 
#' This function is a copycat from `%in%` to use grepl instead of exact match. 
#' If `x` is found in a part of any element of `table`, returns TRUE.
#'
#' @param x vector or NULL: the values to be matched as a pattern.
#' @param table vector or NULL: the values to be matched against. Long vectors
#'  are not supported.
#'  @param ... additional argument valid for `grepl`.
`%grep%` <- function(x, table, ...) {
  isTRUE(any(grepl(x, table, ...)))
}