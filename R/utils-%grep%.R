#' @noRd
`%grep%` <- function(x, table, ...) {
  sapply(x, function(y){
    isTRUE(any(grepl(y, table, ...)))
  })
}