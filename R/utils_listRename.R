#' @importFrom purrr map
#' @noRd
renameList <- function(nested_list, pat = "", rep = "") {
  names(nested_list) <- gsub(pat, rep, names(nested_list), perl = TRUE)
  nested_list %>% map(~{
    if (is.list(.x) && !is.data.frame(.x)) {
      renameList(.x, pat, rep)
    } else {
      .x
    }
  })
}