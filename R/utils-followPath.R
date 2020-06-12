# --- List handling

#' @title followPath
#'
#' @description Takes a hierarchy list (tree), a path written in a vector pasted with sep, and returns the leaf
#'
#' @param tree explored hierarchy list thanks to path
#' @param path vector of characters matching some of tree names and
#' separated with sep
#' @param sep separators between path elements (aka tree names)
#' @param root if your path has a root name for root node, enter its name here.
#' Else, enter NULL.
followPath <- function(tree, path, sep = "/", root = "root") {
  # Validity checks
  if (is.list(path)) {
    path <- unlist(path)
  }

  if (is.null(tree) || is.null(path)) {
    stop("'tree' and 'path' args must be specified")
  }
  if (length(path) > 1) {
    stop("path shall be a length-one vector of characters. Collapse it by sep.")
  }
  if (sep == "") {
    stop("path can't be parsed with provided sep.")
  }

  # Processing
  path <- unlist(strsplit(path, sep))
  if (!is.null(root)) {
    path <- path[!path == root]
  }

  while (length(path) != 0) {
    tree <- tree[[path[1]]]
    path <- path[-1]
  }
  return(tree)
}
