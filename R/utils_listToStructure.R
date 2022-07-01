listToStructure <- function(st, lv = 0, name = "root",
                            node_attributes = list(),
                            root_attributes = node_attributes,
                            leaf_attributes = node_attributes) {
  if (missing(st)) {
    stop("No structure provided")
  }
  if (!is.list(st)) {
    return(structure(st))
  }

  # Check for names
  .names <- replace(
    names(st),
    names(st) == "",
    unlist(st[names(st) == ""])
  )
  if (length(.names) == 0) {
    .names <- unlist(st)
  }

  out <- lapply(seq_along(st), \ (sub) {
    subst <- listToStructure(
      st[[sub]], lv + 1,
      name = .names[sub],
      node_attributes = node_attributes,
      root_attributes = root_attributes,
      leaf_attributes = leaf_attributes
    )
    st[[sub]] <<- do.call(
      structure,
      args = c(
        .Data = list(subst),
        if (lv == 0) {
          root_attributes
        } else
        if (!is.list(subst)) {
          leaf_attributes
        } else {
          node_attributes
        }
      )
    )
  })

  names(st) <- .names

  return(st)
}
