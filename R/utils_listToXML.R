#' @importFrom XML newXMLNode xmlValue xmlValue
listToXML <- function(node, sublist) {
  children <- seq_along(sublist)[names(sublist) != ".attrs"]

  sapply(children, \ (child) {
    child_name <- names(sublist)[[child]]
    child_attr <- if (".attrs" %in% names(sublist[[child]])) {
      sublist[[child]]$.attrs
    } else {
      NULL
    }
    if (!is.character(child_name) || child_name == "") browser()
    child_node <- XML::newXMLNode(child_name, parent = node, attrs = child_attr)

    if (typeof(sublist[[child]]) == "list") {
      if ("text" %in% names(sublist[[child]])) {
        XML::xmlValue(child_node) <- sublist[[child]]$text
      } else {
        listToXML(child_node, sublist[[child]])
      }
    } else {
      # FIXME xmlValue adds unwanted text tag
      # browser()
      XML::xmlValue(child_node) <- sublist[[child]]
    }
  })

  return(node)
}
