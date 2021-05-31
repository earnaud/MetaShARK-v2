#' @import XML
listToXML <- function(node, sublist){
  children <- seq_along(sublist)[names(sublist) != ".attrs"]
  
  # if(identical(names(sublist), c("text", ".attrs")))
  #   browser()
  
  sapply(children, function(child){
    child_name <- names(sublist)[[child]]
    child_attr <- if(".attrs" %in% names(sublist[[child]]))
      sublist[[child]]$.attrs else
        NULL
    child_node <- newXMLNode(child_name, parent=node, attrs = child_attr)
    
    if (typeof(sublist[[child]]) == "list"){
      if("text" %in% names(sublist[[child]]))
        xmlValue(child_node) <- sublist[[child]]$text
      else
        listToXML(child_node, sublist[[child]])
    }
    else{
      # FIXME xmlValue adds unwanted text tag
      # browser()
      xmlValue(child_node) <- sublist[[child]]
    }
    
    # if(any(grepl("text", names(unlist(xmlToList(node))))))
    #   browser()
  })
  
  return(node)
}
