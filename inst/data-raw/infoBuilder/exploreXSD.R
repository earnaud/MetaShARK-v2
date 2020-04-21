library(XML)

exploreXSD <- function(
  node, 
  safe = c("annotation"),
  keep=c("annotation","element","complexType"), 
  lv = 0
){
  # Validity checks
  if(any(class(node) == "XMLInternalDocument"))
    node <- xmlRoot(node)
  if(!any(class(node) == "XMLInternalNode"))
    stop("Node has invalid class") # shall not happen
  
  # Variable initialization
  node.name <- xmlName(node)
  message(rep(" ", lv), "# ", node.name)

  # Check current node interest
  if(!node.name %in% keep){ # Not interesting node
    .children <- getChildrenStrings(node)
    
    # Check potential interest amongst children
    if(!isTRUE(any(unlist(sapply(keep, grepl, x = .children))))){
      removeNodes(node, free = TRUE)
    }
    else {
      replaceNodeWithChildren(node)
      .children <- xmlChildren(node)
      if(length(.children) > 0)
        sapply(.children, exploreXSD, safe = safe, keep = keep, lv = lv+1)
    }
  } # endof Not interesting node
  else { # Interesting node
    # Recursive course except for documentation
    if(!node.name %in% safe){
      .children <- xmlChildren(node)
      if(length(.children) > 0)
        sapply(.children, exploreXSD, safe = safe, keep = keep, lv = lv+1)
    }
    
    # Rename the current node
    if("name" %in% names(xmlAttrs(node))){
      xmlName(node) <- xmlAttrs(node)["name"]
    }
  } # endof Interesting node

  return(NULL)
}