library(XML)

source("inst/data-raw/infoBuilder-v2/renderEMLDoc.R")

exploreXSD <- function(
  node,
  safe = c("annotation", ".attrs"),
  keep = c("element","complexType", "simpleType"), 
  path = c()
){
  # Drops ====
  if(grepl("eml-documentation", node$name))
    return(NULL)
  
  # Validity check ====
  .interesting <- node$name %in% c(keep, safe)
  .has.children <- is.list(node$self) && 
    !is.null(names(node$self))
  
  if(node$name != "root" && !is.null(node$self)) {
    if(last(names(node$self)) == ".attrs")
      node$self[[length(node$self)]] <- append(
        node$self[[length(node$self)]], 
        c(xsd.path = paste(path, collapse = "/"))
      )
  }
  
  # message(rep(".. ", length(path)), "$ ", node$name)
  
  # Action ====
  if (.interesting && (node$name %in% safe || !.has.children) ){
    if(node$name == "annotation"){
      .path <- c(path, node$name)
      node.output <- renderEMLDoc(node$self, path = .path)
    }
    else
      node.output <- node$self
  }
  else if (!.interesting && !.has.children) {
    node.output <- NULL
  }
  else {
    # Recurse
    node.output <- lapply(seq_along(node$self), function(child){
      .path <- c(path, paste0(child, "_", names(node$self)[child]))
      exploreXSD(
        node = list(
          self = node$self[[child]],
          name = names(node$self)[child]
        ),
        safe = safe, keep = keep, path = .path
      )
    })
    names(node.output) <- names(node$self)
  }
  
  # Post-process -----------------------------------
  
  if(!is.null(node.output)) {
      
    # * Remove NULLs & 0Ls ====
    {
      .nulls <- sapply(node.output, function(n) is.null(n) || length(n) == 0)
      if(any(.nulls) && !node$name %in% safe && is.list(node$self)) {
        node.output[.nulls] <- NULL
      }
    }
    
    # * Flatten ====
    {
      .flattenable <- !names(node.output) %in% c(keep, safe)
      if(any(.flattenable) && !node$name %in% safe && is.list(node$self)) {
        # Variable initialization
        .to.flatten <- which(.flattenable)
        
        # Action
        .tmp <- c()
        .attr.doc <- tags$h3("Attributes")
        sapply(seq_along(node.output), function(.ind){
          if(.ind %in% .to.flatten) {
            # if(node$name == "complexType")
            #   browser()
            # remove .attrs
            .attr.ind <- last(
              which(
                names(node.output[[.ind]]) == ".attrs"
              )
            )
            if(!is.na(.attr.ind))
              node.output[[.ind]][.attr.ind] <- NULL
            # Attributes -- retrieve doc
            if(names(node.output)[.ind] == "attribute"){
              .attr.doc <<- tagList(.attr.doc, node.output[[.ind]]$annotation)
              node.output[[.ind]][1] <- NULL
            }
            
            # Concatenate
            .tmp <<- c(.tmp, node.output[[.ind]])
          } else {
            # Keep
            .tmp <<- c(.tmp, node.output[.ind])
          }
        })
        if(!identical(.attr.doc, tags$h3("Attributes"))){
          .tmp$annotation <- tagList(.tmp$annotation, .attr.doc)
          if(sum(names(.tmp) == "annotation") > 1)
            browser()
        }
        node.output <- .tmp
      } 
      
      if(all(names(node.output) == ".attrs")){
        node.output <- NULL
      }
    }
    
    # * Rename elements ====
    {
      .elem <- names(node.output) == "element"
      if(any(.elem)){
        names(node.output)[which(.elem)] <- sapply(which(.elem), function(.ind){
          
          if(is.list(node.output[[.ind]])){
            last(node.output[[.ind]])["name"] # %>% gsub("([a-z])([A-Z])", "\\1 \\2", .)
          }
          else if(any(names(node.output[[.ind]]) == "name")){
            node.output[[.ind]]["name"] # %>% gsub("([a-z])([A-Z])", "\\1 \\2", .)
          } else {
            browser()
          }
        })
      }
    }
    
    # * Rename complexTypes ====
    {
      .cplx <- names(node.output) == "complexType"
      if(any(.cplx)){
        names(node.output)[which(.cplx)] <-  sapply(which(.cplx), function(.ind){
          .attrs <- last(node.output[[.ind]])
          if("name" %in% names(.attrs) && !is.null(.attrs["name"]))
            .attrs["name"] # %>% gsub("([a-z])([A-Z])", "\\1 \\2", .)
          else {
            "<complexType>"
          }
        })
      }
    }
    
    # * Rename simpleTypes ====
    {
      .smpl <- names(node.output) == "simpleType"
      if(any(.cplx)){
        names(node.output)[which(.smpl)] <-  sapply(which(.smpl), function(.ind){
          .attrs <- last(node.output[[.ind]])
          if("name" %in% names(.attrs) && !is.null(.attrs["name"]))
            .attrs["name"]
          else {
            "simpleType"
          }
        })
      }
    }
    
    # * Remove typed elements ====
    {
      .typed <- paste0(toupper(names(node.output)), "TYPE") %in%
          toupper(names(node.output)[.cplx])
      if(any(.typed)){
        sapply(which(.typed), function(.el){
          # Get associated complexType
          if(is.list(node.output[[.el]]))
            .cplx.name <- node.output[[.el]]$.attrs["type"]
          else if(!".attrs" %in% names(node.output[[.el]]))
            .cplx.name <- node.output[[.el]]["type"]
          else
            browser()
            
          # Report annotation
          .ct <- which(names(node.output) == .cplx.name)
          if(names(node.output[[.ct]])[1] != "annotation" &&
              names(node.output[[.el]])[1] == "annotation") {
            node.output[[.ct]] <- c(
              annotation = node.output[[.el]]$annotation,
              node.output[[.ct]]
            )
          }
          else if(names(node.output[[.ct]])[1] != "annotation" &&
              names(node.output[[.el]])[1] != "annotation") {
            node.output[[.ct]] <- c(
              annotation = shiny::tagList(
                tags$h2(names(node.output)[.ct]),
                tags$i("No documentation found.")
              ),
              node.output[[.ct]]
            )
          }
          
          # nullify node
          node.output[[.el]] <<- list(NULL)
        })
        # Properly remove nullified nodes
        .to.remove <- which(sapply(node.output, identical, list(NULL)))
        node.output[.to.remove] <- NULL
      }
    }
    
    # * Rename modules =====
    if(node$name == "root")
    {
      names(node.output) <- names(node.output) %>% 
        gsub("(.*)\\.xsd", "\\1 module", .) %>%
        gsub("^eml-", "", .)
    }
    
  }
  
  # Output ---------------------------------
  
  return( node.output )
}

# Add .attrs as attributes() 
setAttrDoc <- function(node, path = "root", attrCode = ".attrs"){
  
  if(is.list(node)){
    node.output <- lapply(seq_along(node), function(child){
      if(is.list(node[[child]]) && names(node)[child] != "annotation")
        setAttrDoc(node[[child]], path = c(path, names(node)[child]))
      else
        node[[child]]
    })
    names(node.output) <- names(node)
  }
  
  attributes(node.output) <- c(
    attributes(node.output),
    node.output$.attrs
  )
  
  node.output$.attrs <- NULL
  
  # browser()
  
  return(node.output)
}
