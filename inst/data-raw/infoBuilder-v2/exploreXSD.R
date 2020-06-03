library(XML)
library(xml2)

source("inst/data-raw/infoBuilder-v2/renderEMLDoc.R")

# This builds a dataframe indexing, for each namespace, the path
# Needed to reach it in the docGuideline
buildNsIndex <- function(files) {
  namespaces <- sapply(files, function(f) {
    f.xml <- read_xml(f)
    return(xml_ns(f.xml))
  })
  namespaces <- unlist(namespaces[!grepl("^d[0-9]*$", attr(namespaces, "names"))])
  attr(namespaces, "names") <- gsub("^.*\\.", "", attr(namespaces, "names"))
  namespaces <- namespaces[!duplicated(attr(namespaces, "names"))]
  namespaces <- namespaces[!grepl("d[0-9]+", attr(namespaces, "names"))]
  return(namespaces)
}

# Main function: parse the XSD and returns user-legible tree
exploreXSD <- function(
  node,
  safe = c("annotation", ".attrs"),
  keep = c("element","complexType", "simpleType", "group"), 
  path = c(),
  ns
){
  # Drops ====
  .drop <- if(grepl("eml-documentation", node$name) || 
      grepl("enumeration", node$name)){
    TRUE
  } else if(node$name == "simpleContent" &&
      names(node$self) == "extension"){
    if(is.character(node$self$extension) ||
        ("attribute" %in% names(node$self$extension) &&
            length(node$self$extension) < 3))
      TRUE
  } else {
    FALSE
  }
  
  if(isTRUE(.drop))
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
  
  # Action ====
  if (.interesting && (node$name %in% safe || !.has.children) ){
    if(node$name == "annotation"){
      .path <- c(path, node$name)
      node.output <- renderEMLDoc(
        node$self, 
        path = .path
        , debug = debug
      )
    }
    else{
      node.output <- node$self
    }
    
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
        safe = safe, keep = keep, path = .path, ns = ns
      )
    })
    names(node.output) <- names(node$self)
  }
  
  # Post-process -----------------------------------
  
  if(!is.null(node.output)) {
    
    # > Remove NULLs & 0Ls ====
    {
      .nulls <- sapply(node.output, function(n) is.null(n) || length(n) == 0)
      if(any(.nulls) && !node$name %in% safe && is.list(node$self)) {
        node.output[.nulls] <- NULL
      }
    }
    
    # > Flatten ====
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
          .tmp$annotation <- tagList(.tmp$annotation, tags$hr(), .attr.doc)
        }
        node.output <- .tmp
      } 
      
      if(all(names(node.output) == ".attrs")){
        node.output <- NULL
      }
    }
    
    # > Rename elements ====
    {
      # message(names(node.output))
      
      .elem <- names(node.output) == "element"
      if(any(.elem)){
        names(node.output)[which(.elem)] <- sapply(which(.elem), function(.ind){
          
          if(is.list(node.output[[.ind]]) && 
              ".attrs" %in% names(node.output[[.ind]])){
            .attr.ind <- which(names(node.output[[.ind]]) == ".attrs")
            # Excedent .attrs
            if(length(.attr.ind) > 1){
              node.output[[.ind]][head(.attr.ind, -1)] <- NULL
            }
            node.output[[.ind]]$.attrs["name"]
          }
          else if(any(names(node.output[[.ind]]) == "name")){
            node.output[[.ind]]["name"]
          } # else {
            # browser()}
        }) %>% sapply(., prettyString)
      }
    }
    
    # > Rename complexTypes ====
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
        }) %>% sapply(., prettyString)
      }
    }
      
    # +> Flatten empty cplx ====
    {
      .empty <- grepl("<|>", names(node.output))
      if(any(.empty)){
        .tmp <- c()
        sapply(seq_along(node.output), function(.ind){
          if(.ind %in% which(.empty)) {
            .tmp <<- c(.tmp, node.output[[.ind]])
          } else {
            .tmp <<- c(.tmp, node.output[.ind])
          }
        })
        node.output <- .tmp
      }
    }
    
    # +> Merge annotations ====
    {
      .annot <- names(node.output) == "annotation"
      if(length(which(.annot)) > 1){
        node.output[[ which(.annot)[1] ]] <- tagList(
          lapply(which(.annot), function(.ind){
            return(node.output[[.ind]])
          })
        )
        node.output[ which(.annot)[-1] ] <- NULL
      }
    }
    
    # > Rename groups ====
    {
      .grp <- names(node.output) == "group"
      if(any(.grp)){
        names(node.output)[which(.grp)] <- sapply(which(.grp), function(.ind){
          
          .name <- if(is.list(node.output[[.ind]]) && 
              ".attrs" %in% names(node.output[[.ind]])) {
            if("name" %in% names(node.output[[.ind]]$.attrs))
              node.output[[.ind]]$.attrs["name"]
            else if("ref" %in% names(node.output[[.ind]]$.attrs))
              node.output[[.ind]]$.attrs["ref"]
          }
          if(is.na(.name) || is.null(.name))
            .name <- if(any(names(node.output[[.ind]]) == "ref")){
              node.output[[.ind]]["ref"]
            } # else {
              # browser()}
          
          return(.name)
          
        }) %>% gsub("^.*:", "", .) %>% sapply(., prettyString)
      }
      
    }
    
    # > Rename simpleTypes ====
    {
      .smpl <- names(node.output) == "simpleType"
      if(any(.smpl)){
        names(node.output)[which(.smpl)] <-  sapply(which(.smpl), function(.ind){
          .attrs <- last(node.output[[.ind]])
          if("name" %in% names(.attrs) && !is.null(.attrs["name"]))
            .attrs["name"]
          else {
            "simpleType"
          }
        }) %>% sapply(., prettyString)
      }
    }
    
    # > Remove typed elements ====
    {
      .typed <- paste0(toupper(names(node.output)), " TYPE") %in%
        toupper(names(node.output)[.cplx])
      if(any(.typed)){
        sapply(which(.typed), function(.el){
          # Get associated complexType
          if(is.list(node.output[[.el]]) && 
              ".attrs" %in% names(node.output[[.el]])) {
            .cplx.name <- node.output[[.el]]$.attrs["type"] %>% 
              sapply(., prettyString)
          } else if(!".attrs" %in% names(node.output[[.el]])) {
            .cplx.name <- node.output[[.el]]["type"] %>% 
              sapply(., prettyString)
          } # else
            # browser()
          
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
    
    # > Rename modules =====
    if(node$name == "root")
    {
      names(node.output) <- names(node.output) %>% 
        gsub("(.*)\\.xsd", "\\1 module", .) %>%
        gsub("^eml-", "", .) %>% sapply(., prettyString)
    }
    
    # > Add ns doc ====
    {
      if(!"annotation" %in% names(node.output) &&
          node$name %in% keep && 
          node$name != "complexType" &&
          !is.null(node.output)) {
        # Variable init
        if(".attrs" %in% names(node.output)){
          .name <- node.output$.attrs["name"]
          .type <- node.output$.attrs["type"]
          .ref <- node.output$.attrs["ref"]
        } else {
          .name <- node.output["name"]
          .type <- node.output["type"]
          .ref <- node.output["ref"]
        }
        
        # Types
        {
          tag.type <- if(!is.null(.type) && !is.na(.type)){
            
            ns.type <- ns[sapply(names(ns), grepl, .type)]
            if (length(ns.type) == 0)
              ns.type <- "this module"
            tag.type <- tags$span(
              tags$b("Cf."),
              .type %>%
                gsub("^.*:", "", .) %>%
                gsub("([a-z])([A-Z])", "\\1 \\2", .),
              "in",
              gsub(".*org/([a-zA-Z ]+)-2.1.1$", "eml-\\1", ns.type)
            )
          } else
            NULL
        }
        
        # References
        {
          tag.ref <- if(!is.null(.ref) && !is.na(.ref)){
            .ns <- gsub("^(.*):.*$","\\1", .ref)
            .referred <- if(.ns == "res")
              " in Resource Module."
            else if(.ns == "ent")
              " in Entity Module."
            else
              " in this module."
            tags$span(
              tags$b("See also: "),
              .ref,
              .referred
            )
          }
          else 
            NULL
        }
        
        # Name
        tag.name <- if(is.na(.name) || is.null(.name)){
          if(!is.null(tag.ref))
            gsub("^.+:", "", .ref) %>% 
            gsub("([a-z])([A-Z])", "\\1 \\2", .) %>% 
            prettyString %>%
            h2
        }
        else
          .name %>% prettyString %>% h2
        
        # Annotation shiny taglist
        if(!is.null(tag.type) || !is.null(tag.ref)){
          if(is.list(node.output))
            node.output <- unlist(node.output)
          
          node.output <- list(
            annotation = tagList(
              tag.name, tag.type, tag.ref
            ),
            .attrs = node.output
          )
        }
      }
    }
  }
  
  # Output ---------------------------------
  
  return( node.output )
}

# Add .attrs as attributes() 
prettifyTree <- function(node, path = "root", attrCode = ".attrs"){
  # Action ====
  if(is.list(node)){
    node.output <- lapply(seq_along(node), function(child){
      if(is.list(node[[child]]) &&
          names(node)[child] != "annotation")
        prettifyTree(node[[child]], path = c(path, names(node)[child]))
      else
        node[[child]]
    })
    names(node.output) <- names(node)
  }
  
  # Post process --------------------------------
  
  # > Remove duplicated ====
  if(last(path) %in% names(node.output) &&
      length(node.output) == 1) {
    node.output <- node.output[[1]]
  }
  
  node.output$annotation <- NULL
  node.output$.attrs <- NULL
  
  path <- path[which(path != "<complex Type>")]  
  
  if(length(node.output) == 0)
    node.output <- paste(path, collapse = "/")
  
  return(node.output)
}



prettyString <- function(str){
  if(length(str) > 0){
    str <- str %>%
      gsub("([a-z])([A-Z])", "\\1 \\2", .) %>%
      capStr
  }
  return(str)
}

capStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
    sep="", collapse=" ")
}
