# documentation_functions.R

# extracts the targetted EML content from a list
extractContent <- function(content, nsIndex){
  # modules annotation is stored in 'eml-module/schema'
  if(any(grepl("schema", attr(content,"names"))))
    content <- content[[which(grepl("schema",attr(content,"names")))]]
  
  # Extract metadata content from Attributes
  att <- content$`R-Attributes`
  att.out <- c()
  if(any(grepl("TYPE", names(att)))){
    att.type <- nsIndex[sapply(names(nsIndex), grepl, att[["TYPE"]])]
    if(length(att.type) == 0) att.type <- "this module"
    att.out <- c( att.out,
                  paste(as.character(tags$b("Cf.")), 
                        gsub("^.*:","",att["TYPE"]),
                        "in",
                        gsub(".*org/([a-zA-Z ]+)-2.1.1$","eml-\\1",att.type)) )
  }
  if(any(grepl("REF", names(att)))){
    att.ref <- paste0(tags$b("See also: "),
                      content[["R-Attributes"]][["REF"]],
                      "\n")
    att.out <- c( att.out,
                  att.ref)
  }
  
  if(length(att) == 0) browser()
  att <- att.out
  
  # Documentation content
  if(any(grepl("_annotation", attr(content,"names")))){
    
    ## content
    
    # Extract metadata content from main body
    out <- unlist(content[grepl("_annotation", attr(content, "names"))])
    # preprocess 'ulink' tags that require their URL attributes (R-Attributes needed)
    {
      ulinks.ind = which(grepl("ulink", attr(out, "names"))) # ulinks are always structured the same way
      # browser() 
      out[ ulinks.ind[1:length(ulinks.ind) %% 3 == 1] ] <- paste(out[ ulinks.ind[1:length(ulinks.ind) %% 3 == 1] ], # raw text
                                                                 out[ ulinks.ind[1:length(ulinks.ind) %% 3 == 2] ], # URL
                                                                 sep = "[RECOGNIZED]")
    }
    
    out <- out[!grepl("R-Attributes", attr(out, "names"))]
    out <- sapply(out, gsub,  pattern = " +", replacement = " ")
    
    ## titles
    attr(out, "names") <- gsub("\\.[0-9]+_$","", attr(out, "names"))
    attr(out, "names") <- gsub("^.*_","", attr(out, "names"))
    attr(out, "names") <- gsub("([a-z])([A-Z])","\\1 \\2", attr(out, "names"))
    ind <- which(grepl("RECOGNIZED",out))
    names(out)[ind] <- "ulink"
    
    # browser()
    
    #- reorganizing
    out <- nt.titles(out, list(remove = "emphasis",
                               remove = "citetitle",
                               replace = "module Name",
                               moveback = "documentation",
                               tocode = "literal Layout",
                               use = "title",
                               use = "para",
                               addurl = "ulink"))
    
    out <- sapply(seq_along(out),
                  function(o){
                    if(attr(out,"names")[o] %in% c("recommended Usage",
                                                   "tooltip",
                                                   "stand Alone"))
                      return(paste(tags$b(attr(out, "names")[o]),
                                   ":", out[o],
                                   "\n",
                                   sep = " "))
                    else
                      return(paste(h2(attr(out, "names")[o]),
                                   out[o],
                                   sep = "\n"))
                  })
    out <- paste(c(att, out), collapse = "<br>")
    return(paste0(out, sep = "<br>"))
  }
  else if(length(att) == 0) {
    return("No content found")
  }
  else {
    return(paste(att, collapse = "<br>"))
  }
}

# Apply @action on elements from @vec named after @targets
nt.titles <- function(vec, action_target){
  sapply(action_target,
         function(target){
           
           # parse args
           action = attr(action_target, "names")[match(target, action_target)]
           
           # validity check
           possibleActions <- c("remove","replace","moveback",
                                "tocode", "use", "addurl")
           if(!any(sapply(possibleActions, grepl, action)))
             stop("Action not recognized, must be one of those:\n", 
                  paste(possibleActions,sep=" "))
           
           #--- process - The commands are executed one by one as the rest of
           #              the list is processed automatically
           targeted <- which(attr(vec,"names") %in% target)
           if(length(targeted) > 0){
             
             # remove unwanted
             if(action == "remove"){
               for(i in targeted){
                 j = i-1
                 while(j %in% targeted) j <- j-1
                 vec[j] <- paste(vec[j], tags$cite(vec[i]), vec[i+1], sep = " ")
               }
               vec <- vec[-c(targeted, targeted+1)]
             }
             
             # replace names
             if(action == "replace"){
               attr(vec, "names")[targeted] <- vec[targeted]
               vec[targeted] <- ""
             }
             
             # move back the section
             if(action == "moveback"){
               vec <- c(vec[-targeted], vec[targeted])
             }
             
             # change literalLayout to code tag
             if(action == "tocode"){
               attr(vec,"names")[targeted] <- ""
               for(t in targeted)
                 vec[t] <- as.character(pre(code(vec[t])))
             }
             
             # use the section name as HTML tag
             if(action == "use"){
               attr(vec,"names")[targeted] <- ""
               
               if(target == "title")
                 vec[targeted] <- HTML(as.character(
                   gsub("\n","",tags$h4(vec[targeted]))
                 ))
               
               if(target == "p")
                 vec[targeted] <- sapply(vec[targeted], p)
             }
             
             # add an external link through ulink
             if(action == "addurl"){
               # browser()
               work <- unlist(strsplit(vec[targeted],
                                       split = "\\[RECOGNIZED]"))
               work[1] <- gsub("\n", "", work[1])
               
               # check for the type of ulink
               ulink.type <- ifelse(grepl("^http:",work[1]), "external", "internal")
               vec[targeted] <- switch(ulink.type,
                                       external = HTML(as.character(
                                         a(work[1],
                                           href = work[2]))
                                       ),
                                       internal = {
                                         eml.module.ns <- sub("(.*):.*", "\\1", work[1])
                                         eml.module.name <- sub("^.*/([a-zA-Z]+)-.*$", "\\1", nsIndex[eml.module.ns])
                                         HTML(as.character(
                                           tags$b(
                                             sub(eml.module.ns, eml.module.name, work[1])
                                           ))
                                         )
                                       }
               )
               # empirical: ulink never occurs as first element, neither as last
               vec[targeted-1] <- paste(vec[(targeted-1):(targeted+1)],
                                        collapse = " ")
               vec <- vec[-c(targeted, targeted+1)]
             }
           }
           #--- end of process
           
           # commit changes to vec
           vec <<- vec
         })
  # end of nt.titles
  return(vec)
  
}

# Extract the path made common from every leaf path
# Used to get a non-leaf node's path
commonPath <- function(li,name){
  paths <- unlist(li)
  paths <- sapply(paths, strsplit, split = "/")
  minLength <- min(sapply(paths, length))
  minInd <- min(sapply(paths, function(path){
    length(which(path[1:minLength] == paths[[1]][1:minLength]))
  }))
  common <- paste(paths[[1]][1:minInd], collapse = "/")
  common <- gsub(paste0("(",
                        gsub("(.*_| )",
                             "",
                             name),
                        ").*"),
                 "\\1",
                 common)
  return(common)
}

# --- List handling

# Takes a hierarchy list (tree), a path written in a vector pasted
# with sep = sep, and returns the targetted node
# @param tree: explored hierarchy list thanks to @path
# @param path: vector of characters matching some of @tree names and
#              separated with @sep
# @param sep: separators between @path elements (aka @tree names)
followPath <- function(tree, path, sep = "/"){
  # Validity checks
  if(is.null(tree) || is.null(path))
    stop("'tree' and 'path' args must be specified")
  if(length(path) > 1)
    stop("path shall be a vector of characters")
  if(sep == "")
    stop("path can't be parsed with @sep")
  
  if(is.list(path))
    path <- unlist(path)
  
  # Processing
  path <- unlist(strsplit(path,sep))
  path = path[!path == "Root"]
  
  while(length(path) != 0){
    tree <- tree[[ path[1] ]]
    path = path[-1]
  }
  
  return(tree)
}


# check if one of the children of the input list is a list
has_child <- function(li){
  if(!is.list(li)) return(FALSE)
  return(any(sapply(li, is.list)))
}
