### multiApply.R

# Can be used as template for recursive list function
# @param target: the list on which the FUNS will be applied
# @param FUNS: list of functions
# @param ARGS: list of arguments matching FUNS. Caution ! The elements in ARGS
#              shall be concatenated in a vector if possible. Else, send a 
#              pointer (list names or so). Remember you do not need to input
#              target in the ARGS list
# @param path: if a FUN explicitly needs the path followed from the 0th-level, 
#              it can be accessed by figuring it clearly as 'path' in the function
#              arguments.
multiApply <- function(target, 
                       FUNS = list(),
                       ARGS = rep(NA, length(FUNS)),
                       path = "Root",
                       setPath = TRUE){

  #- Validity checks
  if(length(FUNS) != length(ARGS)
     && !is.na(ARGS)) 
    stop("FUNS and ARGS length differ !")
  
  if(length(FUNS) == 0) 
    stop("FUNS shall not be empty")
  
  if(!is.list(target))
    return(target)
  
  ## Preparation of visuals
  # cure by avoiding double numbering
  if(grepl("[0-9]+_[0-9]+_",path)){
    path <- gsub("[0-9]+_([0-9]+_)","\\1",path)
  }
  
  #- Recursion
  if(!is.null(attributes(target)))
    target.attributes <- attributes(target)
  target <- lapply(seq_along(target),
                   function(t,f,a,p,i){
                     multiApply(t[[i]], f, a,
                                paste(path, paste0(i,"_",names(t)[i]), sep="/"),
                                setPath = setPath)
                   },
                   t = target,
                   f = FUNS,
                   a = ARGS,
                   p = character())
  if(exists("target.attributes"))
    attributes(target) <- target.attributes
  
  
  
  # Functions application
  sapply(seq_along(FUNS),
         function(i){
           
           # catch functions
           fun <- FUNS[[i]]
           
           # catch arguments
           arg <- ARGS[[i]]
           if(is.na(unlist(arg)[1])) # if NA, arg shall contain only one element
             arg <- list(li = target)
           else
             arg <- c(li = list(target), arg)
           
           # execute functions
           target <<- do.call(fun, arg)
         })
  
  
  
  # adds path to the target as an attribute
  if(setPath){
    if(is.null(attributes(target))) attributes(target) <- list()
    if(!is.null(path)) attr(target, "path") <- path
  }
  return(target)
}