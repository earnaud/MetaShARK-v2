# --- List handling

# Takes a hierarchy list (tree), a path written in a vector pasted
# with sep = @ep, and returns the leaf
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
  