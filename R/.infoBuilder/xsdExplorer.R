# xsdExplorer.R

# This tool is set up on the EML 2.1.1 release's .xsd files provided
# on their official git (https://github.com/NCEAS/eml) on 2019/02/25

# IMPORTS
source("guidelinesFunctions.R")
source("../utils/multiApply.R")
source("../utils/followPath.R")

# Build full hierarchy 
buildSystemList <- function(files, focus)
{
  # read files
  lists <- lapply(files, function(file){
    return(as_list(read_xml(file)))
  })
  
  # renames first-level lists (files' lists)
  attr(lists,"names") <- gsub("xsdFiles/","",
                              gsub("\\.xsd","",
                                   files
                                   )
                              ) 
  
  # Prepare recursion
  toApplyFUNS = list(
    "addRAttributes", # first as attributes are fragile structures in R
    "renameName",
    "addPathway"
  )

  toApplyARGS = list(
    list(NA),
    list(focus = focus,
         numbers = TRUE),
    list(path = quote(path))
  )
  
    # apply recursion
  lists <- multiApply(lists, 
                      toApplyFUNS, 
                      toApplyARGS)

  return(lists)
}

# Build user-friendly hierarchy from full one
# - No xml elements reference a complexType
# - names are back to their 'name' attribute value
buildBackboneList <- function(li, focus){
  
  # Pruning
  mi <- as.Node(li)
  Prune(mi,
        function(node){
          return(tree.find(node,
                           filter))
        })
  backboneList <- as.list(mi)[-1]
  
  return(backboneList)
}

# Build user-legible tree for documentation purpose
buildDocList <- function(li, filter)
{ 
  # prepare recursion
  toApplyFUNS = list(
    "removeTypedElements",# remove 'typed' elements
    "flatten",            # remove non-UF elements
    "prettyList",         # make names user-legible
    "removeRAttributes"  # remove R-Attributes (= XML attributes translated in R)
    # "adaptUnique"
  )
  
  toApplyARGS = list(
    list(NA),
    list(filter = filter),
    list(path = quote(path)),
    list(NA)
    # list(NA)
  )
  
  # apply recursion
  docList <- multiApply(li, 
                         FUNS = toApplyFUNS,
                         ARGS = toApplyARGS,
                         setPath = FALSE)
  
  return(docList)
}

# Build user-legible tree for documentation purpose
buildFillList <- function(li, focus, filter)
{ 
 
  # browser()
  
  return(li)
}








