# guidelinesExplorer.R

# libraries
library(XML)
library(xml2)


# This builds a dataframe indexing, for each namespace, the path
# Needed to reach it in the docGuideline
buildNsIndex <- function(files){
  namespaces <- sapply(files, function(f){
                         f.xml <- read_xml(f)
                         return(xml_ns(f.xml))
                       })
  namespaces <- unlist(namespaces[!grepl("^d[0-9]*$", attr(namespaces, "names"))])
  attr(namespaces, "names") <- gsub("^.*\\.","", attr(namespaces, "names"))
  namespaces <- namespaces[!duplicated(attr(namespaces, "names"))]
  namespaces <- namespaces[!grepl("d[0-9]+",attr(namespaces, "names"))]
  return(namespaces)
}