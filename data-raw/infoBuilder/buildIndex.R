# buildIndex.R
cat("BUILDING INDEXES\n")

# libraries
library(data.tree)

# imports
source("guidelinesExplorer.R")


# set vars
{
  cat("Setting variables\n")
  xsdFiles = dir(path = "xsdFiles/", 
                 pattern = "eml")
  xsdFiles = paste0("xsdFiles/", xsdFiles)
}

# build index for namespaces
{
  cat("Indexing namespaces\n")
  nsIndex <- buildNsIndex(xsdFiles)
  saveRDS(nsIndex, "../resources/nsIndex.RData")
}