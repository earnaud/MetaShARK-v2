### buildGuidelines.R ###
start.time <- Sys.time()
cat("BUILDING GUIDELINES\n")

### libraries ###
library(xml2)
library(data.tree)

### imports ###
source("xsdExplorer.R")
source("../utils/multiApply.R")

### set vars ###
{
  cat("Setting variables : ")
  files = dir(path = "xsdFiles",pattern="eml",full.names = TRUE)
  focus = c("element"
            , "simpleType"
            , "attribute"
            , "group"
            , "complexType"
            , "R-Attributes"
            , "simpleContent"
            , "extension")
  filter = c("simpleType:"
             , "complexType:"
             , "simpleContent:"
             , "element:"
             , "group:[a-zA-Z0-9]{4,}"
             , gsub("xsdFiles/","", gsub("\\.xsd","",files))
             )
  cat(round(Sys.time() - start.time, 1),"s.\n"); start.time = Sys.time()
}



### Guidelines production ###
# Only lists are saved (as it is a more usual format)
cat("Producing the guidelines:\n")

{
  ## system guideline ##
  # if(!file.exists( "../resources/systemGuideline.RData"))
  {
    cat("* System guideline: ")
    systemList = buildSystemList(files, focus)
    saveRDS(systemList, "../resources/systemGuideline.RData")
    cat(round(Sys.time() - start.time, 1),"s.\n"); start.time = Sys.time()
  }
  # else systemList <- readRDS("../resources/systemGuideline.RData")

  ## Backbone guideline ##
  # if(!file.exists( "../resources/backboneGuideline.RData"))
  {
    cat("* Backbone guideline: ")
    backboneList <- buildBackboneList(li = systemList, focus=focus)
    saveRDS(backboneList, "../resources/backboneGuideline.RData")
    cat(round(Sys.time() - start.time, 1),"s.\n")
  }
  # else backboneList <- readRDS("../resources/backboneGuideline.RData")
  
  ## Doc guideline ##
  # if(!file.exists( "../resources/docGuideline.RData"))
  {
    cat("* Doc guideline: ")
    docList <- buildDocList(li = backboneList, filter = filter)
    saveRDS(docList, "../resources/docGuideline.RData")
    cat(round(Sys.time() - start.time, 1),"s.\n"); start.time = Sys.time()
  }
  # else docList <- readRDS("../resources/docGuideline.RData")
  
  # ## Fill guideline ##
  # if(!file.exists( "../resources/fillGuideline.RData"))
  # {
  #   cat("* Fill guideline: ")
  #   fillList <- buildFillList(li = backboneList)
  #   saveRDS(fillList, "../resources/fillGuideline.RData")
  #   cat(round(Sys.time() - start.time, 1),"s.\n"); start.time = Sys.time()
  # }
  # else docList <- readRDS("../resources/fillGuideline.RData")
}

rm(list = ls())
