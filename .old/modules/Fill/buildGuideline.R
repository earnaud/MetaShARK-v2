### buildGuidelines.R ###
rm(list=ls())
start.time <- Sys.time()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



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
  if(!file.exists( "../resources/systemGuideline.RData"))
  {
    cat("* System guideline: ")
    systemList = buildSystemList(files, focus)
    saveRDS(systemList, "../resources/systemGuideline.RData")
    cat(round(Sys.time() - start.time, 1),"s.\n"); start.time = Sys.time()
  }
  else systemList <- readRDS("../resources/systemGuideline.RData")

  ## Backbone guideline ##
  if(!file.exists( "../resources/backboneGuideline.RData"))
  {
    cat("* Backbone guideline: ")
    backboneList <- buildBackboneList(li = systemList, focus=focus)
    saveRDS(backboneList, "../resources/backboneGuideline.RData")
    cat(round(Sys.time() - start.time, 1),"s.\n")
  }
  else backboneList <- readRDS("../resources/systemGuideline.RData")
  
  ## Doc guideline ##
  if(!file.exists( "../resources/docGuideline.RData"))
  {
    cat("* Doc guideline: ")
    docList <- buildDocList(li = backboneList, filter = filter)
    saveRDS(docList, "../resources/docGuideline.RData")
    cat(round(Sys.time() - start.time, 1),"s.\n"); start.time = Sys.time()
  }
  else docList <- readRDS("../resources/systemGuideline.RData")
  
  ## Fill guideline ##
  {
    cat("* Fill guideline: ")
    fillList <- buildFillList(li = backboneList, 
                              focus=focus, 
                              filter = filter[-which(filter %in% c("simpleType:","simpleContent:","element:"))])
    saveRDS(fillList, "../resources/fillGuideline.RData")
    minFillList <- buildMinFillList(fillList, systemList)
    write.table(minFillList, "../resources/minFillGuideline.tsv", row.names = TRUE)
    cat(round(Sys.time() - start.time, 1),"s.\n")
  }
}

rm(list = ls())
