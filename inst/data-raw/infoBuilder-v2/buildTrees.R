rm(list=ls())

library(XML)
library(dplyr)

# Read files ====
{
  xsd.files <- dir("inst/data-raw/xsdFiles/current/", pattern="eml", full.names = TRUE)
  xsd <- lapply(xsd.files, function(file){
    xmlParse(file) %>% xmlRoot %>% xmlToList
    # read_xml(file) %>% as_list %>% .$schema # does not save attributes
  })
  names(xsd) <- basename(xsd.files)
}

# Structure tree ====
{
  source("inst/data-raw/infoBuilder-v2/exploreXSD.R")
  
  nsIndex <- buildNsIndex(xsd.files)
  doc <- exploreXSD(
    node = list(
      self = xsd, 
      name = "root"
    ),
    safe = c("annotation", ".attrs"),
    keep = c(names(xsd), "element", "complexType", "simpleType", "group"),
    ns = nsIndex
  )
  tree <- prettifyTree(doc)
  
  beepr::beep(5)
}

# Shiny Tree test ====
{
  source("inst/data-raw/infoBuilder-v2/testShinyTree.R")
  
  renderShinyTree(doc, tree)
}

# Save to JSON ====
{
  library(jsonlite)
  
  doc.json <- serializeJSON(doc, pretty = TRUE)
  write_json(doc.json, "./inst/resources/doc_guideline.json")
  # doc2 <- read_json("inst/data-raw/infoBuilder-v2/doc_guideline.json")[[1]]
  # doc.back <- unserializeJSON(doc2)
  # identical(doc, doc.back)

  tree.json <- serializeJSON(tree, pretty = TRUE)
  write_json(tree.json, "./inst/resources/tree_guideline.json") 
  # tree2 <- read_json("inst/data-raw/infoBuilder-v2/tree_guideline.json")[[1]]
  # tree.back <- unserializeJSON(tree2)
  # identical(tree, tree.back)
}
