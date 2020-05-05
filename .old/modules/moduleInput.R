### moduleNavTree.R

# Imports
source("functionsInput.R")
source("styleInput.R")

# Guidelines
cat("* Loading System Guideline ...\r")
SystemGuideline = as.list(readRDS("../../guideLines/SystemGuidelineList.RData"))
cat("* System Guideline successfully loaded !\n")

# UI functions
inputUI <- function(id, IM){
  ns <- NS(id)
  
  
}

# Server functions
input <- function(input, output, session, IM){
  
}
