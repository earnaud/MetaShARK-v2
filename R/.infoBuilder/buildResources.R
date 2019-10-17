# buildResources.R
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Imports
source("buildGuideline.R", chdir = TRUE)
source("buildIndex.R", chdir = TRUE)