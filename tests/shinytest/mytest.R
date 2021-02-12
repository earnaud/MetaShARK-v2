# Settings ====
golem::document_and_reload("../../")

# Set args in .GlobalEnv
args <- list(...)
args$dev <- isTRUE(args$dev)
args$wip <- isTRUE(args$wip)
args$reactlog <- isTRUE(args$reactlog) || isTRUE(args$dev)
args$test <- isTRUE(args$test)
assign("metashark.args", args, envir = .GlobalEnv)
on.exit(rm("metashark.args", envir = .GlobalEnv))

# Set settings -- substitute to runMetashark()

# Set steps in .GlobalEnv for UI purposes
assign("ui.steps", c(
  "SelectDP",
  "Data_Files",
  "Attributes",
  "Categorical_Variables",
  "Geographic_Coverage",
  "Taxonomic_Coverage",
  "Personnel",
  "Miscellaneous",
  "Make_EML"
), envir = .GlobalEnv)
on.exit(rm("ui.steps", envir = .GlobalEnv))

# Set resourcePaths
addResourcePath("media", system.file("media/", package = "MetaShARK"))
on.exit(removeResourcePath("media"))

# Ensure correct encoding
options(encoding = 'UTF-8')
Sys.setlocale("LC_ALL", "en_US.utf8") 

# Test ====

app <- ShinyDriver$new("../../R")
app$snapshotInit("mytest")

app$snapshot()

