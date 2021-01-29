.onLoad <- function(libname, pkgname) {
  # Options
  op <- options()
  op.metashark <- c(
    metashark.env = new.env()
  )
  toset <- !(names(op.metashark) %in% names(op))
  if(any(toset)) options(op.metashark[toset])
  
  # Resource Path
  shiny::addResourcePath(
    prefix = "media",
    directoryPath = system.file(
      "media",
      package = "MetaShARK"
    )
  )
  
  invisible()
}