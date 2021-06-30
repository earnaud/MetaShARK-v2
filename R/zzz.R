.onLoad <- function(libname, pkgname) {
  # Options
  op <- options()
  op.metashark <- c(
    metashark.env = new.env()
  )
  toset <- !(names(op.metashark) %in% names(op))
  if(any(toset)) options(op.metashark[toset])

  # increase shiny file input limit size to 100 MB
  options(shiny.maxRequestSize = 100*1024^2)
  
  # Required packages
  invisible(require("shinyBS"))
  invisible(require("shinyTree"))
  
  invisible()
}