.onLoad <- function(libname, pkgname) {
<<<<<<< HEAD
  # Options
=======
>>>>>>> 21780e3c7e17505ab12284e63b960fbb7e749dc8
  op <- options()
  op.metashark <- c(
    metashark.env = new.env()
  )
  toset <- !(names(op.metashark) %in% names(op))
  if(any(toset)) options(op.metashark[toset])
  
<<<<<<< HEAD
  # Resource Path
  shiny::addResourcePath(
    prefix = "media",
    directoryPath = system.file(
      "media",
      package = "MetaShARK"
    )
  )
  
=======
>>>>>>> 21780e3c7e17505ab12284e63b960fbb7e749dc8
  invisible()
}