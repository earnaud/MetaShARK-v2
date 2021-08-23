#' @noRd
rebuild <- function(new.version = NA) {
  stopifnot(!is.na(new.version))
  
  old.archive <- dir(".", pattern = "MetaShARK_.*tar.gz")
  old.version <- gsub("MetaShARK_(.*).tar.gz", "\\1", old.archive)
  message(sprintf("Updating from %s to %s", old.version, new.version))
  # change description
  system(sprintf(
    "sed -i '/Version:/ s/%s/%s/' DESCRIPTION",
    old.version, new.version
  ))
  # rebuild
  file.remove(old.archive)
  devtools::build(path = ".")
  # Set golem options
  golem::set_golem_options()
}