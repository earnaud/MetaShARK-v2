#' @importFrom devtools build
#' @importFrom golem set_golem_options
#'
#' @noRd
rebuild <- function(new_version = NA) {
  stopifnot(!is.na(new_version))

  # snapshot dependencies
  renv::snapshot()
  
  old_archive <- dir(".", pattern = "MetaShARK_.*tar.gz")
  old_version <- gsub("MetaShARK_(.*).tar.gz", "\\1", old_archive)
  message(sprintf("Updating from %s to %s", old_version, new_version))

  # change DESCRIPTION
  system(sprintf(
    "sed -i '/Version:/ s/%s/%s/' DESCRIPTION",
    old_version, new_version
  ))

  # Set Dockerfile version
  system(sprintf(
    "sed -i 's/LABEL version=\"%s/LABEL version=\"%s/' Dockerfile",
    old_version, new_version
  ))

  # Set golem options
  golem::set_golem_options()

  # rebuild
  file.remove(old_archive)
  devtools::build(path = ".")
}
