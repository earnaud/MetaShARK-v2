#' @noRd
writeText <- function(x, file = ".", collapse = "\n") {
  # Validity check ----
  if (missing(x)) {
    stop("Error: no text has been provided.")
  }

  if (!is.character(x)) {
    stop("Error: 'x' is not a text.")
  }
  if (!dir.exists(dirname(file))) {
    stop("Error: provided 'file' does not exist.")
  }
  if (length(file) > 1) {
    file <- file[1]
    warning("Length of 'file' > 1 : only the first element has been used.")
  }

  # Process data ----
  if (is.list(x)) {
    x <- unlist(x)
  }
  if (length(x) > 1) {
    x <- paste(x, collapse = collapse)
  }

  # Proper write ----
  file_con <- file(file)
  writeLines(x, file_con)
  close(file_con)
}
