#' @title write.text
#' 
#' @description Applies 'writeLines' in a friendly way. This function has been
#' quickly designed to allow the writing of text files.
#' 
#' @param x A character vector or list. A list will be unlisted. If length(x) > 1
#' then every item will be collapsed into a single vector.
#' @param file A file name (1-length character vector).
#' 
#' @export
write.text <- function(x, file = ".", collapse = "\n"){
  # Validity check ----
  if(missing(x))
    stop("Error: no text has been provided.")
  if(!is.character(x))
    stop("Error: 'x' is not a text.")
  if( !dir.exists(dirname(file)) )
    stop("Error: provided 'file' does not exist.")
  if(length(file) > 1){
    file <- file[1]
    warning("Length of 'file' > 1 : only the first element has been used.")
  }
  
  # Process data ----
  if(is.list(x))
    x <- unlist(x)
  if(length(x) > 1)
    x <- paste(x, collapse = collapse)
  
  # Proper write ----
  fileConn <- file(file)
  writeLines(x, fileConn)
  close(fileConn)

}