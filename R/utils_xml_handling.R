# Functions ====
#' @importFrom XML compareXMLDocs
#'
#' @export
identicalXML <- function(A, B) {
  stopifnot(
    class(A) == "XMLInternalDocument" &&
      class(B) == "XMLInternalDocument"
  )

  comp <- XML::compareXMLDocs(A, B)
  test <- !any(sapply(comp, length) > 0)
  if (isFALSE(test)) {
    warning(comp)
  }

  return(test)
}
