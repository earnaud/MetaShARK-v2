#' @title renderBibliography
#'
#' @description shiny-formatted render* function. Allow the user to print a .bib bibliography content. References are NOT numbered
#' according to possible calls from the app.
#'
#' @param bib file path to bibliography
#'
#' @export
#' @importFrom RefManageR NoCite PrintBibliography
#' @importFrom shiny renderUI withProgress incProgress HTML
#' @importFrom utils capture.output
renderBibliography <- function(bib) {
  NoCite(bib, "*")
  renderUI(
    withProgress(message = "Loading bibtex ...", value = 0, {
      HTML(
        paste(
          capture.output(
            invisible(
              sapply(
                bib,
                function(b) {
                  incProgress(1 / length(bib))
                  PrintBibliography(b,
                    .opts = list(style = "html")
                  )
                }
              )
            )
          ),
          collapse = ""
        )
      )
    })
  )
}

# Dev: use it to get lbiraries and version
libsToTerminal <- function() {
  ins <- installed.packages()[loaded_packages()$package, 1:3]
  ins <- ins[ins[, "LibPath"] != ins["base", "LibPath"], c(1, 3)]
  cat(paste0(ins[, "Package"], " v", ins[, "Version"], "\n"))
}
