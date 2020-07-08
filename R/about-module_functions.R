#' @title renderBibliography
#'
#' @description shiny-formatted render* function. Allow the user to print a .bib bibliography content. References are NOT numbered
#' according to possible calls from the app.
#'
#' @param bib file path to bibliography
#'
#' @import shiny
#' @importFrom RefManageR ReadBib NoCite PrintBibliography
#' @importFrom utils capture.output
renderBibliography <- function(bib) {
  .bib <- RefManageR::ReadBib(bib)
  RefManageR::NoCite(bib, "*")

  renderUI(
    withProgress(message = "Loading bibtex ...", value = 0, {
      HTML(
        paste(
          utils::capture.output(
            invisible(
              sapply(
                .bib,
                function(b) {
                  incProgress(1 / length(.bib))
                  RefManageR::PrintBibliography(b,
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
