#' @title renderBibliography
#'
#' @description shiny-formatted render* function. Allow the user to print a .bib bibliography content. References are NOT numbered
#' according to possible calls from the app.
#'
#' @param bib file path to bibliography
#'
#' @export
#' @importFrom RefManageR ReadBib NoCite PrintBibliography
#' @importFrom shiny renderUI withProgress incProgress HTML
#' @importFrom utils capture.output
renderBibliography <- function(bib) {
  bib <- ReadBib(bib)
  NoCite(bib, "*")

  renderUI(
    # sapply(
    #   bib,
    #   function(b) {
    #     incProgress(1 / length(bib))
    #     PrintBibliography(b,
    #       .opts = list(style = "html")
    #     )
    #   }
    # ) %>%
    #   invisible %>%
    #   capture.output %>%
    #   paste(collapse = "") %>%
    #   HTML %>%
    #   withProgress(message = "Loading bibtex ...", value = 0)
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
