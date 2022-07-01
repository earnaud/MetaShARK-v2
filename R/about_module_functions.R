#' @title renderBibliography
#'
#' @description shiny-formatted render* function. Allow the user to print a .bib
#' bibliography content. References are NOT numbered
#' according to possible calls from the app.
#'
#' @param bib character. Path to bibliography file (.bib format).
#'
#' @import shiny
#' @importFrom utils capture.output
#' @importFrom RefManageR PrintBibliography
#'
#' @export
renderBibliography <- function(bib) {
  # read bibliography content
  .bib <- RefManageR::ReadBib(bib)
  # avoid getting numbers: just render a list
  RefManageR::NoCite(.bib, "*")

  # make the UI, once and for all
  renderUI(
    withProgress(message = "Loading bibliography entry ...", value = 0, {
      HTML(
        paste(
          utils::capture.output(
            invisible(
              # for each elements of bibliography render proper bibliography
              sapply(.bib, \ (b) {
                incProgress(1 / length(.bib))
                RefManageR::PrintBibliography(b, .opts = list(style = "html"))
              })
            ) # end of invisible
          ),
          collapse = ""
        ) # end of paste
      )
    }) # end of withProgress
  ) # end of renderUI
}
