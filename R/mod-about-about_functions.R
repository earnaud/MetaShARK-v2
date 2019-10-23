# about_functions.R

renderBibliography <- function(bib){
  NoCite(bib, "*")
  renderUI(
    withProgress(message = "Loading bibtex ...", value = 0, {
      HTML(
        paste(
          capture.output(
            invisible(
              sapply(bib,
                     function(b){
                       incProgress(1/length(bib))
                       PrintBibliography(b,
                                         .opts = list(style = "html")
                                         )
                     }
                     )
              )
            ),
          collapse = "")
        )
      })
    )
}

# Dev: use it to get lbiraries and version
libsToTerminal <- function(){
  ins <- installed.packages()[loaded_packages()$package,1:3]
  ins <- ins[ins[,"LibPath"] != ins["base","LibPath"],c(1,3)]
  cat(paste0(ins[,"Package"], " v", ins[,"Version"],"\n"))
}
