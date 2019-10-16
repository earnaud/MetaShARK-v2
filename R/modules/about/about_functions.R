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

# data.table v1.12.2
# RefManageR v1.2.12
# devtools v2.1.0
# usethis v1.5.1
# EMLassemblyline v2.6.1
# EML v2.0.0
# tippy v0.1.0
# tcltk2 v1.2-11
# shinyjs v1.0
# shinyFiles v0.7.3
# shinydashboard v0.7.1
# shinyTree v0.2.7
# shiny v1.3.2
# + R-base v3.6.0