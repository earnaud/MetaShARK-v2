guessDateTimeFormat <- function(date) {
  compos <- unlist(strsplit(as.character(date), split = "[[:punct:]]"))
  potentialities <- rep(
    list(c("YYYY", "YY", "MM", "DD", "hh", "mm", "ss")),
    length(compos)
  )
  
  # Any is 4-character long: YYYY
  sapply(seq(compos), function(cind) {
    compo <- compos[cind]
    if(nchar(compo) == 4)
      potentialities[[cind]] <<- "YYYY"
    else
      potentialities[[cind]] <<- potentialities[[cind]][-1:-2]
  })
  # > 12: YY DD hh mm ss
  # > 23: YY DD mm ss
  # > 31: YY mm ss
  # > 60: YY
  
}