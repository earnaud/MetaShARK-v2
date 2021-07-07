setDateSelection <- function(dates.sample){
  stopifnot(grepl("[0-9]", dates.sample))
  # number of items in a date
  compo.sample <- unlist(strsplit(as.character(dates.sample[1]), split = "[[:punct:]]"))
  potentialities <- rep(
    list(c("YYYY", "YY", "MM", "DD", "hh", "mm", "ss")),
    length(compo.sample)
  )
  
  # Set default possible choices
  choices <- expand.grid(potentialities, stringsAsFactors = FALSE)
  choices <- choices[!sapply(1:nrow(choices), function(ind) {
    row <- unlist(choices[ind,])
    any(duplicated(row)) || (any(row == "YYYY") && any(row == "YY"))
  }),]
  choices <- sapply(1:nrow(choices), function(ind){
    row <- unlist(choices[ind,])
    paste(row, collapse = "-")
  })
  
  # Try to guess
  guessed.formats <- try({
    sapply(dates.sample, guessDateTimeFormat) |>
      unlist |> 
      table
  })
  # If failed, return raw format choices
  if(class(guessed.formats) == "try-error")
    return(choices)
  guessed.formats <- guessed.formats[guessed.formats == max(guessed.formats)]
  coded.finds <- names(guessed.formats) |>
    gsub(pattern = "YYYY", replacement = "YY") |>
    gsub(pattern = "-", replacement = "") |>
    gsub(pattern = "YY", replacement = "4") |>
    gsub(pattern = "MM", replacement = "5") |>
    gsub(pattern = "DD", replacement = "6") |>
    gsub(pattern = "hh", replacement = "3") |>
    gsub(pattern = "mm", replacement = "2") |>
    gsub(pattern = "ss", replacement = "1")
  guessed.formats <- names(guessed.formats)[order(coded.finds, decreasing = TRUE)]
  choices <- list(
    suggested = guessed.formats,
    other = choices[!(choices %in% guessed.formats)]
  )
  
  return(choices)
}



guessDateTimeFormat <- function(date) {
  compos <- unlist(strsplit(as.character(date), split = "[ [:punct:]]"))
  potentialities <- rep(
    list(c("YYYY", "YY", "MM", "DD", "hh", "mm", "ss")),
    length(compos)
  )
  
  # Any is 4-character long: YYYY
  sapply(seq(compos), function(cind) {
    compo <- as.numeric(compos[cind])
    pot <<- potentialities[[cind]]
    if(nchar(compo) == 4)
      pot <<- "YYYY"
    else
      pot <<- pot[pot != "YYYY"]
    
    if(length(pot) > 1){
      if(compo > 60){
        pot <<- pot[pot != "ss"]
        pot <<- pot[pot != "mm"]
      }
      if(compo > 31)
        pot <<- pot[pot != "DD"]
      if(compo > 24)
        pot <<- pot[pot != "hh"]
      if(compo > 12)
        pot <<- pot[pot != "MM"]
    }
    
    potentialities[[cind]] <<- pot
  })
  names(potentialities) <- compos
  
  # Once restricted, let's check the possibilities
  out <- expand.grid(potentialities)
  
  # remove duplicates
  toremove <- sapply(1:nrow(out), function(ind) {
    row <- unlist(out[ind,])
    row <- gsub("YYYY", "YY", row)
    return(!any(duplicated(row)))
  })
  out <- out[toremove,]
  
  # Remove great gap format
  types <- c(YYYY = 0, YY = 0, MM = 1, DD = 2, hh = 3, mm = 4, ss = 5)
  tokeep <- sapply(1:nrow(out), function(ind) {
    row <- as.character(unlist(out[ind,]))
    typed.row <- types[row]
    return(max(typed.row) - min(typed.row) <= ncol(out) - 1)
  })
  out <- out[tokeep,]
  
  # format output
  rev.types <- rev(types) |> setNames(names(types))
  out <- sapply(1:nrow(out), function(ind) {
    row <- unlist(out[ind,])
    paste(row, collapse = "-")
  }) |> 
    data.frame(stringsAsFactors = FALSE) |>
    setNames(paste(compos, collapse = "-"))
  
  # set ordered results as first
  sorted <- sapply(1:nrow(out), function(ind) {
    .out <- out[ind,] |>
      unlist |> 
      as.character |>
      strsplit("-") |>
      unlist 
    any(
      sapply(.out, function(i) types[i]) |>
        unname |> 
        is.unsorted() |> 
        isFALSE,
      sapply(.out, function(i) rev.types[i]) |>
        unname |> 
        is.unsorted() |> 
        isFALSE
    )
  })
  if(any(sorted)){
    out <- data.frame(
      c(sort(out[sorted,]), sort(out[!sorted,])),
      stringsAsFactors = FALSE
    ) |>
      setNames(paste(compos, collapse = "-"))
  }
  
  return(out)
}