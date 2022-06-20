set_date_selection <- function(dates_sample) {
  stopifnot(grepl("[0-9]", dates_sample))
  # number of items in a date
  compo_sample <- as.character(dates_sample[1]) |>
    strsplit(split = "[[:punct:]]") |>
    unlist()
  potentialities <- rep(
    list(c("YYYY", "YY", "MM", "DD", "hh", "mm", "ss")),
    length(compo_sample)
  )

  # Set default possible choices
  choices <- expand.grid(potentialities, stringsAsFactors = FALSE)
  choices <- choices[!sapply(seq_along(choices), function(ind) {
    row <- unlist(choices[ind, ])
    any(duplicated(row)) || (any(row == "YYYY") && any(row == "YY"))
  }), ]
  choices <- sapply(seq_along(choices), function(ind) {
    row <- unlist(choices[ind, ])
    paste(row, collapse = "-")
  })

  # Try to guess
  guessed_formats <- try({
    sapply(dates_sample, guessDateTimeFormat) |>
      unlist() |>
      table()
  })
  # If failed, return raw format choices
  if (class(guessed_formats) == "try-error") {
    return(choices)
  }
  guessed_formats <- guessed_formats[guessed_formats == max(guessed_formats)]
  coded_finds <- names(guessed_formats) |>
    gsub(pattern = "YYYY", replacement = "YY") |>
    gsub(pattern = "-", replacement = "") |>
    gsub(pattern = "YY", replacement = "4") |>
    gsub(pattern = "MM", replacement = "5") |>
    gsub(pattern = "DD", replacement = "6") |>
    gsub(pattern = "hh", replacement = "3") |>
    gsub(pattern = "mm", replacement = "2") |>
    gsub(pattern = "ss", replacement = "1")
  guessed_formats <- names(guessed_formats)[
    order(coded_finds, decreasing = TRUE)
  ]
  choices <- list(
    suggested = guessed_formats,
    other = choices[!(choices %in% guessed_formats)]
  )

  return(choices)
}


#' @importFrom lubridate guess_formats
guessDateTimeFormat <- function(date, lubridate_formats) {
  # guess format by lubridate
  date.formats <- date |>
    lubridate::guess_formats(lubridate_formats) |>
    unique()
  # are there separators? if there are, remove mismatching formats like "YYDD"
  if ("[[:punct:]]" %grep% date.formats) {
    date.formats <- date.formats[!grepl("%[a-zA-Z]%", date.formats)]
  }

  return(date.formats)
}


#' Convert lubridate to common format
convertLubridateFormat <- function(date.formats) {
  date.formats |>
    gsub(pattern = "%Y", replacement = "YYYY") |>
    gsub(pattern = "%O?y", replacement = "YY") |>
    gsub(pattern = "%O?m", replacement = "MM") |>
    gsub(pattern = "%O?d", replacement = "DD") |>
    gsub(pattern = "%O?H", replacement = "hh") |>
    gsub(pattern = "%O?M", replacement = "mm") |>
    gsub(pattern = "%S", replacement = "ss") |>
    gsub(pattern = "%T", replacement = "hh:mm:ss") |>
    gsub(pattern = "%R", replacement = "hh:mm")
}
