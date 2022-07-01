saveHTMLasMD <- function(content) {
  .tmp.file <- tempfile(fileext = ".html")
  htmltools::save_html(html = htmltools::HTML(content$content), .tmp.file)
  rmarkdown::pandoc_convert(
    .tmp.file,
    from = "html",
    to = "markdown_strict",
    output = content$file
  )
}

removeDuplicateFiles <- function(filename, path) {
  if (isContentTruthy(filename)) {
    onlyname <- sub("\\..+", "", basename(filename))
    synonyms <- dir(
      path,
      pattern = onlyname,
      full.names = TRUE
    )
    file.remove(synonyms[basename(synonyms) != basename(filename)])
  }
}


#' @export
readHTMLfromMD <- function(file) {
  if (isFALSE(file.exists(file))) {
    return("<p></p>")
  }

  .tmp_file <- tempfile(fileext = ".html")
  rmarkdown::pandoc_convert(
    file,
    from = "markdown_strict",
    to = "html",
    output = .tmp_file
  )
  .out <- xml2::read_html(.tmp_file, encoding = "UTF-8") |>
    textutils::HTMLdecode()
  .out <- ifelse(
    grepl(pattern = "<body>.*</body>", .out),
    gsub(".*<body>(.*)</body>.*", "\\1", .out),
    gsub(".*", "", .out)
  )
  return(.out)
}
