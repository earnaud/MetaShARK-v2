readHTMLfromMD <- function(file) {
  if(isFALSE(file.exists(file)))
    return("<p></p>")
  
  .tmp.file <- tempfile(fileext = ".html")
  rmarkdown::pandoc_convert(
    file,
    from = "markdown_strict",
    to = "html",
    output = .tmp.file
  )
  .out <- xml2::read_html(.tmp.file) %>% 
    textutils::HTMLdecode()
  .out <- ifelse(
    grepl(pattern = "<body>.*</body>", .out),
    gsub(".*<body>(.*)</body>.*", "\\1", .out),
    gsub(".*", "", .out)
  )
  return(.out)
}