isShpDir <- function(dir_path) {
  if (!fs::is_dir(dir_path)) {
    return(FALSE)
  }
  # else
  dir_files <- dir(dir_path)
  # check for main file of the shp ESRI format
  return(any(grepl("shp$", dir_files)))
}

wkt2list <- function(wkt) {
  wkt <- gsub(" ", "", wkt)
  wkt <- gsub("\n", "", wkt)
  wkt <- gsub("\\[", " = list(", wkt)
  wkt <- gsub("]", ")", wkt)
  wkt <- gsub("list\\ (([a-zA-Z]+),", "list(\"\\1\",", wkt)
  wkt <- gsub(",([a-zA-Z]+),", ",\"\\1\",", wkt)
  wkt <- gsub(",([a-zA-Z]+))", ",\"\\1\")", wkt)

  eval(str2expression(wkt))
}
