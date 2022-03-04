is.shp.dir <- function(dir.path) {
  if( !fs::is_dir(dir.path) )
    return(FALSE)
  # else
  dir.files <- dir(dir.path)
  # check for main file of the shp ESRI format
  return(any(grepl("shp$", dir.files)))
}

wkt2list <- function(wkt) {
  wkt <- gsub(" ", "", wkt)
  wkt <- gsub("\n", "", wkt)
  wkt <- gsub("\\[", " = list(", wkt)
  wkt <- gsub("]", ")", wkt)
  wkt <- gsub("list\\(([a-zA-Z]+),", "list(\"\\1\",", wkt)
  wkt <- gsub(",([a-zA-Z]+),", ",\"\\1\",", wkt)
  wkt <- gsub(",([a-zA-Z]+))", ",\"\\1\")", wkt)
  # browser()
  eval(str2expression(wkt))
}

