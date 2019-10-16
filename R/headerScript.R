#' @import shiny
.headerScript <- function(){
  # minor functions
  rm(list = ls())
  options(shiny.reactlog=TRUE)
}

.globalScript <- function(){
  ### Global variables ----
  DP.PATH <- paste0(getwd(),"/dataPackagesOutput/emlAssemblyLine/")
  THRESHOLD = list(
    dp_data_files = 500000
  )
  HOME = fs::path_home()
  
  # Date time format strings
  DATE.FORMAT <- combn(rep(c('YYYY','MM','DD'),3),3)
  DATE.FORMAT <- unique(as.list(as.data.frame(DATE.FORMAT[,!apply(DATE.FORMAT, 2, function(y) any(duplicated(y)))], stringsAsFactors = FALSE)))
  DATE.FORMAT <- sapply(c('-','/',':'), function(sep)
    sapply(DATE.FORMAT, paste, collapse = sep)
  )
  HOUR.FORMAT <- c(NA, gsub("YYYY","hh", gsub("MM","mm", gsub("DD","ss", DATE.FORMAT))))
  DATE.FORMAT <- as.vector(rbind(DATE.FORMAT, gsub('Y{4}','YY',DATE.FORMAT)))
  DATE.FORMAT <- DATE.FORMAT[order(DATE.FORMAT, decreasing = TRUE)]
  UNIT.LIST <- c("custom", get_unitList()$units$name)
  
  ## Dir creation ----
  dir.create(DP.PATH, recursive = TRUE, showWarnings = FALSE)
}