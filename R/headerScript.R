#' @import shiny
.headerScript <- function(){
  # minor functions
  rm(list = ls())
  options(shiny.reactlog=TRUE)
  
  .sourceModules()
}

#' @import EML
#' 
#' Provide global variables for server part
#' 
# .globalScript ----
.globalScript <- function(){
  HOME = fs::path_home()
  DP.PATH <- paste0(HOME,"/dataPackagesOutput/emlAssemblyLine/")
  dir.create(DP.PATH, recursive = TRUE, showWarnings = FALSE)
  
  THRESHOLD = list(
    dp_data_files = 500000
  )
  
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
  
  globals <- list(
    DEFAULT.PATH = list(DP.PATH),
    FORMAT = list(DATE = DATE.FORMAT,
                  HOUR = HOUR.FORMAT,
                  UNIT = UNIT.LIST)
  )
  
  # output
  return(globals)
}

.sourceModules <- function(){
  # welcome module
  source("R/modules/welcome/welcomeUI.R")
  # documentation module
  source("R/modules/documentation/documentation.R")
  source("R/modules/documentation/documentation_functions.R")
  # about module
  source("R/modules/about/about.R")
  source("R/modules/about/aboutUI.R")
}