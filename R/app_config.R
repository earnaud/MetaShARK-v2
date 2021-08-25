app_sys <- function(...){
  system.file(..., package = "MetaShARK")
}

#' @importFrom config get
#' 
#' @noRd
get_golem_config <- function(
  value, 
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE", 
    Sys.getenv(
      "R_CONFIG_ACTIVE", 
      "default"
    )
  ), 
  use_parent = TRUE
){
  config::get(
    value = value, 
    config = config, 
    # Modify this if your config file is somewhere else:
    file = app_sys("golem-config.yml"), 
    use_parent = use_parent
  )
}
