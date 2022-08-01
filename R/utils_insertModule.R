#' @noRd
insertModule <- function(module_name, id, main_env) {
  do.call(
    switch(
      module_name,
      "Data_Files" = insertDataFileInput,
      "Personnel" = insertPersonnelInput
    ), 
    args = list(id = id, main_env = main_env)
  )
}

insertDataFileInput <- function(id, main_env) {
  # create the UI
  new_ui <- DataFileInputUI(id$ui, main_env)
  # insert the UI
  insertUI(selector = "#inserthere_eal2", ui = new_ui, immediate = TRUE)
  # create the server
  DataFileInput(id$server, main_env)
}

insertPersonnelInput <- function(id, main_env) {
  # create the UI
  new_ui <- PersonnelInputUI(id$ui, main_env)
  # insert the UI
  insertUI(selector = "#inserthere_eal7", ui = new_ui, immediate = TRUE)
  # create the server
  PersonnelInput(id$server, main_env)
}