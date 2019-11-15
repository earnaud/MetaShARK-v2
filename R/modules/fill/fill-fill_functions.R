# fill_functions.R

# Manage savevar variable ----
# EMLAL module specific function
# @param sublist: either NULL, "emlal", "metafin" to precise which sublist
#                 to initialize
initReactive <- function(sublist = NULL, savevar = NULL) {
  if (!is.null(sublist) && is.null(savevar)) {
    stop("Attempt to initialize savevar's sublist without savevar.")
  }
  if (!(is.null(sublist) || sublist %in% c("emlal", "metafin"))) {
    stop("Attempt to initialize savevar with inconsistent arguments")
  }

  # re-creates a whole savevar
  if (is.null(sublist)) {
    savevar <- reactiveValues()
  }

  # emlal reactivelist management
  if (is.null(sublist) || sublist == "emlal") {
    savevar$emlal <- reactiveValues(
      step = 0,
      selectDP = reactiveValues(
        dp_name = NULL,
        dp_path = NULL
      ),
      createDP = reactiveValues(
        dp_data_files = NULL
      ),
      templateDP = reactiveValues()
    )
  }

  # metafin reactivelist management
  if (is.null(sublist) || sublist == "metafin") {
    savevar$metafin <- reactiveValues()
  }

  # differential returns
  return(if (is.null(sublist)) {
    savevar
  } else {
    switch(sublist,
      emlal = savevar$emlal,
      metafin = savevar$metafin
    )
  })
}

# set the path and save the savevar
saveReactive <- function(toSave, path, filename) {
  location <- paste0(path, "/", filename, ".rds")
  message("Saving current metadata as:", location, "\n", sep = " ")
  if (file.exists(location)) file.remove(location)
  saveRDS(toSave, location)
}
