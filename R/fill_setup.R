# Main save_variable ====

#' @import shiny
#'
#' @noRd
initReactive <- function(sub.list = NULL, save_variable = NULL, main_env) {
  if (!is.null(sub.list) && is.null(save_variable)) {
    stop("Attempt to initialize save_variable's sub.list without save_variable.")
  }
  if (!(is.null(sub.list) || sub.list %in% c("emlal", "metafin"))) {
    stop("Attempt to initialize save_variable with inconsistent arguments")
  }

  # re-creates a whole empty save_variable
  if (is.null(sub.list)) {
    save_variable <- reactiveValues()
  }
  # emlal reactivelist management
  else if (sub.list == "emlal") {
    save_variable <- reactiveValues(
      step = 1,
      # quick = FALSE,
      creator = character(),
      history = isolate(main_env$EAL$history),
      SelectDP = reactiveValues(
        dp_name = NULL,
        dp_path = NULL,
        dp_metadata_path = NULL,
        dp_data_path = NULL,
        dp_title = NULL
      ),
      DataFiles = data.frame(stringsAsFactors = FALSE),
      Attributes = reactiveValues(
        content = NA, # list of data tables
        use_catvars = reactiveVal(FALSE) # called as use_catvars() later
      ),
      CatVars = reactiveValues(),
      GeoCov = reactiveValues(),
      TaxCov = reactiveValues(
        taxa_table = NULL,
        taxa_col = NULL,
        taxa_name_type = NULL,
        taxa_authority = NULL
      ),
      Personnel = reactiveValues(),
      Misc = reactiveValues(
        abstract = reactiveValues(
          content = character(),
          file = character()
        ),
        methods = reactiveValues(
          content = character(),
          file = character()
        ),
        keywords = data.frame(
          keyword = character(),
          keywordThesaurus = character(),
          keyword_set = character()
        ),
        temporal_coverage = c(NA, NA),
        additional_information = reactiveValues(
          content = character(),
          file = character()
        )
      ),
      Annotations = reactiveValues(
        annot_table = data.frame(stringsAsFactors = FALSE)
      )
    )
  }
  # metafin reactivelist management
  else if (sub.list == "metafin") {
    save_variable <- reactiveValues()
  }

  # differential returns
  return(save_variable)
}

#' @import shiny
#'
#' @noRd
setSaveVariable <- function(content, save_variable, lv = 1, root = "root") {
  lapply(
    names(content),
    function(label) {
      sub_content <- content[[label]]
      # type_content <- typeof(sub_content)
      # sub_save_variable <- save_variable[[gsub("_", ".", label)]]
      sub_save_variable <- save_variable[[label]]
      # type_save_variable <- typeof(sub_save_variable)
      if (is.reactivevalues(sub_save_variable)) {
        if (!is.data.frame(sub_content) &&
          is.list(sub_content)) {
          x <- setSaveVariable(
            content[[label]],
            save_variable[[label]],
            lv = lv + 1,
            root = label
          )
        } else {
          x <- sub_content
        }
      } else {
        x <- sub_content
      }

      isolate({
        save_variable[[label]] <- x
      })
      return(NULL)
    }
  )

  return(save_variable)
}