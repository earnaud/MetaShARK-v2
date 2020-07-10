#' @import shiny
setSavevar <- function(content, save.variable, lv = 1, root = "root") {

  lapply(
    names(content),
    function(label) {
      sub.content <- content[[label]]
      type.content <- typeof(sub.content)
      sub.save.variable <- savevar[[label]]
      type.save.variable <- typeof(sub.savevar)

      if (is.reactivevalues(sub.save.variable)) {
        if (!is.data.frame(sub.content) &&
          is.list(sub.content)) {
          x <- setSavevar(content[[label]], save.variable[[label]], lv = lv + 1, root = label)
        }
        else {
          x <- sub.content
        }
      }
      else {
        x <- sub.content
      }

      isolate(save.variable[[label]] <- x)
      return(NULL)
    }
  )

  return(save.variable)
}
