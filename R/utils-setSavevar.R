setSavevar <- function(content, savevar, lv = 1, root = "root") {

  lapply(
    names(content),
    function(label) {
      sub.content <- content[[label]]
      type.content <- typeof(sub.content)
      sub.savevar <- savevar[[label]]
      type.savevar <- typeof(sub.savevar)

      if (is.reactivevalues(sub.savevar)) {
        if (!is.data.frame(sub.content) &&
          is.list(sub.content)) {
          x <- setSavevar(content[[label]], savevar[[label]], lv = lv + 1, root = label)
        }
        else {
          x <- sub.content
        }
      }
      else {
        x <- sub.content
      }

      isolate(savevar[[label]] <- x)
      return(NULL)
    }
  )

  return(savevar)
}
