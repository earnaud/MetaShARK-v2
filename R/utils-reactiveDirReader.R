reactiveDirReader <- function(repo, session, ...) {
  dir.args <- as.list(...)
  files <- reactiveVal(character())
  
  files.poll <- reactivePoll(
    1000,
    session,
    function()
      identical(files(), do.call(dir, c(repo, dir.args))),
    function()
      dir(repo)
  )
  
  observe({
    files(files.poll())
  })
  
  
  return(files.poll)
}
