reactiveDirReader <- function(repo, session, pattern) {
  files <- reactiveVal(character())
  
  files.poll <- reactivePoll(
    1000,
    session,
    function()
      identical(files(), dir(repo, pattern)),
    function()
      dir(repo, pattern)
  )
  
  observe({
    files(files.poll())
  })
  
  
  return(files.poll)
}