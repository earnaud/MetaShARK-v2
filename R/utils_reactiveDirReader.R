reactiveDirReader <- function(repo, session, pattern, full.names = FALSE) {
  files <- reactiveVal(character())
  
  files.poll <- reactivePoll(
    1000,
    session,
    function()
      identical(files(), dir(repo, pattern)),
    function()
      dir(repo, pattern, full.names = full.names)
  )
  
  observe({
    files(files.poll())
  })
  
  
  return(files.poll)
}