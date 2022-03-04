#' reactiveDirReader
#' 
#' Function adapted from `reactivePoll()`. Used to create a reactive data source,
#' which works by periodically polling a directory of non-reactive data sources.
#' 
#' @usage 
#' 
#' files.poll <- reactiveDirReader(path, session, pattern, full.names = FALSE, intervalMillis = 1000)
#' 
#' @param repo (character) Path of the target directory to poll.
#' @param session The user session to associate this file reader with, or NULL if none. If non-null, the reader will automatically stop when the session ends.
#' @param pattern (character) an optional regular expression. Only file names which match the regular expression will be returned.
#' @param full.names (logical) a logical value. If TRUE, the directory path is prepended to the file names to give a relative file path. If FALSE, the file names (rather than paths) are returned.
#' @param intervalMillis (integer) Approximate number of milliseconds to wait between calls to checkFunc. This can be either a numeric value, or a function that returns a numeric value.
#' 
#' @return 
#' 
#' A reactive returning the files contained in the target repository. If folders
#' are returned, their content won't be detailed. If no file is to be found, only
#' returns "No files found." through `validate()`.
#' 
#' @export
reactiveDirReader <- function(repo, session, pattern, full.names = FALSE, intervalMillis = 1000) {
  files <- reactiveVal(character())
  
  files.poll <- reactivePoll(
    intervalMillis,
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