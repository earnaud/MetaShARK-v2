#' @noRd
# shortcut for oberver in EAL pages
.browse_dev <- function(main_env, page) {
  observeEvent(main_env$dev_browse(), {
    if (main_env$current_tab() == "fill" && main_env$EAL$page == page)
      browser()
  }, label = sprintf("EAL%s dev browser", page))
}