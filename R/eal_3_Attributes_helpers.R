#' @noRd
buildAttributesTree <- function(main.env) {
  if(main.env$EAL$page == 3){
    .tables <- isolate(main.env$local.rv$md.tables)
    req(isContentTruthy(.tables))
    
    devmsg("compute tree", tag = "attributes")
    
    lapply(
      names(.tables),
      # Files node
      function(file.name) {
        structure(lapply(
          .tables[[file.name]]$attributeName,
          # Attributes leaves
          file.name = file.name,
          function(attributeName, file.name){
            # Render leaf
            structure(
              attributeName,
              # sttype="default",
              sticon="fa fa-table"
            )
          }
        ) |>
          setNames(nm = .tables[[file.name]]$attributeName),
        stopened = TRUE,
        # sttype = "root",
        sticon = "fa fa-file"
        )
      }
    ) |> 
      setNames(nm = names(.tables))
  } else {
    list()
  }
}