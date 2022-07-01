library(dataone)

staging_nodes <- listMemberNodes(D1Client("STAGING"))
prod.nodes <- listMemberNodes(D1Client("PROD"))

messages <- c()

do_staging_drop <- c()
sapply(seq_along(staging_nodes), function(i) {
  ping <- try(httr::GET(staging_nodes[[i]]@baseURL)$status == 200)
  if (is.logical(ping)) {
    do_staging_drop <<- c(do_staging_drop, ping) } else {
    messages <<- c(
      messages,
      if (grepl("Connection timed out after 10000 milliseconds", ping[[1]]))
        paste(staging_nodes[[i]]@baseURL, ": timeout ping") else
          if (grepl("CAfile: none CRLfile: none", ping[[1]]))
            paste(
              staging_nodes[[i]]@baseURL,
              ": expired openssl certificate?") else
              ping[[1]]
    )
    do_staging_drop <<- c(do_staging_drop, FALSE)
  }
})

messages <- c()

do_prod_drop <- c()
sapply(seq_along(prod.nodes), function(i) {
  ping <- try(httr::GET(prod.nodes[[i]]@baseURL)$status == 200)
  if (is.logical(ping)) {
    do_prod_drop <<- c(do_prod_drop, ping)} else {
    messages <<- c(
      messages,
      if (grepl("Connection timed out after 10000 milliseconds", ping[[1]]))
        paste(prod.nodes[[i]]@baseURL, ": timeout ping") else
          if (grepl("CAfile: none CRLfile: none", ping[[1]]))
            paste(
              prod.nodes[[i]]@baseURL,
              ": expired openssl certificate?") else
              ping[[1]]
    )
    do_prod_drop <<- c(do_prod_drop, FALSE)
  }
})

# do something with messages here

staging_list <- staging_nodes[do_staging_drop]
prod_list <- prod.nodes[do_prod_drop]

.list <- c(staging_list, prod_list)

.table <- sapply(seq_along(.list), function(i) {
  c(
    name = .list[[i]]@name,
    mn = .list[[i]]@identifier,
    cn = ifelse(i <= length(staging_list), "STAGING", "PROD"),
    description = .list[[i]]@description
  )
}) |> 
  setNames(seq_along(.list)) |> 
  as.data.frame() |>
  t()

data.table::fwrite(
  .table, 
  system.file(
    "resources/registeredEndpoints.txt",
    package = "MetaShARK"))