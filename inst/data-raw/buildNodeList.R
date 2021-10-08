library(dataone)

staging.nodes <- listMemberNodes(D1Client("STAGING"))
prod.nodes <- listMemberNodes(D1Client("PROD"))

messages <- c()

do.staging.drop <- c()
sapply(1:length(staging.nodes), function(i) {
  ping <- try(httr::GET(staging.nodes[[i]]@baseURL)$status == 200)
  if(is.logical(ping)) {do.staging.drop <<- c(do.staging.drop, ping)} else {
    messages <<- c(
      messages, 
      if(grepl("Connection timed out after 10000 milliseconds", ping[[1]]))
        paste(staging.nodes[[i]]@baseURL, ": timeout ping") else
          if(grepl("CAfile: none CRLfile: none", ping[[1]]))
            paste(staging.nodes[[i]]@baseURL, ": expired openssl certificate?") else
              ping[[1]]
    )
    do.staging.drop <<- c(do.staging.drop, FALSE)
  }
})

messages <- c()

do.prod.drop <- c()
sapply(1:length(prod.nodes), function(i) {
  ping <- try(httr::GET(prod.nodes[[i]]@baseURL)$status == 200)
  if(is.logical(ping)) {do.prod.drop <<- c(do.prod.drop, ping)} else {
    messages <<- c(
      messages, 
      if(grepl("Connection timed out after 10000 milliseconds", ping[[1]]))
        paste(prod.nodes[[i]]@baseURL, ": timeout ping") else
          if(grepl("CAfile: none CRLfile: none", ping[[1]]))
            paste(prod.nodes[[i]]@baseURL, ": expired openssl certificate?") else
              ping[[1]]
    )
    do.prod.drop <<- c(do.prod.drop, FALSE)
  }
})

# do something with messages here

staging.list <- staging.nodes[do.staging.drop]
prod.list <- prod.nodes[do.prod.drop]

.list <- c(staging.list, prod.list)

.table <- sapply(1:length(.list), function(i) {
  c(
    name = .list[[i]]@name,
    mn = .list[[i]]@identifier,
    cn = ifelse(i <= length(staging.list), "STAGING", "PROD"),
    description = .list[[i]]@description
  )
}) |> setNames(1:length(.list)) |> as.data.frame() |> t()

data.table::fwrite(.table, system.file("resources/registeredEndpoints.txt", package = "MetaShARK"))