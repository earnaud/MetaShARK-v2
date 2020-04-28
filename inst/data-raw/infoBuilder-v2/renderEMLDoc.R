library(shiny)

renderEMLDoc <- function(node, path){
  # Variable initialization
  if("documentation" %in% names(node)){
    .ind <- which(names(node) == "documentation")
    node <- c(node[-.ind], node[.ind])
  }
  
  # Action
  ui <- tagList(
    lapply(names(node), function(label){
      if(label == "example")
        tag <- node[which(names(node) == "example")] %>% unlist
      else
        tag <- node[[label]]
      
      # if(label == "documentation")
      #   browser()
      
      .ui <- switch(label,
        appinfo = list(helpText(paste(c("XSD path: ", head(path, -2)), collapse = "/")), renderEMLDoc(tag, path = c(path, label))),
        moduleDocs = renderEMLDoc(tag, path = c(path, label)),
        moduleDescription = renderEMLDoc(tag, path = c(path, label)),
        moduleName = h1(gsub("^eml-","",tag)),
        tooltip = tags$h3(tag),
        section = lapply(tag, function(t){
            tags$p(gsub("( +|\\n)", " ", t))
        }),
        para = tags$p(tag),
        recommendedUsage = tags$span(
          tags$b("Recommended usage:"), 
          gsub("( +|\\n)", " ", tag),
          tags$br()
        ),
        standAlone = tags$span(
          tags$b("Standalone:"), 
          tag,
          tags$br()
        ),
        summary = tagList(
          tags$h4("Summary"),
          tags$p(gsub("( +|\\n)", " ", tag))
        ),
        description = tagList(
          tags$h4("Description"),
          tags$p(gsub("( +|\\n)", " ", tag))
        ),
        example = tagList(
          tags$h4("Examples"),
          tags$ul(
            lapply(tag, tags$li)
          )
        ),
        documentation = tagList(
          tags$hr(),
          tags$h3("Notice"),
          HTML(
            tag %>% 
              gsub("( +|\n)", " ", .) %>%
              gsub("'\\$|\\$'", "<br/>", .) %>%
              gsub("<br/> +<br/>", "<br/>", .) %>% 
              gsub("^ +", "", .) %>% 
              gsub("<br/> *([[:alnum:]]+:)", "<br/><b>\\1<b/>", .)
          )
        ),
        comment = NULL,
        module = NULL,
        # default
        browser()
      ) # end of switch
      
      return(.ui)
    })
  )
  
  return(ui)
}