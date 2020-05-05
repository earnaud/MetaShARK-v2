library(shiny)
library(htmltools)

renderEMLDoc <- function(node, path, debug = FALSE){
  
  # Variable initialization ====
  if("documentation" %in% names(node)){
    .ind <- which(names(node) == "documentation")
    node <- c(node[-.ind], node[.ind])
  }
  .examples.done <- FALSE
  
  # Action ====
  ui <- tagList(
    lapply(seq_along(node), function(.ind){
      label <- names(node)[.ind]
      tag <- node[[.ind]]
      
      # Prepare
      if(label == "example") 
        tag <- node[which(names(node) == "example")] %>% unlist
      
      if(label == "ulink"){
        if(grepl("^eml", tag$.attrs["url"]))
          tag$.attrs["url"] <- paste0(
            "https://eml.ecoinformatics.org/schema/",
            gsub(
              "(eml-.*)(.htm)",
              "\\1_xsd\\2",
              tag$.attrs["url"]
            )
          )
      }
      
      # Set UI
      .ui <- switch(label,
        # Recurse
        appinfo = list(
          helpText(paste(c("XSD path: ", head(path, -2)), collapse = "/")),
          renderEMLDoc(tag, path = c(path, label), debug = debug)
        ),
        moduleDocs = renderEMLDoc(tag, path = c(path, label), debug = debug),
        moduleDescription = renderEMLDoc(tag, path = c(path, label), debug = debug),
        section = renderEMLDoc(tag, path = c(path, label), debug = debug),
        itemizedlist = tags$ul(renderEMLDoc(tag, path = c(path, label), debug = debug)),
        listitem = tags$li(renderEMLDoc(tag, path = c(path, label), debug = debug)),
        # Titles
        moduleName = gsub("^eml-","",tag) %>% prettyString %>% h1,
        title = gsub("( +|\\n)", " ", tag) %>% prettyString %>% h2,
        tooltip = tags$h3(tag),
        # Content
        para = if(is.list(tag))
          tags$p(renderEMLDoc(
            tag, 
            path = c(path, label), 
            debug = debug
          ))
        else
          tags$p(gsub("( +|\\n)", " ", tag)),
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
        # Text
        text = gsub("( +|\\n)", " ", tag),
        emphasis = tags$i(gsub("( +|\\n)", " ", tag)),
        ulink = tags$a(
          tag$citetitle,
          href=tag$.attrs["url"]
        ),
        # ulink = HTML(
        #   paste0("<a href = ",attr(tag, "url"),">",tag$citetitle,"</a>")
        # ),
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
        summary = if(is.null(tag))
          NULL
        else
          tagList(
            tags$h4("Summary"),
            tags$p(gsub("( +|\\n)", " ", tag))
          ),
        description = if(is.null(tag))
          NULL
        else if(is.list(tag))
          renderEMLDoc(tag, path = c(path, label), debug = debug)
        else
          tagList(
            tags$h4("Description"),
            tags$p(gsub("( +|\\n)", " ", tag))
          ),
        example = if(isFALSE(.examples.done)) # not done yet
          tagList(
            tags$h4("Examples"),
            tags$ul(
              lapply(tag, tags$li)
            )
          )
        else
          NULL,
        literalLayout = tags$pre(tag),
        code = tags$code(tag),
        comment = NULL,
        module = NULL,
        # default
        browser()
      ) # end of switch
      
      if(label == "example")
        .examples.done <<- TRUE
      
      return(.ui)
    })
  )
  
  # if(path[1] == "25_eml.xsd")
  #   browser()
  
  # Output ====
  
  return(ui)
}