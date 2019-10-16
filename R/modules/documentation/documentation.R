### documentation.R

### UI ###
docUI <- function(id, IM){
  ns <- NS(id)
  
  # var initialization
  moduleNames <- sub("^[0-9]+_(.*)$","\\1",names(docGuideline))
  # avoid 404
  moduleNames <- moduleNames[moduleNames != "eml-unit Type Definitions"]
  
  # UI output
  tagList(
    fluidRow(
      box(width = 12,
          title = "Check original documentation",
          "This documentation is brought to you from XSD files downloaded from
          <a href=''>this git</a>. You can visit the original documentation by 
          chosing a module name and clicking the 'GO' button below:",
          column(6,
                 selectInput(ns("select-module"), NULL,
                             moduleNames, selected = moduleNames[25], 
                             multiple =FALSE)
          ),
          column(6,
                 actionButton(ns("visit-module"), "Go !",
                              icon = icon("external-link-alt"))
          )
      )
    ),
    fluidRow(
      # search sidebar
      column(5,
             box(shinyTree(outputId = ns(IM[2]), # render tree
                           search = TRUE,
                           theme = "proton"),
                 width = 12
             )
             , style = sidebarStyle
      ),
      # display main panel
      column(7,
             div(box(uiOutput( ns(IM[4]) ), # XPath
                     uiOutput( ns(IM[3]) ), # Documentation
                     width = 12
                 )
                 , style = mainpanelStyle
             )
      )
    )
  )
}

### SERVER ###
documentation <- function(input, output, session, IM, tree = docGuideline, ns.index = nsIndex){
  
  observeEvent(input$`visit-module`, {
    url <- paste0("https://nceas.github.io/eml/schema/",
                  input$`select-module`,
                  "_xsd.html")
    url <- sub(" +","",url)
    browseURL(url)
  })
  
  # render tree
  output[[IM[2]]] <- renderTree(tree)
  
  # output selected node
  output[[IM[3]]] <- renderText({
    jstree <- input[[IM[2]]]
    if (is.null(jstree)){
      "None"
    } else{
      node <- get_selected(tree = jstree)
      if(length(node) == 0)
        return("(Select a node first)")
      docPath <- gsub("^/","",
                       paste(
                         paste(attr(node[[1]], "ancestry"), collapse="/"),
                         unlist(node),
                         sep="/")
      )
      output[[IM[4]]] <- renderText(as.character(h3(docPath)))
      
      # fetch the systemGuideLine path in the userGuideLine list
      systemPath <- followPath(tree, docPath)
      
      if(!is.character(systemPath))
        systemPath <- commonPath(systemPath,unlist(node))
      # return(userPath)
      
      # fetch the eml-xsd content in the systemGuideLine list
      systemContent <- followPath(systemGuideline, systemPath)
      out <- extractContent(systemContent, nsIndex = ns.index)
      return(out)
    }
  })
  
}


