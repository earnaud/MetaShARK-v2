# fill_functions.R

# root input
fillExplore <- function(li,              # currently examined list
                        id = "none",     # vector of id, built for each input UI element
                        depth = 0,       # the current depth - e.g. number of times fillExplore() was applied
                        sequence = NULL, # the depth of the last entered sequence 
                        choice = NULL,   # the depth of the last entered choice
                        lastName         # if no name is precised at a level, which was the last name to remember?
                        ){
  ui <- lapply(li[!grepl("annotation|R-Attributes", attr(li,"names"))],
               function(ll){
                 # set variables
                 ind <- unname(which(unlist(lapply(li, identical, ll))))
                 llName <- attr(li,"names")[ind]
                 arguments <- as.list(parent.env(environment()))
                 arguments$li <- ll
                 arguments$id <- arguments$li$`R-Attributes`$XSDPATH # unique for each element
                 arguments$depth <- arguments$depth+1
                 
                 # go for non direct recursion
                 # NOTA: the following is obviously not optimized but at least is LEGIBLE
                 # thanks me later ;)
                 # sequence
                 if(grepl("sequence",llName)){
                   arguments$sequence <- arguments$depth
                   return(do.call(sequenceUI, arguments))
                 }
                 # choice
                 if(grepl("choice",llName)){
                   arguments$choice <- arguments$depth
                   return(do.call(choiceUI, arguments))
                 }
                 # element
                 if(grepl("element:",llName)){
                   return(do.call(elementUI, arguments))
                 }
                 # union
                 if(grepl("union",llName)){
                   return(do.call(unionUI, arguments))
                 }
                 # simpleType, simpleContent, extension
                 if(grepl("simpleType|simpleContent|extension",llName)){
                   return(do.call(simpleTypeUI, arguments))
                 }
                 # restriction
                 if(grepl("restriction",llName)){
                   return(do.call(restrictionUI, arguments))
                 }
                 # complexType, complexContent
                 if(grepl("complexType|complexContent",llName)){
                   return(do.call(complexTypeUI, arguments))
                 }
                 # attributes
                 if(grepl("attribute",llName)){
                   # browser()
                   return("attribute")
                 }
                 
                 return(tags$u(llName))
               })
  
  return(ui)
}

# sequence input
sequenceUI <- function(li, id, depth, sequence, choice, lastName){
  content <- do.call(fillExplore, as.list(environment()))
  return(box(content, title = lastName, width = 12))
}

# choice
choiceUI <- function(li, id, depth, sequence, choice, lastName){
  content <- do.call(fillExplore, as.list(environment()))
  return(lapply(content, box, width=12, status = "warning"))
}

# element input
elementUI <- function(li, id, depth, sequence, choice, lastName){
  arguments <- as.list(environment())
  if(!is.null(li$`R-Attributes`$NAME))
    name <- li$`R-Attributes`$NAME 
  else{
    name <- "element"
  }
  if(length(li[!grepl("annotation|R-Attributes",attr(li,"names"))]) > 0){
    arguments$lastName <- name
    content <- do.call(fillExplore, arguments)
  }
  else{
    content <- textInput(id, name)
  }
  return(content)
}

# union
unionUI <- function(li, id, depth, sequence, choice, lastName){
  return(do.call(fillExplore, as.list(environment())))
}

# simpleType
simpleTypeUI <- function(li, id, depth, sequence, choice, lastName){
  return(do.call(fillExplore, as.list(environment())))
}

# restriction
restrictionUI <- function(li, id, depth, sequence, choice, lastName){
  values <- lapply(li[!grepl("annotation|R-Attributes", attr(li,"names"))],
                   function(sub)
                     return(sub$`R-Attributes`$VALUE)
                   )
  if(length(values) == 0)
    return()
  return(selectInput(id, lastName, values))
}

# complexType
complexTypeUI  <- function(li, id, depth, sequence, choice, lastName){
  return(do.call(fillExplore, as.list(environment())))
}

#_UI <- function(li, id, depth, sequence, choice, lastName)