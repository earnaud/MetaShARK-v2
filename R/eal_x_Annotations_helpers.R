getPredicate <- function(api.key, ontology = "OBOREL", predicate) {
  if(missing(api.key) || !isTruthy(api.key)) {
    warning("No api key provided to getPredicate. Returning NULL.")
    return(NULL)
  }
  
  # Check `ontology` is available in BioPortal
  predicate.classes <- cedarr::accessOntology(api.key, ontology, item = "classes")
  # Search for `predicate` in `ontology` classes
  predicate.id <- functionToFindApproximatelyATermAmongOthers()
  predicate.found <- cedarr::accessClass(api.key, ontology, predicate.id)
  
  return(predicate.found)
}

getObject <- function(ontology, object) {
  
}

# TODO Build a row:
# 
# Idea
# 
# Use a modalDialog for each annotation written.
# 
# Content
# 
# 1. Id: /%1/%2/... 
# values according to the context (as named as possible)
# Levels:
# /dataset
# /<file.name>
#   /<attribute>
# /<person.name>
# 
# 2. element: /%1/%2
# values according to the context (unnamed)
# /dataset
# /dataTable OR /otherEntity
#   /attribute
# /ResponsibleParty
# 
# 3. context: %1
# upper level of current item
# eml
# dataset
#   <file.name> (from dataset)
# dataset
# 
# 4. subject: 
# last item of Id
# 
# 5. predicate_label:
# name of predicate
# 
# 6. predicate URI
# uri of predicate
# 
# 7 & 8. same as predicate for object
# have a search engine to get 