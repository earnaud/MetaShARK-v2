annotationsUI <- function(id) {
  ns <- NS(id)

  return(
    tagList(
      useShinyjs(),
      tags$div(
        tags$br(),
        actionButton(ns("addui"), "", icon("plus")),
        fluidRow(
          column(11,
            column(4, "Subject"),
            column(4, "Predicate"),
            column(4, "Object")
          ),
          style = "background-color: white; text-align: center"
        ),
        uiOutput(ns("annotation_fields")),
        tags$div(id = ns("inserthere")),
        tags$br(),
        class = "inputBox wip"
      ) # end of fluidPage
    )
  ) # end of return
}

#' @importFrom cedarr search
#' @importFrom shinyjs disabled enable disable
annotations <- function(input, output, session, savevar, main.env) {
  ns <- session$ns

  # Initialize variables ----
  rv <- reactiveValues(
    annotations = data.frame(
      id = character(),
      element = character(),
      context = character(),
      subject = character(),
      predicate_label = character(),
      predicate_uri = character(),
      object_label = character(),
      object_uri = character()
    ),
    file = paste(
      savevar$emlal$SelectDP$dp_metadata_path,
      "annotations.txt",
      collapse = "/"
    )
  )
  if(file.exists(rv$file))
    rv$annotations <- fread(rv$file)
  else
    fwrite(rv$annotations, rv$file)
  
  # Set UI ----
  # Default terms
  # Custom terms
  onclick("addui", {
    onInsertUI <- modalDialog(
      title = "Select an element to annotate",
      ... = tagList(
        
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          ns("insert"),
          "New annotation"
        ) %>% disabled
      )
    )
    
    
    
    insertAnnot(
      as.character(id),
      rv,
      ns,
      main.env
    )
  })
  
  # Process data ----
  
  # Output ----
  return(savevar)
}

#' @title insertPersonnelInput
#'
#' @description helper function to insert PersonnelMod* functions. Calling this from
#' a shiny server will insert PersonnelModUI and create its server part. Provided with
#' features to delete them.
#' 
#' @importFrom shiny insertUI callModule 
insertAnnotInput <- function(id, rv, ns, main.env, default = NULL) {
  
  # initialize IDs -----------------------------------------------------
  div_id <- id
  site_id <- paste0("site_", id)
  rmv_id <- paste0("rmv_", id)
  
  # Proper module server -----------------------------------------------------
  # insert new UI
  newUI <- AnnotModUI(
    ns(id), div_id, site_id, rmv_id, 
    default = default
  )
  insertUI(
    selector = paste0("#", ns("inserthere")),
    ui = newUI
  )
  
  # create associated server
  rv <- callModule(
    AnnotMod, id, # module args
    main.env, rv, # reactiveValues
    rmv_id, site_id, div_id, # renderUI ids
    default = default # set saved
  )
  
  # Output -----------------------------------------------------
  return(rv)
}

#' @title AnnotModUI
#'
#' @description module to document EML annotation
#'
#' @importFrom shinyBS bsTooltip
AnnotModUI <- function(id, div_id, site_id, rmv_id, default = NULL) {
  ns <- NS(id)
  
  default <- if(checkTruth(default)){
    default[default$id == div_id,]
  } else {
    rep(NA, 3)
  }
  
  tags$div(
    id = site_id,
    fluidRow(
      class = "inputBox",
      column(11,
        column(4,
          actionButton(
            paste0("subject_", id),
            if(is.na(default[1])) "[Subject]" else default[1]
          )
        ),
        column(4,
          actionButton(
            paste0("predicate_", id),
            if(is.na(default[2])) "[Predicate]" else default[2]
          )
        ),
        column(4,
          actionButton(
            paste0("object_", id),
            if(is.na(default[3])) "[Object]" else default[3]
          )
        )
      ),
      column(1,
        if (is.null(default)) {
          actionButton(
            ns(rmv_id),
            "",
            icon("trash"),
            class = "danger"
          )
        },
        style = "padding-left: 0"
      )
    )
  )
}

#' @title AnnotMod
#'
#' @describeIn AnnotModUI
#'
#' @importFrom shiny insertUI removeUI
#' @importFrom rorcid as.orcid orcid_person orcid_employments orcid_email orcid_fundings
#' @importFrom stringr str_extract
AnnotMod <- function(input, output, session, main.env,
  rv, rmv_id, site_id, ref, role = NULL, saved = NULL) {
  ns <- session$ns
  
  # Variable initialization -----------------------------------------------------
  if(!is.null(saved)){
    value <- saved[saved$id == ref,]
  } else {
    value <- NULL
  }
  
  localRV <- reactiveValues(
    ref = ref,
    id = if(!is.null(default)) default$id
      else character(),
    element = if(!is.null(default)) default$element
      else character(),
    context = if(!is.null(default)) default$context
      else character(),
    subject = if(!is.null(default)) default$subject
      else "[Subject]",
    predicate_label = if(!is.null(default)) default$predicate_label
      else "[Predicate]",
    predicate_uri = if(!is.null(default)) default$predicate_uri
      else character(),
    object_label = if(!is.null(default)) default$object_label
      else "[Object]",
    object_uri = if(!is.null(default)) default$object_uri
      else character()
  )
  
  # Display ontology access ====
  onclick(paste0("subject_", id), {
    localRV <- ontoloGUI("subject", localRV)
  })
  onclick(paste0("predicate_", id), {
    localRV <- ontoloGUI("predicate", localRV)
  })
  onclick(paste0("object_", id), {
    localRV <- ontoloGUI("object", localRV)
  })
  
  # Metadata save -----------------------------------------------------
  observe({
    req(
      !is.null(default) ||
        (any(grepl(rmv_id, names(input))) &&
            input[[rmv_id]] < 1)
    )
    personnel <- isolate(rv$Personnel)
    # Fetch correct index
    ind <- if (ref %in% personnel$id) {
      match(ref, personnel$id) # find its index
    }
    else {
      dim(personnel)[1] + 1
    }
    
    # print values into rv at selected index
    localValues <- printReactiveValues(localRV)
    localValues <- localValues[colnames(personnel)]
    localValues[which(!sapply(localValues, isTruthy))] <- ""
    isolate(rv$Personnel[ind,] <- localValues)
  })
  
  # Remove UI====
  if(is.null(role))
    onclick(rmv_id, {
      # unload the RV
      ind <- match(ref, rv$Personnel$id)
      rv$Personnel <- rv$Personnel %>% slice(-ind)
      
      # remove the UI
      removeUI(selector = paste0("#", site_id), immediate = TRUE)
    })
  
  # Output -----------------------------------------------------
  return(rv)
}