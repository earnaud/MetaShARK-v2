annotationsUI <- function(id) {
  actionButton(NS(id, "annotate"), "", icon("project-diagram"))
}

annotations <- function(id, label, main.env) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$annotate, {
      modalDialog(
        title = sprintf("Annotate %s", label),
        tagList(
          selectizeInput()
        ),
        footer = tags$span(
          modalButton("Dismiss"),
          actionButton("validate", "Validate")
        )
      )
    })
  })
}

#' annotationsUI <- function(id) {
#'   ns <- NS(id)
#'
#'   return(
#'     tagList(
#'       useShinyjs(),
#'       tags$div(
#'         tags$br(),
#'         actionButton(NS(id, "addui"), "", icon("plus")),
#'         fluidRow(
#'           column(11,
#'             column(4, "Subject"),
#'             column(4, "Predicate"),
#'             column(4, "Object")
#'           ),
#'           style = "background-color: white; text-align: center"
#'         ),
#'         uiOutput(NS(id, "annotation_fields")),
#'         tags$div(id = NS(id, "inserthere")),
#'         tags$br(),
#'         class = "inputBox wip"
#'       ) # end of fluidPage
#'     )
#'   ) # end of return
#' }
#'
# @importFrom cedarr search
#' #' @importFrom shinyjs disabled enable disable
#' annotations <- function(input, output, session, save.variable, main.env) {
#'   ns <- session$ns
#'
#'   # Initialize variables ----
#'   rv <- reactiveValues(
#'     annotations = data.frame(
#'       id = character(),
#'       element = character(),
#'       context = character(),
#'       subject = character(),
#'       predicate_label = character(),
#'       predicate.uri = character(),
#'       object.label = character(),
#'       object.uri = character()
#'     ),
#'     file = paste(
#'       save.variable$SelectDP$dp.metadata.path,
#'       "annotations.txt",
#'       collapse = "/"
#'     )
#'   )
#'   if(file.exists(rv$file))
#'     rv$annotations <- fread(rv$file)
#'   else
#'     fwrite(rv$annotations, rv$file)
#'
#'   # Set UI ----
#'   # Default terms
#'   # Custom terms
#'   onclick("addui", {
#'     onInsertUI <- modalDialog(
#'       title = "Select an element to annotate",
#'       ... = tagList(
#'
#'       ),
#'       footer = tagList(
#'         modalButton("Cancel"),
#'         actionButton(
#'           NS(id, "insert"),
#'           "New annotation"
#'         ) %>% disabled
#'       )
#'     )
#'
#'
#'
#'     insertAnnot(
#'       as.character(id),
#'       rv,
#'       ns,
#'       main.env
#'     )
#'   })
#'
#'   # Process data ----
#'
#'   # Output ----
#'   return(save.variable)
#' }
#'
#' #' @title insertPersonnelInput
#' #'
#' #' @description helper function to insert PersonnelInput* functions. Calling this from
#' #' a shiny server will insert PersonnelInputUI and create its server part. Provided with
#' #' features to delete them.
#' #'
#' #' @import shiny
#' insertAnnotInput <- function(id, rv, ns, main.env, value = NULL) {
#'
#'   # initialize IDs ----
#'   div_id <- id
#'   site_id <- paste0("site_", id)
#'   rmv_id <- paste0("rmv_", id)
#'
#'   # Proper module server ----
#'   # insert new UI
#'   newUI <- AnnotModUI(
#'     ns(id), div_id, site_id, rmv_id,
#'     value = value
#'   )
#'   insertUI(
#'     selector = paste0("#", NS(id, "inserthere")),
#'     ui = newUI
#'   )
#'
#'   # create associated server
#'   rv <- callModule(
#'     AnnotMod, id, # module args
#'     main.env, rv, # reactiveValues
#'     rmv_id, site_id, div_id, # renderUI ids
#'     value = value # set saved
#'   )
#'
#'   # Output ----
#'   return(rv)
#' }
#'
#' #' @title AnnotModUI
#' #'
#' #' @description module to document EML annotation
#' #'
#' #' @importFrom shinyBS bsTooltip
#' AnnotModUI <- function(id, div_id, site_id, rmv_id, value = NULL) {
#'   ns <- NS(id)
#'
#'   value <- if(isContentTruthy(value)){
#'     value[value$id == div_id,]
#'   } else {
#'     rep(NA, 3)
#'   }
#'
#'   tags$div(
#'     id = site_id,
#'     fluidRow(
#'       class = "inputBox",
#'       column(11,
#'         column(4,
#'           actionButton(
#'             paste0("subject_", id),
#'             if(is.na(value[1])) "[Subject]" else value[1]
#'           )
#'         ),
#'         column(4,
#'           actionButton(
#'             paste0("predicate_", id),
#'             if(is.na(value[2])) "[Predicate]" else value[2]
#'           )
#'         ),
#'         column(4,
#'           actionButton(
#'             paste0("object_", id),
#'             if(is.na(value[3])) "[Object]" else value[3]
#'           )
#'         )
#'       ),
#'       column(1,
#'         if (is.null(value)) {
#'           actionButton(
#'             ns(rmv_id),
#'             "",
#'             icon("trash"),
#'             class = "danger"
#'           )
#'         },
#'         style = "padding-left: 0"
#'       )
#'     )
#'   )
#' }
#'
#' #' @title AnnotMod
#' #'
#' #' @describeIn AnnotModUI
#' #'
#' #' @import shiny
#' #' @importFrom rorcid as.orcid orcid_person orcid_employments orcid_email orcid_fundings
#' #' @importFrom stringr str_extract
#' AnnotMod <- function(input, output, session, main.env,
#'   rv, rmv_id, site_id, ref, role = NULL, saved = NULL) {
#'   ns <- session$ns
#'
#'   # Variable initialization ----
#'   if(!is.null(saved)){
#'     value <- saved[saved$id == ref,]
#'   } else {
#'     value <- NULL
#'   }
#'
#'   local.rv <- reactiveValues(
#'     ref = ref,
#'     id = if(!is.null(value)) value$id
#'       else character(),
#'     element = if(!is.null(value)) value$element
#'       else character(),
#'     context = if(!is.null(value)) value$context
#'       else character(),
#'     subject = if(!is.null(value)) value$subject
#'       else "[Subject]",
#'     predicate_label = if(!is.null(value)) value$predicate_label
#'       else "[Predicate]",
#'     predicate.uri = if(!is.null(value)) value$predicate.uri
#'       else character(),
#'     object.label = if(!is.null(value)) value$object.label
#'       else "[Object]",
#'     object.uri = if(!is.null(value)) value$object.uri
#'       else character()
#'   )
#'
#'   # Display ontology access ====
#'   onclick(paste0("subject_", id), {
#'     local.rv <- ontoloGUI("subject", local.rv)
#'   })
#'   onclick(paste0("predicate_", id), {
#'     local.rv <- ontoloGUI("predicate", local.rv)
#'   })
#'   onclick(paste0("object_", id), {
#'     local.rv <- ontoloGUI("object", local.rv)
#'   })
#'
#'   # Metadata save ----
#'   observe({
#'     req(
#'       !is.null(value) ||
#'         (any(grepl(rmv_id, names(input))) &&
#'             input[[rmv_id]] < 1)
#'     )
#'     personnel <- isolate(rv$Personnel)
#'     # Fetch correct index
#'     ind <- if (ref %in% personnel$id) {
#'       match(ref, personnel$id) # find its index
#'     }
#'     else {
#'       dim(personnel)[1] + 1
#'     }
#'
#'     # print values into rv at selected index
#'     localValues <- printReactiveValues(local.rv)
#'     localValues <- localValues[colnames(personnel)]
#'     localValues[which(!sapply(localValues, isTruthy))] <- ""
#'     isolate(rv$Personnel[ind,] <- localValues)
#'   })
#'
#'   # Remove UI====
#'   if(is.null(role))
#'     onclick(rmv_id, {
#'       # unload the RV
#'       ind <- match(ref, rv$Personnel$id)
#'       rv$Personnel <- rv$Personnel %>% slice(-ind)
#'
#'       # remove the UI
#'       removeUI(selector = paste0("#", site_id), immediate = TRUE)
#'     })
#'
#'   # Output ----
#'   return(rv)
#' }
