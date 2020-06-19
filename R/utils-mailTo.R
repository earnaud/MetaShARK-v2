#' mailToUI <- function(id, .ui = actionButton, .label = "Mail"){
#'   ns <- NS(id)
#'   
#'   if(!.ui %in% c(actionButton, actionLink))
#'     .ui <- actionButton
#'   
#'   .ui(ns("ui"), .label)
#' }
#' 
#' #' mailTo
#' #' 
#' #' @importFrom shiny reactiveValues modalDialog modalButton tagList tags fluidRow
#' #'  column textInput textAreaInput actionButton icon showModal removeModal 
#' #'  observeEvent isTruthy req isolate
#' #' @importFrom tagsinput tagsTextInput Input
#' #' @importFrom shinyFeedback useShinyFeedback showFeedbackDanger hideFeedback 
#' #' FeedbackDanger
#' #' @importFrom sendmailR sendmail
#' mailTo <- function(input, output, session){
#'   ns <- session$ns
#'   
#'   # Variable initialization ----
#'   mail.pattern <- "[^[@]+@[[:alpha:]]+\\.[[:alpha:]]+"
#'   valid <- reactiveValues(
#'     from = FALSE,
#'     to = FALSE,
#'     subject = FALSE,
#'     body = FALSE
#'   )
#'   
#'   # Modal ----
#'   mailModal <- modalDialog(
#'     {
#'       tagList(
#'         useShinyFeedback(),
#'         tags$div(
#'           fluidRow(
#'             column(12, 
#'               textInput(ns("from"), "From")
#'             )
#'           ),
#'           fluidRow(
#'             column(12, 
#'               tagsTextInput(
#'                 ns("to"), 
#'                 "To", 
#'                 value = "elie.arnaud@mnhn.fr"
#'               )
#'             )
#'           ),
#'           fluidRow(
#'             column(12, textInput(ns("subject"), "Subject"))
#'           ),
#'           class = "inputBox"
#'         ),
#'         fluidRow(
#'           column(12, textAreaInput(ns("body"), "Body")),
#'           class = "inputBox"
#'         )
#'       )
#'     },
#'     footer = tagList(
#'       modalButton("Cancel"),
#'       actionButton(ns("send"), "Send", icon("envelope"))
#'     ), 
#'     size = "l",
#'     easyClose = FALSE
#'   )
#'   
#'   observeEvent(input$ui, {
#'     showModal(mailModal)
#'   })
#'   
#'   # Server ----
#'   observeEvent(input$from, {
#'     valid$from <- grepl(mail.pattern, input$from)
#'     
#'     if(isFALSE(valid$from))
#'       showFeedbackDanger(
#'         "from", 
#'         text = "Invalid sender adress."
#'       )
#'     else
#'       hideFeedback("from")
#'   }, ignoreInit = FALSE)
#'   
#'   observeEvent(input$to, {
#'     .truthy.mails <- isTruthy(input$to)
#'     .valid.mails <- all(sapply(input$to, grepl, pattern = mail.pattern))
#'     valid$to <- all(.truthy.mails, .valid.mails)
#'     
#'     if(isFALSE(valid$to)){
#'       showFeedbackDanger(
#'         "to", 
#'         text = if(isFALSE(.truthy.mails))
#'           "No adresses provided."
#'         else if(isFALSE(.valid.mails))
#'           "One or more invalid adresses."
#'       )
#'     }
#'     else{
#'       hideFeedback("to")
#'     }
#'   }, ignoreInit = FALSE)
#'   
#'   observeEvent(input$subject, {
#'     valid$subject <- isTruthy(input$subject)
#'     
#'     if(isFALSE(valid$subject))
#'       showFeedbackDanger(
#'         "subject", 
#'         text = "No subject provided"
#'       )
#'     else
#'       hideFeedback("subject")
#'   }, ignoreInit = FALSE)
#'   
#'   observeEvent(input$body, {
#'     valid$body <- isTruthy(input$body)
#'     
#'     if(isFALSE(valid$body))
#'       showFeedbackDanger(
#'         "body", 
#'         text = "No text provided"
#'       )
#'     else
#'       hideFeedback("body")
#'   }, ignoreInit = FALSE)
#'   
#'   # Send ----
#'   observeEvent(input$send, {
#'     .valid <- all(printReactiveValues(valid))
#'     
#'     req(isTRUE(.valid))
#'     
#'     from <- paste0("<", isolate(input$from), ">")
#'     to <- paste0("<", strsplit(isolate(input$to), ",")[[1]], ">")
#'     browser()
#'     subject <- isolate(input$subject)
#'     body <- isolate(input$body)
#'     sendmail(from, to, subject, msg)
#'   })
#' }