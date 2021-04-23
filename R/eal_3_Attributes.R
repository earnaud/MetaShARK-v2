#' @import shiny
#' @importFrom shinyTree shinyTree
#'
#' @noRd
AttributesUI <- function(id) {
  ns <- NS(id)
  
  # unit.list <- isolate(setUnitList(main.env)$unit.list)
  
  tagList(
    tags$p(
      "Even if EML Assembly Line automatically infers most
      of your data's metadata, some steps need you to check
      out. Please check the following attribute, and fill
      the required fields. Once they will be filled, corresponding
      fields will turn to green."
    ),
    fluidRow(
      # Left : shinyTree (output) ====
      column(
        4, 
        tags$h3("Attributes list"),
        shinyTree::shinyTree(
          # TODO add colors
          ns("tree") #,
          # types = "{ 'red-node': {'a_attr' : { 'style' : 'color:red' }},
          #   'green-node': {'a_attr' : { 'style' : 'color:green' }} }"
        ),
        style = "
          overflow: scroll;
          max-height: 60vh;
          background-color: #e4e7ec;
        "
      ),
      # Right: form ====
      column(
        5, offset = 1,
        tags$h3("Attribute description"),
        tags$div(
          id = "no_attribute",
          helpText("No attribute selected")
        ),
        shinyjs::hidden(
          tags$div(
            id = "form",
            style = 'width: 100%;',
            # - filename (output)
            helpText(textOutput(ns("filename"))),
            # - attributeName (output)
            tags$h4(textOutput(ns("attributeName"))),
            # * attributeDefinition ----
            textAreaInput(
              ns("attributeDefinition"), 
              "Description of the attribute",
              value = "!Add description here!",
              resize = "both"
            ) %>%
              shiny::tagAppendAttributes(style = 'width: initial;'),
            # * class ----
            selectInput(
              ns("class"),
              "Dectected class (change if misdetected)",
              choices = c("numeric", "character", "Date", "categorical")
            ),
            # * dateTimeFormatString ----
            selectInput(
              ns("dateTimeFormatString"),
              "Select a date format",
              choices = c(NA_character_)
            ),
            # * unit ----
            selectInput(
              ns("unit"),
              label = tagList(
                tags$b("Select an unit"),
                helpText("You can search a unit by typing its name.")
              ),
              choices = c(NA_character_)
            ),
            # * missingValueCode ----
            textInput(
              ns("missingValueCode"),
              "Code for missing value (1 word)"
            ),
            # * missingValueCodeExplanation ----
            textAreaInput(
              ns("missingValueCodeExplanation"),
              "Explain Missing Values",
              width = "100%"
            )
          ) # end div
        ) # end hidden
      ), # end column
      # Preview ----
      column(
        2, 
        tags$div(
          id = "preview",
          tags$b("Data Preview"),
          tableOutput(ns("preview"))
        )
      )
    ), # end fluidRow
    # Custom Units ----
    shinyjs::hidden(
      tags$div(
        id = "custom_units",
        tags$hr(),
        tags$h4("Custom units"),
        fluidRow(
          tableOutput(ns("CUUI"))
        )
      )
    )
  )
}

#' @importFrom shinyTree renderTree
Attributes <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    if (main.env$dev){
      observeEvent(
        main.env$dev.browse(), 
        {
          if (main.env$current.tab() == "fill" &&
              main.env$EAL$page == 3) {
            browser()
          }
        }
      )
    }
    
    # Tree ====  
    
    # * Make tree ----
    # Compute tree
    # treeContent <- eventReactive({
    #   main.env$EAL$page
    #   main.env$local.rv$md.tables
    # }, {
    #   
    # })
    
    # Render computed tree
    # initialize empty tree
    output$tree <- shinyTree::renderEmptyTree()
    outputOptions(output, "tree", suspendWhenHidden = FALSE)
    
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page == 3) 
      req(isContentTruthy(main.env$local.rv$tree.content))
      devmsg("update tree", tag = "attributes")
      shinyTree::updateTree(
        session = session, 
        treeId = "tree",
        data = main.env$local.rv$tree.content
      )
    }, priority = -1)
    
    # * Get input from tree ----
    # shinyTree selection
    .selected <- reactive({
      devmsg("selected", tag="attributes")
      if(isContentTruthy(input$tree)){
        get_selected(input$tree)
      } else {
        return(NULL)
      }
    })
    
    # shinyTree path exploration
    .ancestor <- reactive({
      devmsg("ancestor", tag="attributes")
      if(isContentTruthy(.selected())){
        attr(.selected()[[1]], "ancestry")
      } else {
        return(NULL)
      }
    })
    
    # get selected file from shinyTree
    selected.file <- reactive({
      req(isContentTruthy(.selected()) && 
            isContentTruthy(.ancestor()))
      
      devmsg("selected file", tag = "attributes")
      
      if(length(.ancestor()) == 0)
        return(.selected()[[1]][1])
      else
        return(.ancestor())
    })
    
    # Show selected filename
    output$filename <- renderText(selected.file())
    
    # Set shortcut reactive for table
    selected.table <- reactive({
      main.env$local.rv$md.tables[[selected.file()]]
    })
    
    # get selected attribute from shinyTree
    selected.attribute <- reactive({
      req(isContentTruthy(.selected()))
      if(length(.ancestor()) == 0)
        return(NULL)
      else
        return(.selected()[[1]][1])
    })
    
    # Show selected attributeName
    output$attributeName <- renderText({
      req(selected.attribute())
      selected.attribute()
    })
    
    # * Change attribute ----
    observeEvent({
      selected.attribute()
    }, {
      validate(
        need(isTruthy(selected.file()), "Not a valid file"),
        need(!is.null(selected.attribute()), "Not a valid attribute")
      )
      
      # Update values
      .row <- main.env$local.rv$md.tables[[selected.file()]] %>%
        filter(attributeName == selected.attribute())
      # Set attributeDefinition
      updateTextAreaInput(
        session,
        "attributeDefinition",
        value = .row$attributeDefinition
      )
      # Set class
      updateSelectInput(
        session,
        "class", 
        selected = .row$class
      )
      # Set dateTimeFormatString
      updateSelectInput(
        session,
        "dateTimeFormatString",
        choices = main.env$FORMATS$dates,
        selected = .row$dateTimeFormatString
      )
      # Set unit
      .tmp <- setUnitList(
        main.env, 
        set = if(.row$class == "numeric") .row$unit
      )
      updateSelectInput(
        session,
        "unit",
        choices = .tmp$unit.list,
        selected = .tmp$set.unit
      )
      # Set missingValueCode
      updateTextInput(
        session,
        "missingValueCode",
        value = .row$missingValueCode
      )
      # Set missingValueCodeExplanation
      updateTextAreaInput(
        session,
        "missingValueCodeExplanation",
        value = .row$missingValueCodeExplanation
      )
      
      # ** Check validity ----
      # at least check one attribute
      main.env$local.rv$checked <- TRUE
      
      # ShinyFeedBack
      checkFeedback(input, "attributeDefinition", type = "danger")
      checkFeedback(input, "class", type = "danger")
      checkFeedback(
        input, "dateTimeFormatString", 
        condition = if(input$class == "Date")
          isTruthy(input$dateTimeFormatString) && 
          input$dateTimeFormatString %in% main.env$FORMATS$dates
        else
          TRUE,
        type = "danger"
      )
      checkFeedback(
        input, "unit", 
        condition = if(input$class == "numeric")
          isTruthy(input$unit) && 
          input$unit %grep% main.env$FORMATS$units
        else
          TRUE,
        type = "danger"
      )
      checkFeedback(input, "missingValueCode", type = "warning")
      checkFeedback(input, "missingValueCodeExplanation", type = "warning")
    }, 
    ignoreNULL = FALSE, 
    ignoreInit = FALSE)
    
    # Form ====
    
    # toggle form 
    observe({
      shinyjs::toggle(selector = "#no_attribute", condition = is.null(selected.attribute()))
      shinyjs::toggle(selector = "#form", condition = !is.null(selected.attribute()))
      shinyjs::toggle(
        selector = "#custom_units",
        condition = nrow(main.env$local.rv$custom.units$table) > 0
      )
    })
    
    # * attributeDefinition ----
    observeEvent(input$attributeDefinition, {
      validate(
        need(isTruthy(selected.file()), "No file selected"),
        need(isTruthy(selected.attribute()), "No attribute selected")
      )
      
      # Get value
      .row <- which(
        main.env$local.rv$
          md.tables[[selected.file()]]$
          attributeName == selected.attribute()
      )
      
      main.env$local.rv$
        md.tables[[selected.file()]]$
        attributeDefinition[.row] <<- input$attributeDefinition
      
      # Check validity
      checkFeedback(input, "attributeDefinition", type = "danger")
    })
    
    # * class ----
    observeEvent({
      input$class
      selected.attribute()
    }, {
      validate(
        need(isTruthy(selected.file()), "No file selected"),
        need(isTruthy(selected.attribute()), "No attribute selected"),
        need(isTruthy(input$class), "Invalid value for class"),
        need(
          input$class %in% c("character", "categorical", "numeric", "Date"), 
          "Value not found for class"
        )
      )
      
      main.env$local.rv$md.tables[[selected.file()]] <<- replaceValue(
        main.env$local.rv$md.tables[[selected.file()]],
        selected.attribute(),
        "class",
        input$class
      )
      
      # Hide/show units and dateTFS
      shinyjs::toggle(
        "dateTimeFormatString",
        condition = input$class == "Date"
      )
      if(input$class != "Date") {
        updateSelectInput(
          session,
          "dateTimeFormatString",
          selected = ""
        )
      } else {
        updateSelectInput(
          session,
          "dateTimeFormatString",
          selected = main.env$FORMATS$dates[1] # YYYY-MM-DD
        )
      }
      
      shinyjs::toggle(
        "unit",
        condition = input$class == "numeric"
      )
      if(input$class != "numeric") {
        updateSelectInput(
          session,
          "unit",
          selected = ""
        )
      } else {
        .unit <- selected.table()[
          which(selected.table()$attributeName == selected.attribute()),
          "unit"
        ]
        .tmp <- setUnitList(
          main.env, 
          set = optional(.unit, "dimensionless")
        )
        updateSelectInput(
          session,
          "unit",
          choices = .tmp$unit.list,
          selected = .tmp$set.unit
        )
      }
      
      # Check validity
      checkFeedback(input, "class", type = "danger")
    })
    
    # * dateTimeFormatString ----
    observeEvent(input$dateTimeFormatString, {
      validate(
        need(isTruthy(selected.file()), "No file selected"),
        need(isTruthy(selected.attribute()), "No attribute selected"),
        need(input$class == "Date", "Not a Date"),
        need(!is.na(input$dateTimeFormatString), "Unset dateTimeFormatString input.")
      )
      
      if(main.env$dev)
        devmsg(input$dateTimeFormatString)
      
      # Correct input value
      .value <- input$dateTimeFormatString
      if(isFALSE(.value %in% main.env$FORMATS$dates)) {
        .value <- main.env$FORMATS$dates[1] # YYYY-MM-DD
        updateSelectInput(
          session,
          "dateTimeFormatString",
          selected = .value
        )
      }
      
      main.env$local.rv$md.tables[[selected.file()]] <<- replaceValue(
        main.env$local.rv$md.tables[[selected.file()]],
        selected.attribute(),
        "dateTimeFormatString",
        .value
      )
      
      # Check validity
      checkFeedback(
        input, 
        "dateTimeFormatString", 
        condition = if(input$class == "Date")
          isTruthy(input$dateTimeFormatString) && 
          .value %in% main.env$FORMATS$dates
        else
          TRUE,
        type = "danger"
      )
    })
    
    # * unit ----
    unit.value <- reactive({
      validate(
        need(isTruthy(selected.file()), "No file selected"),
        need(isTruthy(selected.attribute()), "No attribute selected"),
        need(input$class == "numeric", "Not a number"),
        need(!is.na(input$unit), "Unset unit input.")
      )
      input$unit
    }) %>%
      debounce(1000)
    
    observe({
      req(unit.value())
      
      # Correct input value
      .value <- unit.value()
      if(isFALSE(.value %in% c(
        main.env$FORMATS$units,
        "custom",
        main.env$local.rv$custom.units$table$unit.id
      ))) {
        .value <- main.env$FORMATS$units[2] # dimensionless
        # updateSelectInput(
        #   session,
        #   "unit",
        #   selected = .value
        # )
        showNotification(
          id = "unit_404",
          "Queried unit not found: check spelling."
        )
      }
      
      # Standard unit
      if(.value != "custom") {
        # Save value
        main.env$local.rv$md.tables[[selected.file()]] <<- replaceValue(
          main.env$local.rv$md.tables[[selected.file()]],
          selected.attribute(),
          "unit",
          .value
        )
      } else { # Custom unit
        # Check if currently worked unit is a custom one
        saved <- main.env$local.rv$md.tables[[selected.file()]] %>%
          filter(attributeName == selected.attribute()) %>%
          select(unit) %>%
          unlist()
        # Set default value for input module
        if(isTRUE(saved %in% main.env$local.rv$custom.units$table$unit.id)) {
          .values <- main.env$local.rv$custom.units$table %>%
            dplyr::filter(id == saved) %>%
            unlist(use.names = T)
        } else { # New custom unit - empty UI
          .values <- rep(NA, 5) %>%
            setNames(nm = names(main.env$local.rv$custom.units$table))
        }
        
        # Properly show modal
        showModal(
          customUnitsUI(
            session$ns("customUnits"),
            values = .values,
            main.env = main.env
          )
        )
      }
      
      # Check validity
      .condition <- if(input$class == "numeric") {
        isTruthy(.value) && 
          .value != "custom" &&
          (.value %grep% main.env$FORMATS$units ||
             .value %in% main.env$local.rv$custom.units$table$id)
      } else TRUE
      if(main.env$dev)
        devmsg("%s", as.character(.condition))
      checkFeedback(
        input, 
        "unit",
        condition = .condition,
        type = "danger"
      )
    })
    
    # ** Custom Units ----
    customUnits("customUnits", main.env)
    
    observeEvent({
      if(main.env$EAL$page == 3 &&
         "cancel" %in% names(main.env$local.rv$custom.units)){
        main.env$local.rv$custom.units$cancel()
      }
    }, {
      # Set unit
      .tmp <- setUnitList(
        main.env, 
        set = "dimensionless"
      )
      updateSelectInput(
        session,
        "unit",
        choices = .tmp$unit.list,
        selected = .tmp$set.unit
      )
    }, priority = -1)
    
    observeEvent({
      if(main.env$EAL$page == 3 &&
         "reactive" %in% names(main.env$local.rv$custom.units)){
        main.env$local.rv$custom.units$reactive()
      }
    }, {
      req(main.env$EAL$page == 3)
      req(input$unit == "custom")
      
      .unit <- main.env$local.rv$custom.units$table$id
      main.env$local.rv$md.tables[[selected.file()]] <<- replaceValue(
        main.env$local.rv$md.tables[[selected.file()]],
        selected.attribute(),
        "unit",
        .unit
      )
      .tmp <- setUnitList(
        main.env, 
        set = if(input$class == "numeric") .unit
      )
      updateSelectInput(
        session,
        "unit",
        choices = .tmp$unit.list,
        selected = .tmp$set.unit
      )
    }, priority = -1)
    
    output$CUUI <- renderTable({
      main.env$local.rv$custom.units$reactive()
    })
    
    # Check if a custom unit has been removed
    observe({
      # Trigger observe
      .row <- main.env$local.rv$md.tables[[selected.file()]] %>% 
        filter(attributeName == selected.attribute())
      # Get list of CU id
      .cuids <- main.env$local.rv$custom.units$table$id
      sapply(.cuids, function(.cuid){
        .found <- any(sapply(
          listReactiveValues(main.env$local.rv$md.tables),
          function(table){
            .cuid %grep% table$unit
          }
        ))
        if(!.found){
          .row.index <- which(.cuids == .cuid)
          main.env$local.rv$custom.units$table <<-
            main.env$local.rv$custom.units$table[-.row.index,]
        }
      })
    }, priority = -1)
    
    # * missingValueCode ----
    observeEvent(input$missingValueCode, {
      validate(
        need(isTruthy(selected.file()), "No file selected"),
        need(isTruthy(selected.attribute()), "No attribute selected")
      )
      
      .value <- input$missingValueCode
      
      # Limit to 1 word
      if (grepl(".+ +.*", .value)) {
        # Shorten
        .value <- strsplit(
          gsub("^ +", "", .value),
          split = " "
        )[[1]][1]
        # Update input
        updateTextInput(
          session,
          "missingValueCode",
          value = .value
        )
        # Send notification to user
        showNotification(
          id = "mvc_update",
          ui = tagList(
            tags$code("missingValueCode"),
            "fields are limited to a",
            tags$b("single word.")
          ),
          duration = 3,
          type = "warning"
        )
      }
      
      main.env$local.rv$md.tables[[selected.file()]] <<- replaceValue(
        main.env$local.rv$md.tables[[selected.file()]],
        selected.attribute(),
        "missingValueCode",
        .value
      )
      
      # Check validity
      checkFeedback(
        input,
        "missingValueCode",
        type = if(identical(
          isContentTruthy(.value) || .value == "", 
          isContentTruthy(input$missingValueCodeExplanation)
        )) "warning" else "danger"
      )
      
      if(isContentTruthy(input$missingValueCode))
        checkFeedback(input, "missingValueCodeExplanation", type = "danger") else
          checkFeedback(input, "missingValueCodeExplanation", type = "warning")
      
    })
    
    # * missingValueCodeExplanation ----
    observeEvent(input$missingValueCodeExplanation, {
      validate(
        need(isTruthy(selected.file()), "No file selected"),
        need(isTruthy(selected.attribute()), "No attribute selected")
      )
      
      .value <- input$missingValueCodeExplanation
      main.env$local.rv$md.tables[[selected.file()]] <<- replaceValue(
        main.env$local.rv$md.tables[[selected.file()]],
        selected.attribute(),
        "missingValueCodeExplanation",
        .value
      )
      
      # Check validity
      checkFeedback(
        input,
        "missingValueCodeExplanation",
        type = if(identical(
          isContentTruthy(.value),
          isContentTruthy(input$missingValueCode) || input$missingValueCode == ""
        )) "warning" else "danger"
      )
      
      checkFeedback(
        input,
        "missingValueCode",
        type = if(identical(
          isContentTruthy(.value),
          isContentTruthy(input$missingValueCode) || input$missingValueCode == ""
        )) "warning" else "danger"
      )
    })
    
    # Preview ====
    output$preview <- renderTable({
      req(isContentTruthy(selected.file()))
      req(isContentTruthy(selected.attribute()))
      
      validate(
        need(
          isTruthy(main.env$local.rv$preview[[selected.file()]][selected.attribute()]),
          "empty column"
        )
      )
      
      main.env$local.rv$preview[[selected.file()]][[selected.attribute()]] %>%
        as.character %>%
        enc2utf8 %>%
        as.data.frame %>%
        unname
        # setNames(nm = "Data preview")
    })
    
    # Completeness ====
    observeEvent({
      input$attributeDefinition
      input$class
      input$dateTimeFormatString
      input$unit
      input$missingValueCode
      input$missingValueCodeExplanation
    }, {
      req(main.env$EAL$page == 3)
      # Upon any input change, check attribute's completeness
      main.env$local.rv$completed[[
        selected.file()
      ]][[
        selected.attribute()
      ]] <- (
        isContentTruthy(input$attributeDefinition) &&
          isContentTruthy(input$class) &&
          input$class %in% c("character", "numeric", "Date", "categorical") &&
          {
            if(input$class == "Date") {
              isContentTruthy(input$dateTimeFormatString) &&
                input$dateTimeFormatString %in% main.env$FORMATS$dates
            } else TRUE
          } &&
          {
            if(input$class == "numeric") {
              isContentTruthy(input$unit) &&
                input$unit != "custom" &&
                input$unit %in% c(
                  main.env$local.rv$custom.units$table$id,
                  main.env$FORMATS$units
                )
            } else TRUE
          } && 
          {
            if(input$missingValueCodeExplanation != "") {
              (isContentTruthy(input$missingValueCode) || input$missingValueCode == "") &&
                isContentTruthy(input$missingValueCodeExplanation)
            } else (input$missingValueCode == "")
          }
      )
      
      # Update whole completeness
      main.env$EAL$completed <- isTRUE(main.env$local.rv$checked) &&
        all(
          unlist(
            listReactiveValues(
              main.env$local.rv$completed
            )
          )
        )
      
      # If any problem, tell the user
      # TODO add lacking attributes completion
      if(!main.env$EAL$completed) {
        main.env$EAL$tag.list <- tagList()
        
      } else {
        # Else, allow next
        main.env$EAL$tag.list <- tagList()
        
      }
    },
    priority = -2)
    
    # (End of Attributes) ====
    
  })
}