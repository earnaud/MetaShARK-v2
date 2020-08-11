#' @import shiny
#'
#' @noRd
fillUI <- function(id, main.env) {
  ns <- NS(id)
  
  tabsetPanel(
    id = NS(id, "tabs"),
    tabPanel(
      "EAL",
      # EMLALUI(NS(id, "EAL"), main.env)
      fluidPage(
        style = "padding-top:2.5%;",
        # * Top row ----
        tags$span(
          fluidRow(
            div(
              h4("EML Assembly Line"),
              style = "float: left"
            ),
            div(
              actionButton(
                NS(id, "help"), 
                "Help", 
                icon("question-circle")
              ),
              actionButton(
                NS(id, "save"),
                "Save",
                icon("save")
              ), # fill-wizard-save
              actionButton(
                NS(id, "quit"), 
                "Quit",
                icon("times-circle"),
                class = "danger"
              ), # fill-wizard-quit
              style = "float: right;"
            ), # fill-wizard-help
            style = "width: 100%"
          ),
          # uiOutput(NS(id, "chain")),
          style = "display: inline-flex; width: 100%"
        ),
        # * Pages ----
        pagesUI(
          NS(id, "wizard"),
          parent.id = id,
          main.env = main.env
        )
      ) # end fluidPage
    ),
    tabPanel(
      "MetaFIN",
      tags$h1("Under Construction"),
      tags$img(src = "media/working.png")
    )
  )
}

#' @import shiny
#'
#' @noRd
fill <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    if (main.env$dev) {
      shinyjs::onclick(
        "dev",
        {
          if (main.env$current.tab() == "fill") {
            browser()
          }
        },
        asis = TRUE
      )
    }
    
    # Variable initialization ====
    steps <- isolate(main.env$VALUES$steps)
    
    # Wizard  ====
    # pages change
    pagesServer(NS(id, "wizard"), main.env)
    
    # modules content
    SelectDP("SelectDP", full.id = NS(id, "SelectDP"), main.env)
    DataFiles("DataFiles", full.id = NS(id, "DataFiles"), main.env)
    Attributes("Attributes", full.id = NS(id, "Attributes"), main.env)
    CatVars("CatVars", full.id = NS(id, "CatVars"), main.env)
    GeoCov("GeoCov", full.id = NS(id, "GeoCov"), main.env)
    TaxCov("TaxCov", full.id = NS(id, "TaxCov"), main.env)
    Personnel("Personnel", full.id = NS(id, "Personnel"), main.env)
    Misc("Misc", full.id = NS(id, "Misc"), main.env)
    MakeEML("MakeEML", full.id = NS(id, "MakeEML"), main.env)
    
    # * Wizard components server ----
    # * Quit ----
    {
      # show modal on 'quit' button clicked
      observeEvent(input$quit, {
        req(input$quit)
        showModal(
          modalDialog(
            title = "You are leaving data description.",
            "Are you sure to leave? Some of your metadata have maybe not been saved.",
            easyClose = FALSE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                NS(id, "save_quit"),
                "Save & Quit"
              ),
              actionButton(
                NS(id, "simple_quit"),
                "Quit",
                icon("times-circle"),
                class = "redButton"
              )
            )
          ) # end of modalDialog
        )
      },
      label = "EAL quit?"
      )
      
      # calls saveRDS method and quits
      observeEvent(input$save_quit, {
        req(input$save_quit)
        
        # Save work at this state
        saveReactive(main.env$save.variable)
      },
      label = "EAL save+quit",
      ignoreInit = TRUE
      )
      
      # quits simply
      observeEvent(input$simple_quit, {
        req(input$quit)
        
        removeModal()
        
        # Clean & reset variables
        main.env$EAL$history <- "SelectDP"
        main.env$EAL$page <- 1
      },
      label = "EAL quit",
      ignoreInit = TRUE
      )
    } # quit management
    
    # * Save ----
    observeEvent(input$save, {saveReactive(main.env)})
    
    # Navigation ====
    observeEvent(main.env$EAL$page, {
      # * set EAL variables ----
      # reset state
      if (main.env$EAL$current == "Data Files") {
        unlink(main.env$PATHS$eal.tmp)
      }
      
      # Properly change page
      main.env$EAL$current <- main.env$VALUES$steps[main.env$EAL$page]
      
      if (main.env$EAL$current == "Data Files") {
        main.env$PATHS$eal.tmp <- tempdir()
      }
      
      # if (isFALSE(main.env$EAL$completed)) {
      #   main.env$EAL$completed <- TRUE
      # } # trigger
      main.env$EAL$completed <- FALSE
      main.env$EAL$tag.list <- tagList()
      message("fill")
      main.env$local.rv <- setLocalRV(main.env)
      
      # Edition changed path -> remove excedent history
      if (!main.env$EAL$current %in% main.env$EAL$history) {
        main.env$EAL$history <- c(main.env$EAL$history, main.env$EAL$current)
      }
      
      # Savevar modification
      main.env$save.variable$step <- main.env$EAL$page
      main.env$save.variable$history <- main.env$EAL$history
      
      if(main.env$EAL$page > 1) {
        shinyjs::show("help")
        shinyjs::show("save")
        shinyjs::show("quit")
      } else {
        shinyjs::hide("help")
        shinyjs::hide("save")
        shinyjs::hide("quit")
      }
      updateTabsetPanel(session, "wizard-wizard", selected = steps[main.env$EAL$page])
      
      # * Helps ====
      {
        main.env$EAL$help <- modalDialog(
          title = paste0(main.env$EAL$current, " - Help"),
          switch(main.env$EAL$page,
                 # ** SelectDP ====
                 tagList(
                   tags$p("This module allows you to manage your", tags$strong("data packages"), ".
                    A data package (aka DP) is a collection of a dataset and its associated metadata
                    and resources (scripts, literature, ...). You can:"),
                   tags$ul(
                     tags$li("Load an existing data package among local ones (left). You
                      will resume its edition at the last saved encountered step."),
                     tags$li(
                       "Create a new data package. You must fill in", tags$strong("three fields"),
                       tags$i("Data package name"), "will be used for identifying the DP,",
                       tags$i("Data package title"), "will be displayed when consulting the DP
                        page once formatted (explained in last step),",
                       tags$i("Data package license"), "is the license assigned to this production for
                        intellectual rights properties"
                     )
                   ),
                   tags$p("Also, notice the", tags$strong("quick"), "check box above DP name.
                    Checking this box enables \"quick mode\" which will pre-fill most of the 
                    fields in further steps. You still will be able to edit them at your 
                    convenience.")
                 ),
                 # ** Data Files ====
                 tagList(
                   tags$p("This module allows you to load data files from the dataset you want to
            describe. Once uploaded, you can set:"),
                   tags$ul(
                     tags$li(tags$i("Content name:"), "A name for the data contained in the file (e.g. table name)."),
                     tags$li(tags$i("URL:"), "If the file is accessible remotely, here is a way to reference it."),
                     tags$li(tags$i("Description:"), "A short text provided to describe the file, its content,
              relevant information about the data in the entity.")
                   ),
                   tags$p("To edit your selection, select files among the list with the check boxes on
            their left, then click the \"Remove\" button."),
                   tags$p("Recommended size per file is around 1 Gb. Such files and heavier ones might slow down the
            app.")
                 ),
                 # ** Attributes  ====
                 tagList(
                   tags$p("This module allows you to describe precisely each attribute of each file. Some of these metadata
            are guessed from the data files. Such fields are annoted with a star (*).
            For each attribute, you can set:"),
                   tags$ul(
                     tags$li(tags$i("Attribute Name*:"), "the name of the attribute."),
                     tags$li(tags$i("Attribute Description:"), "a "),
                     tags$li(tags$i("Attribute Class*:"), "the type of content in the attributes among
              \"numeric\", \"character\", \"categorical\" and \"Date\". Categorical means a 
              character string with encoded values (e.g. Male/Female)."),
                     tags$li(tags$i("Date format (only for Date class): how the dates of the attributes are
              written (e.g. DD-MM-YYYY).")),
                     tags$li(tags$i("Unit (only for Numeric class):"), "which is the unit used for the 
              numeric data of the attributes. The list is huge and refers to", tags$a("STMML Scientific
              units", href = "http://www.ch.ic.ac.uk/rzepa/codata2/"), ". You can also define you own unit
              (see Custom Units thereafter)."),
                     tags$li(tags$i("Missing Value Code:"), "a one-word code used instead of a missing value among 
              the data of the attribute currently described."),
                     tags$li(tags$i("Missing Value Code Explanation:"), "a short definition of the meaning(s) given
              for the missing value code.")
                   ),
                   tags$h3("Custom units creation"),
                   tags$p("EML allows the user to define its own units, but this require to fulfill some more
            fields. However, the custom units you will have defined will be saved into the Custom Units
            table at the bottom of this page. You will find the written custom units in the units selection
            once they are written. To define a custom unit, chose the unit to be \"custom\". A custom 
            unit is defined with:"),
                   tags$ul(
                     tags$li(tags$i("Unit id:"), "the id of the unit (e.g. gramsPerOneThirdMeter). The unit id 
              must validate the STMML schema."),
                     tags$li(tags$i("Unit type:"), "the physical property measured by the custom unit (e.g. mass)."),
                     tags$li(tags$i("Parent unit in SI:"), "from which unit among the most common one is the custom 
              unit derived (e.g. gram)."),
                     tags$li(tags$i("Multiplier to SI:"), "by how many has the custom unit to be multiplied to be 
              equal to its parent unit."),
                     tags$li(tags$i("Unit description:"), "some additional notes about the unit, how to compute it.")
                   )
                 ),
                 # ** Catvars ====
                 tagList(
                   tags$p("This module allows you to detail the categorical variables (class \"categorical\" in Attributes).
            For each variable, you will be able to detail each of its value by a short description.")
                 ),
                 # ** Geocov ====
                 tagList(
                   tags$p("This module allows you to define the geographic area in which the data have been produced. 
            You have the choice between two methods to define geographic coverage:"),
                   tags$ul(
                     tags$li(
                       tags$h4("Columns description (recommended)"),
                       tags$p("This method asks you to choose columns in one of your files. For latitude and longitude,
                you can select either one or two columns. If a single column contains a pair of numbers, they
                will be detected. Chosing single coordinates will be considered as single sites. Chosing pairs of
                coordinates will be considered as sub-areas. For each coordinate, you can select a column 
                containing description for each one.")
                     ),
                     tags$li(
                       tags$h4("Custom description"),
                       tags$p("With this, you will be able to define by hand each one of the sites covered by your data.")
                     )
                   )
                 ),
                 # ** Taxcov ====
                 tagList(
                   tags$p("This module allows you to define the taxonomical coverage of the study. You will be asked to 
            select columns among your files containing the species name. Also, let the app know if the taxonomic
            coverage shall be written with scientific, common or both names. At last, select at least one taxonomic
            authority among the ones suggested."),
                 ),
                 # ** Personnel ====
                 tagList(
                   tags$p("This module allows you to get a full list of the people who contributed to the creation of 
            this dataset. The recommended best practice is to", tags$b("use the ORCID"), "of a person. With 
            this and the help of {rorcid}, the app will be able to fetch all available and interesting data. 
            You will be able to correct this afterwards. Also, note that you must fill in the fields
            for two roles: creator and contact."),
                   tags$p("Suggested roles are the following:"),
                   tags$ul(
                     tags$li("Creator: person who contributed to produce the data."),
                     tags$li("Contact: persone to contact for any question about the data."),
                     tags$li("Principal investigator: person who led the creation of the dataset. Selecting this will allow
              you to fill in additional information about the project and its funding."),
                     tags$li("Custom: as the list of roles is not exhaustive, feel free to add any role you consider important.")
                   )
                 ),
                 # ** Misc ====
                 tagList(
                   tags$p("This module allows you to define the last details of your data package. Note that you can write
            some of these metadata using the markdown syntax. Here are brief descriptions of the fields:"),
                   tags$ul(
                     tags$li("Abstract: the abstract of the publication linked to those data."),
                     tags$li("Methods: the methods used in the production of this dataset."),
                     tags$li(
                       "Keywords: you can type a list of keywords for your dataset. For each keyword, you can add a
              keyword thesaurus, like", tags$a("the LTER controlled vocabulary ", href = "http://vocab.lternet.edu/vocab/vocab/index.php"),
                       ", which are controlled vocabulary your exact keyword originates from. Keywords thesaurus are not
              required."
                     ),
                     tags$li("Temporal coverage: this lets you define the duration of the study during which the data have been produced."),
                     tags$li("Additional information: if any information has been omitted, you can provide it here (e.g. collection metadata
              from GBIF-EML).")
                   )
                 ),
                 # ** Make EML ====
                 tagList(
                   tags$p("Here we are (well done) ! This is the final step to write EML. Just click the button and let the magic happen. If an 
            error occurs, this will be displayed to the screen. In this case, do not hesitate to get in touch with the dev team."),
                   tags$p("You can also use the {emldown} package to get a human-readable version of the generated EML. The button below it will
            let you download the result of this step.")
                 )
          ),
          footer = modalButton("Close"),
          easyClose = TRUE, fade = TRUE
        )
      }
    },
    label = "EAL0 update"
    )
    
    observeEvent(input$help, {
      showModal(main.env$EAL$help)
    })
    # End ====
  })
}
