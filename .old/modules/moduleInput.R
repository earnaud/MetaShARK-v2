# This module shows the different possible inputs

### UI ELEMENTS ###

# Choice input
choiceInput <- function(id = "choice",
                        choices){
  # Function namespace
  ns = NS(id)
  
  # Final UI dusplay
  return(do.call(tabsetPanel, c(id = id,
                         lapply(1:length(choices),
                                function(i){
                                  tabPanel(
                                    title=names(choices)[i],
                                    paste(unlist(choices[[i]]),
                                          collapse = " ")
                                  )
                                })
                         )
          )
  )
}


### SERVER ELEMENTS ###
choiceServer <- function(input, 
                         output, 
                         session){
  output <- reactive({
    return(input)
  })
}
