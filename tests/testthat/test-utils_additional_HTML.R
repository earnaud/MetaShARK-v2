library(shiny)

sample_tag <- tags$p("I am a sample paragraph tag", tags$code("with code"), ".")

# centered

expect_identical(
  MetaShARK::centered(sample_tag),
  tags$div(sample_tag, style = "text-align: center")
)

# withRedStar

expect_identical(
  MetaShARK::withRedStar(sample_tag),
  tags$span(
    HTML(
      paste0(
        sample_tag,
        tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
)

# unns 

expect_equal(
  unns("ns-test"),
  "test"
)
