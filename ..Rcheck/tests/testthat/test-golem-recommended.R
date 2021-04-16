context("golem tests")

library(golem)

test_that("app ui", {
  ui <- ui()
  expect_shinytaglist(ui)
})

test_that("app server", {
  server <- server
  expect_type(server, "closure")
})

# Configure this test to fit your need
test_that(
  "app launches",{
    # skip_on_cran()
    # skip_on_travis()
    # skip_on_appveyor()
    x <- processx::process$new(
      "R", 
      c(
        "-e", 
        "pkgload::load_all(here::here());runMetashark()"
      )
    )
    Sys.sleep(5)
    expect_true(x$is_alive())
    x$kill()
  }
)








