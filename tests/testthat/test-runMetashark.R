library(MetaShARK)

test_that("is app", {
  app <- runMetashark(server = TRUE, test = TRUE)
  expect(
    is(app, "shiny.appobj"),
    message("Bad class, expected \"shiny.appobj\".")
  )
})


# Configure this test to fit your need
test_that("app launches", {
  # skip_on_cran()
  # skip_on_travis()
  # skip_on_appveyor()
  p <- processx::process$new(
    "R",
    c(
      "-e",
      "pkgload::load_all();MetaShARK::runmetashark()"
    ),
    cleanup_tree = TRUE
  )
  print(p)

  # Sys.sleep(5) # commented because launched from a function, not common shiny launch

  if (isFALSE(p$is_alive())) {
    stop("App not running")
  }

  p$kill()
  print(p)
})
