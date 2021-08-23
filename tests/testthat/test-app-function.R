library(shinytest)

test_that("runMetaShark() works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp(testthat::test_path("app"), compareImages = FALSE))
})