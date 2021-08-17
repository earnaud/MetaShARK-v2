app <- ShinyDriver$new("../../")
app$snapshotInit("test_metashark")

app$setInputs(sidemenu = "fill", allowInputNoBinding_ = TRUE)
app$setInputs(sidemenu = "upload", allowInputNoBinding_ = TRUE)
app$snapshot()
app$setInputs(sidemenu = "about", allowInputNoBinding_ = TRUE)
app$snapshot()
