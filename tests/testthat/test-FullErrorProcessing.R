test_that("FullErrorProcessing", {

  data(TestData)

  Rslt_Test <- suppressWarnings(FullErrorProcessing(TestData))
  # Rslt_Test <- FullErrorProcessing(TestData, Source = "WFO", WFOData = WFO_Backbone)
  # Rslt_Panama <- FullErrorProcessing(PanamaFormated)


  # Check general errors detection
  # Check botanical correction
  # Check the life status correction
  # Check taper correction
  # Check diameter correction
  # Check recruitment correction

  })
