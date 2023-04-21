test_that("DiameterCorrection", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  suppressWarnings(library(data.table))
  data(TestData)

  # Remove other errors types (non-unique idTree, missing Year)
  TestData <- TestData[!IdTree %in% c("100898", "101686")]

  # Create test data ----------------------------------------------------------------------------------------------------------------
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("Diameter")]
  HOMData <- copy(TestData[IdTree == "100658"])
  HOMData[, HOM := 1.3] # data with HOM
  POMData <- copy(TestData[IdTree == "100658"])
  POMData[, POM := as.factor(1)] # data with POM
  POMData[, HOM := NA]


  # Check the function argument -----------------------------------------------------------------------------------------------------

  expect_error(DiameterCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  expect_error(DiameterCorrection(NoDBHData),
               regexp = "column does't exist in the dataset")

  expect_error(DiameterCorrection(TestData, DefaultHOM = "a",
                                  # MaxDBH = c(1,2),
                                  PositiveGrowthThreshold = c("a", "b"),
                                  NegativeGrowthThreshold = "-2",
                                  PioneersGrowthThreshold = F,
                                  DBHRange = "10",
                                  MinIndividualNbr = "5"),
               regexp = "The 'PositiveGrowthThreshold', 'NegativeGrowthThreshold', 'PioneersGrowthThreshold' and 'DefaultHOM' arguments
         of the 'DiameterCorrection' function must be 1 numeric value each")

  expect_error(DiameterCorrection(TestData, Pioneers = T),
               regexp = "'Pioneers' argument must be a characters vector, or NULL")

  expect_error(DiameterCorrection(TestData, WhatToCorrect = "diameter"),
               regexp = "'arg' should be one of \"POM change\", \"Abnormal growth\"")

  expect_error(DiameterCorrection(TestData, CorrectionType = "best"),
               regexp = "'arg' should be one of \"individual\", \"phylogenetic hierarchical\"")

  expect_message(DiameterCorrection(POMData, CorrectionType = "individual", WhatToCorrect = "Abnormal growth", UseTaperCorrection = F),
                 regexp = "You have the 'POM' information in your dataset")


  # Check the function work ---------------------------------------------------------------------------------------------------------


  ## Correction --------------------------------------------------------------------------------------------------------------------
  # options(warn = 2) # trace warning
  # options(warn = 0) # when debug is over

  expect_warning(Rslt <- DiameterCorrection(
    TestData,

    UseTaperCorrection = F,

    PositiveGrowthThreshold = 5,
    NegativeGrowthThreshold = -2,

    Pioneers = c("Cecropia","Pourouma"),
    PioneersGrowthThreshold = 7.5,

    WhatToCorrect = c("POM change", "Abnormal growth"),

    CorrectionType = c("phylogenetic hierarchical"),

    DBHRange = 10,
    MinIndividualNbr = 1),
    "We added rows for trees that were supposed to be recruited earlier based on linear extrapolation of growth and MinDBH
We added rows for trees that were missed and estimated their missed DBH by linear interpolation")

  # Growth > 5 cm DBH/year & < -2 cm DBH/census

  # Comment and Methode if correction
  expect_true(all(!is.na(Rslt[Diameter_DataHarmonizationCor != Diameter, DiameterCorrectionMeth_DataHarmonization]))) # method when the DBH has been corrected

  # expect_true(all(Rslt[Diameter_DataHarmonizationCor != round(Diameter), Comment] != "")) # comment when the DBH has been corrected



})

