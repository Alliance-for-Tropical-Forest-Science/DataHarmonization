test_that("RecruitmentCorrection", {

  # Import data
  suppressWarnings(library(data.table))
  data(TestData)

  # Create test data
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("Diameter")]
  NoDBHCorData <- TestData

  setnames(TestData, "Diameter", "Diameter_DataHarmonizationCor", skip_absent=TRUE)


  # Check the function argument
  expect_error(RecruitmentCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  expect_error(RecruitmentCorrection(TestData, OnlyDetectMissedRecruits = "no"),
               regexp = "The 'OnlyDetectMissedRecruits' argument
         of the 'RecruitmentCorrection' function must be logicals")

  expect_error(RecruitmentCorrection(NoDBHData),
               regexp = "column does't exist in the dataset.")



  expect_warning(  Rslt <-RecruitmentCorrection(TestData), regexp = "We added rows for trees that were supposed to be recruited earlier based on growth pattern and MinDBH")

  expect_equal(sum(Rslt$CorrectedRecruit), 77)

  # Check the function works
  Rslt <- RecruitmentCorrection(TestData, OnlyDetectMissedRecruits = T)
  expect_null(Rslt$CorrectedRecruit)

})
