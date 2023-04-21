test_that("TaperCorrection", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
                         Diameter = c(13:16, 16-4, NA, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = c(0, 0, 0, 0, 1, 1, 1, 2, 2),
                         HOM = c(1.3, NA, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))

  NoHOM <- copy(DataTree)
  NoHOM[, HOM := NULL] # remove HOM column



  # Check the function argument -----------------------------------------------------------------------------------------------------
  expect_error(TaperCorrection(NoHOM),
               regexp = "You have chosen to make a 'taper' correction,
       but you do not have the necessary 'HOM' column in your dataset")


  expect_error(TaperCorrection(DataTree,
                               DefaultHOM = "Diameter"),
               regexp = "The 'DefaultHOM' argument must be numeric")


  expect_error(TaperCorrection(DataTree,
                               TaperParameter = "0.156 - 0.023 * log(DAB) - 0.021 * log(HOM)",
                               TaperFormula = 2*c(3,8,9)),
               regexp = "The 'TaperParameter' and 'TaperFormula' arguments must be functions")


  # Check the function work ---------------------------------------------------------------------------------------------------------


  ## Correction ---------------------------------------------------------------------------------------------------------------------
  Rslt <- TaperCorrection(DataTree)

  expect_true(all(c("Diameter_DataHarmonizationCor", "DiameterCorrectionMeth_DataHarmonization", "Comment_DataHarmonization") %in% names(Rslt)))

  # Add a "Comment" and "Methode" value when "Diameter" != "Diameter_DataHarmonizationCor"
  Comment <- Rslt[, Comment_DataHarmonization] != ""
  Methode <- Rslt[, DiameterCorrectionMeth_DataHarmonization] != ""

  compareNA <- function(v1,v2) { # function to compare values, including NA
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }


  # If HOM different from the default HOM -> comment
  expect_true(all(na.omit((Rslt$HOM != 1.3) == Comment)))

  # If correction -> methode
  expect_true(all(!compareNA(Rslt$Diameter, Rslt$Diameter_DataHarmonizationCor) == Methode))

  # If initial value is NA, output value is NA too
  expect_true(all(is.na(Rslt$Diameter) == is.na(Rslt$Diameter_DataHarmonizationCor)))

  # Check the value of the "DiameterCorrectionMeth" column
  expect_true(all(Rslt$DiameterCorrectionMeth[Methode] == "taper"))



})
