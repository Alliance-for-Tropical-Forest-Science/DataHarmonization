# Plot diameter correction result

Plot diameter correction result

## Usage

``` r
DiameterCorrectionPlot(
  Data,
  OnlyCorrected = TRUE,
  CorCol = "Diameter_DataHarmonizationCor",
  SeveralWindows = TRUE
)
```

## Arguments

- Data:

  Dataset (data.frame or data.table) The dataset must contain the
  columns:

  - `IdStem` (character)

  - `IdCensus` (ordered factor)

  - `Diameter` (numeric)

  - `DBHCor` (numeric)

  - `HOM` (Height Of Measurement) (numeric)

  - `HOMCor` (Corrected Height Of Measurement) (numeric)

- OnlyCorrected:

  TRUE: plot only corrected stems, FALSE: plot all stems (logical)

- CorCol:

  Corrected column name (character)

- SeveralWindows:

  TRUE: return each page in a new window (better visualisation in
  Rstudio), FALSE: return each page in the same window (needed to save
  all the pages) (logical)

## Value

The plots of the initial diameter values and proposed corrections, by
IdStem.

## Examples

``` r
if (FALSE) { # \dontrun{
# pdf("DiameterCorrectionPlots_TestData.pdf", width = 25, height = 10)

 data(TestData)

TestData$HOM[1:3] <- c(0.5,1.5,NA)
TestData$Diameter[1:3] <- TestData$Diameter[1:3] + c(2,-2,0)
TestData$Diameter[21:23] <- c(31,91,14)

Rslt <- DiameterCorrection(
 TestData,
  WhatToCorrect = c("POM change","Abnormal growth"),
   CorrectionType = c("phylo"),
    MinIndividualNbr = 1)
DiameterCorrectionPlot(Rslt, OnlyCorrected = TRUE, SeveralWindows = FALSE)

# dev.off()
} # }
```
