# Plot life status correction result

Plot life status correction result

## Usage

``` r
StatusCorrectionPlot(Data, OnlyCorrected = TRUE, SeveralWindows = TRUE)
```

## Arguments

- Data:

  Dataset (data.frame or data.table) The dataset must contain the
  columns:

  - `IdTree` (character)

  - `Year` (numeric)

  - `LifeStatus` (logical or character)

  - `LifeStatus_DataHarmonizationCor` (logical or character)

- OnlyCorrected:

  TRUE: plot only corrected trees, FALSE: plot all trees (logical)

- SeveralWindows:

  TRUE: return each page in a new window (better visualisation in
  Rstudio), FALSE: return each page in the same window (needed to save
  all the pages) (logical)

## Value

The plots of the initial diameter values and proposed corrections, by
IdStem/IdTree

## Examples

``` r
if (FALSE) { # \dontrun{
data(TestData)
Rslt <- StatusCorrection(TestData)
StatusCorrectionPlot(Rslt, SeveralWindows = TRUE)
} # }
```
