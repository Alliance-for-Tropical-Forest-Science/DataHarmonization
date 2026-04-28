# Plot Botanical correction results

Plot Botanical correction results

## Usage

``` r
BotanicalCorrectionPlot(Data)
```

## Arguments

- Data:

  Dataset (data.frame or data.table) The dataset must contain the
  columns:

  - `IdTree` (character)

  - `ScientificName` (character)

  - `ScientificName_DataHarmonizationCor` (character)

## Value

Table of corrected species IDs

## Examples

``` r
if (FALSE) { # \dontrun{
data(TestData)
Rslt <- BotanicalCorrection(TestData)$Data
BotanicalCorrectionPlot(Rslt)
} # }
```
