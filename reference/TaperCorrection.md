# TaperCorrection

Transform the tree diameter measured at a given height into the diameter
corresponding to the default measurement height (HOM), using an
allometry.

## Usage

``` r
TaperCorrection(
  Data,
  DefaultHOM = 1.3,
  TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  TaperFormula = function(DAB, HOM, TaperParameter, DefaultHOM) DAB/(exp(-TaperParameter
    * (HOM - DefaultHOM)))
)
```

## Arguments

- Data:

  Dataset (data.frame or data.table) The dataset must contain the
  columns:

  - `Diameter` (numeric)

  - **`HOM` (Height Of Measurement) (numeric)**

- DefaultHOM:

  Default Height Of Measurement in meter (Default: 1.3 m) (numeric, 1
  value)

- TaperParameter:

  Taper parameter (unitless) formula (function) Default: *TaperParameter
  = 0.156 - 0.023 log(DAB) - 0.021 log(HOM)* of Cushman et al.2021.
  With:

  - *DAB*: Diameter Above Buttress (in cm)

  - *HOM*: Height Of Measurement (in m)

- TaperFormula:

  Taper formula (function) Default: *DAB / (e^(- TaperParameter (HOM -
  DefaultHOM)))* of Cushman et al.2021. With:

  - *DAB*: Diameter Above Buttress (in cm)

  - *HOM*: Height Of Measurement (in m)

  - *DefaultHOM*: Default Height Of Measurement (in m)

  - *TaperParameter*: Taper parameter (unitless)

## Value

Fill the *Comment_DataHarmonization* column with error type informations
and add columns: - *Diameter_DataHarmonizationCor*: corrected trees
diameter at default HOM - *DiameterCorrectionMeth* = "taper" -
*HOM_DataHarmonizationCor* (numeric): HOM corresponding to the
*Diameter_DataHarmonizationCor* (= *DefaultHOM*)

## Examples

``` r
library(data.table)

Data <- data.table(IdStem = "A",
      ScientificName = "Tree",
      Year = c(1998, 2008, 2016, 2017, 2018, 2019, 2021),
      IdCensus = factor(c(1998, 2008, 2016, 2017, 2018, 2019, 2021), ordered = TRUE),
      Diameter = c(19, 19, 21.4, 22.6, 23.1, 23.1, 23.6),
      HOM = c(1.30, 3.25, 3.25, 3.25, 3.25, 3.25, 3.25))

Rslt <- TaperCorrection(Data)
DiameterCorrectionPlot(Rslt, CorCol = "Diameter_DataHarmonizationCor")
```
