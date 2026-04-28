# Diameter correction

Diameter correction

## Usage

``` r
DiameterCorrection(
  Data,
  UseTaperCorrection = TRUE,
  DefaultHOM = 1.3,
  TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  TaperFormula = function(DAB, HOM, TaperParameter, DefaultHOM) DAB/(exp(-TaperParameter
    * (HOM - DefaultHOM))),
  KeepMeas = c("MaxHOM", "MaxDate"),
  MinDBH = NULL,
  AddMissedRecruits = TRUE,
  AddMissedStems = TRUE,
  PositiveGrowthThreshold = 5,
  NegativeGrowthThreshold = -2,
  Pioneers = NULL,
  PioneersGrowthThreshold = 7.5,
  WhatToCorrect = c("POM change", "Abnormal growth"),
  CorrectionType = c("individual", "phylogenetic hierarchical"),
  DBHRange = 10,
  MinIndividualNbr = 5,
  DBHCorForDeadTrees = TRUE,
  coef = 0.9
)
```

## Arguments

- Data:

  Dataset (data.frame or data.table) The dataset must contain the
  columns:

  - `IdTree` or `IdStem` (character)

  - `ScientificName_DataHarmonizationCor` (character)

  - `Diameter` (numeric)

  - `IdCensus` (ordered factor)

  - **`POM` (Point Of Measurement) (factor)** or **`HOM` (Height Of
    Measurement) (numeric)** if you want to correct from the **"POM
    change"** If you want to apply the **"phylogenetic hierarchical"**
    correction, the dataset must also contain the columns:

  - `Genus_DataHarmonizationCor` (character)

  - `Family_DataHarmonizationCor` (character)

- UseTaperCorrection:

  (logical) TRUE: transform the tree diameter measured at a given height
  into the diameter corresponding to the default measurement height
  (`DefaultHOM`), using an allometry. FALSE: do not apply a taper
  correction

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

- KeepMeas:

  In case of **multiple diameter measurements** in the same census:
  Possible values: "MaxHOM", "MaxDate" (character).

  - "MaxHOM": apply the correction to the measurement taken at the
    **highest POM**

  - "MaxDate": apply the correction to the **most recent measurement**
    (same IdCensus but more recent date)

- MinDBH:

  Minimum diameter of trees inventoried (in cm) (numeric, 1 value) or
  NULL (Default) if you wish to use the MinDBH indicated in your data,
  which may vary per plot

- AddMissedRecruits:

  (logical) TRUE: adds rows for stem that were supposed to be recruited
  at a prior census, based on their estimated diameter (from linear
  regression) and MinDBH. FALSE: will only indicate in the comment that
  the stem was supposed to be recruited earlier.

- AddMissedStems:

  (logical) if TRUE, adds rows for trees that were missed between two
  censuses, with their estimated diameter (based on linear regression)

- PositiveGrowthThreshold:

  in cm/year: a tree widening by more than this value is considered
  abnormal (numeric, 1 value)

- NegativeGrowthThreshold:

  in cm/census: the possible positive measurement error (+n) cannot be
  corrected until the growth appears abnormal, but a negative
  measurement error can be allowed until -n (a tree does not decrease).
  Thus the positive measurement error (+n) is "compensated". (numeric, 1
  value)

- Pioneers:

  Scientific names of the pioneer species of the site, as in the
  `ScientificName_DataHarmonizationCor` column (characters vector)

- PioneersGrowthThreshold:

  in cm/year: a tree of a pioneer species that widens by more than this
  value is considered abnormal (numeric, 1 value)

- WhatToCorrect:

  Possible values: "POM change", "Abnormal growth" (character). All are
  complementary and recommended.

  - "POM change": detect POM change in the column `POM` and correct the
    Diameter values from it. (Ignored if taper correction is applied)

  - "Abnormal growth": detect if the growth is greater than
    PositiveGrowthThreshold ('PioneersGrowthThreshold' if species
    belongs to 'Pioneers') or smaller than NegativeGrowthThreshold and
    correct it by `CorrectionType`

- CorrectionType:

  Possible values: "individual", "phylogenetic hierarchical" (character,
  1 value).

  - "individual": replace abnormal growth by interpolation from the
    individual values.

  - "phylogenetic hierarchical": replace abnormal growth with the
    average growth of other trees in the dataset, at the specific,
    genus, family or stand level, within a DBH range of x cm (*DBHRange*
    argument). If the number of these trees \< n (*MinIndividualNbr*
    argument) at the specific level, we switch to the genus level etc.

- DBHRange:

  DBH range in cm to take into account to select other trees in the
  dataset to apply "phylogenetic hierarchical" correction (Default:
  10 cm) (numeric, 1 value)

- MinIndividualNbr:

  Minimum number of individuals to take into account in "phylogenetic
  hierarchical" correction (Default: 5) (numeric, 1 value)

- DBHCorForDeadTrees:

  (logical) TRUE: return DBHCor also for dead trees. FALSE: do not
  return DBHCor for dead trees. In this case it is advisable to have
  corrected the tree life status with the *StatusCorrection()* function.

- coef:

  (numeric, 1 value) This is used in individual corrections, to
  calculate weight of the growths by temporal proximity

## Value

Fill the *Comment_DataHarmonization* column with error type information
and add columns:

- *Diameter_DataHarmonizationCor*: corrected trees diameter at default
  HOM

- *DiameterCorrectionMeth_DataHarmonization* = "local linear
  regression","weighted mean"/phylogenetic
  hierarchical("species"/"genus"/"family"/"stand")/ "shift
  realignment"/"Same value".

- *POM_DataHarmonizationCor* (factor): POM value at which the corrected
  diameters are proposed. Corresponds to the 1st POM value at which the
  stem was measured.

- *HOM_DataHarmonizationCor* (numeric): HOM value at which the corrected
  diameters are proposed. Corresponds to the 1st HOM value at which the
  stem was measured.

- *MissedStem_DataHarmonizationCor* if AddMissedStems is TRUE (with
  value TRUE for new rows).

- *MissedRecruit_DataHarmonizationCor* if AddMissedRecruits is TRUE
  (with value TRUE for new rows).

## Details

When there is only 1 `Diameter` value for a tree/stem,
`Diameter_DataHarmonizationCor` takes the original `Diameter` value. If
this value is 0 or \> MaxDBH, `Diameter_DataHarmonizationCor` takes NA.
Diameters not linked to an IdTree/IdStem or to a Census IdCensus are not
processed. Punctual error correction only with linear regression and not
quadratic, because punctual errors are corrected from a local regression
with the 2 framing values.

## Examples

``` r
# library(data.table)
data(TestData)

TestData$HOM[1:3] <- c(0.5,1.5,NA)
TestData$Diameter[21:23] <- c(31,91,14)
TestData <- TestData[!(IdStem %in% "100658_1_auto" & IdCensus %in% 2017), ]

Rslt <- DiameterCorrection(
 TestData,
  WhatToCorrect = c("POM change", "Abnormal growth"),
    CorrectionType = c("phylo"),
    MinIndividualNbr = 1)
#> Warning: We added rows for trees that were supposed to be recruited earlier based on linear extrapolation of growth and MinDBH
#> We added rows for trees that were missed and estimated their missed DBH by linear interpolation

DiameterCorrectionPlot(Rslt, OnlyCorrected = TRUE)
#> Warning: The shape palette can deal with a maximum of 6 discrete values because more
#> than 6 becomes difficult to discriminate
#> ℹ you have requested 7 values. Consider specifying shapes manually if you need
#>   that many of them.
#> Warning: The shape palette can deal with a maximum of 6 discrete values because more
#> than 6 becomes difficult to discriminate
#> ℹ you have requested 7 values. Consider specifying shapes manually if you need
#>   that many of them.
#> Warning: The shape palette can deal with a maximum of 6 discrete values because more
#> than 6 becomes difficult to discriminate
#> ℹ you have requested 7 values. Consider specifying shapes manually if you need
#>   that many of them.
#> Warning: Removed 80 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: The shape palette can deal with a maximum of 6 discrete values because more
#> than 6 becomes difficult to discriminate
#> ℹ you have requested 7 values. Consider specifying shapes manually if you need
#>   that many of them.
#> Warning: Removed 80 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: The shape palette can deal with a maximum of 6 discrete values because more
#> than 6 becomes difficult to discriminate
#> ℹ you have requested 7 values. Consider specifying shapes manually if you need
#>   that many of them.
#> Warning: Removed 80 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: The shape palette can deal with a maximum of 6 discrete values because more
#> than 6 becomes difficult to discriminate
#> ℹ you have requested 7 values. Consider specifying shapes manually if you need
#>   that many of them.
#> Warning: Removed 80 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: The shape palette can deal with a maximum of 6 discrete values because more
#> than 6 becomes difficult to discriminate
#> ℹ you have requested 7 values. Consider specifying shapes manually if you need
#>   that many of them.
#> Warning: Removed 80 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: The shape palette can deal with a maximum of 6 discrete values because more
#> than 6 becomes difficult to discriminate
#> ℹ you have requested 7 values. Consider specifying shapes manually if you need
#>   that many of them.
#> Warning: Removed 80 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: The shape palette can deal with a maximum of 6 discrete values because more
#> than 6 becomes difficult to discriminate
#> ℹ you have requested 7 values. Consider specifying shapes manually if you need
#>   that many of them.
#> Warning: Removed 80 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: The shape palette can deal with a maximum of 6 discrete values because more
#> than 6 becomes difficult to discriminate
#> ℹ you have requested 7 values. Consider specifying shapes manually if you need
#>   that many of them.
#> Warning: Removed 80 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
```
