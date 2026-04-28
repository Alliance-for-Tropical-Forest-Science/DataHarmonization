# Full error processing

Full error processing

## Usage

``` r
FullErrorProcessing(
  Data,
  DeathConfirmation = 2,
  UseSize = FALSE,
  AddRowsForForgottenCensuses = TRUE,
  UseTaperCorrection = TRUE,
  DefaultHOM = 1.3,
  TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  TaperFormula = function(DAB, HOM, TaperParameter, DefaultHOM) DAB/(exp(-TaperParameter
    * (HOM - DefaultHOM))),
  KeepMeas = c("MaxHOM", "MaxDate"),
  PositiveGrowthThreshold = 5,
  NegativeGrowthThreshold = -2,
  Pioneers = NULL,
  PioneersGrowthThreshold = 7.5,
  WhatToCorrect = c("POM change", "Abnormal growth"),
  CorrectionType = c("individual", "phylogenetic hierarchical"),
  DBHRange = 10,
  MinIndividualNbr = 5,
  MinDBH = NULL,
  AddMissedRecruits = T,
  AddMissedStems = T,
  DBHCorForDeadTrees = TRUE,
  coef = 0.9,
  OnlyDetectMissedRecruits = FALSE
)
```

## Arguments

- Data:

  Dataset (data.frame or data.table)

- DeathConfirmation:

  Number of times (censuses) needed for an unseen tree to be considered
  dead (numeric) (Default = 2 censuses)

- UseSize:

  Use the size presence (\> min DBH) as a witness of the living status
  of the tree (logical) (Default = FALSE)

- AddRowsForForgottenCensuses:

  TRUE: adds rows for forgotten censuses, FALSE: does not add any rows
  (logical).

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

  In case of **multiple diameter measurements** in the same census year:
  Possible values: "MaxHOM", "MaxDate" (character).

  - "MaxHOM": apply the correction to the measurement taken at the
    **highest POM**

  - "MaxDate": apply the correction to the **most recent measurement**
    (same year but more recent date)

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

- DBHCorForDeadTrees:

  (logical) TRUE: return DBHCor also for dead trees. FALSE: do not
  return DBHCor for dead trees. In this case it is advisable to have
  corrected the tree life status with the *StatusCorrection()* function.

- coef:

  (numeric, 1 value) This is used in individual corrections, to
  calculate weight of the growths by temporal proximity

- OnlyDetectMissedRecruits:

  TRUE: Only detect errors, FALSE: detect and correct errors (logical)

## Value

The original dataset (data.table) with a *Comment* column containing
information on the errors detected per row, the correction columns, and
columns containing correction methods.

## Details

Detect errors or detect and correct errors:

- Check general errors (*GeneralErrorsDetection*)

- Check botanical identification (*BotanicalCorrection*)

- Check the life status evolution of the trees/stems
  (*StatusCorrection*)

- Apply a taper allometry on diameters measured at heights different
  from the default HOM (*TaperCorrection*)

- Check diameter evolution of the trees (*DiameterCorrection*)

- Check tree/stem recruitment (*RecruitmentCorrection*)

## See also

[GeneralErrorsDetection](GeneralErrorsDetection.md),
[BotanicalCorrection](BotanicalCorrection.md),
[StatusCorrection](StatusCorrection.md),
[TaperCorrection](TaperCorrection.md),
[DiameterCorrection](DiameterCorrection.md),
[RecruitmentCorrection](RecruitmentCorrection.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(TestData)
Rslt <- FullErrorProcessing(TestData,
 OnlyDetectMissedRecruits = TRUE,
  AddRowsForForgottenCensuses = FALSE)

Rslt_Test <- FullErrorProcessing(TestData)
Rslt_Panama <- FullErrorProcessing(PanamaFormated)
} # }
```
