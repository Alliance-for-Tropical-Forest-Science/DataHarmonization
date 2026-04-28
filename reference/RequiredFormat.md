# RequiredFormat

RequiredFormat

## Usage

``` r
RequiredFormat(Data, input, x = NULL, MeasLevel = NULL)
```

## Arguments

- Data:

  Forest inventory data set (data.frame or data.table) - already
  stacked, merged and tidyed

- input:

  A named list, typically the output of function
  RequiredFormat_interactive, also called site profile. It has
  information on column names correspondence, size units etc...

- x:

  For internal use when function used by Shiny app

- MeasLevel:

  your deepest level of measurements(When function is run outside of
  Shiny app). Options are one of c("Plot", "Species", "Tree", "Stem")

## Value

Input inventory (data.frame) in the required package format.

## Details

This function takes the forest inventory data.frame or data.table as it
is, and converts the column names to the standardized names used in this
package. It also generates missing information, when possible (e.g.
Diameter when only circumference is givent, Genus and Species when only
scientifique name is given etc...). All the decisions are made based on
what is provided in the input argument, which is a named list, as
returned by function RequiredFormat_interactive or Profile.rds file
downloaded from shiny app

## Examples

``` r
if (FALSE) { # \dontrun{
data(ParacouSubset)
data(ParacouProfile)
ParacouSubsetFormated <- RequiredFormat(
  ParacouSubset,
  input = ParacouProfile,
  MeasLevel = "Tree")
               } # }
```
