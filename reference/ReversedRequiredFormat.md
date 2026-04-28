# ReversedRequiredFormat

ReversedRequiredFormat

## Usage

``` r
ReversedRequiredFormat(Data, input, x = NULL, Untidy = FALSE)
```

## Arguments

- Data:

  Standardized data, returned from
  [`RequiredFormat()`](RequiredFormat.md)

- input:

  A named list, typically the output of function
  RequiredFormat_interactive, also called site profile with information
  on column names correspondence, size units etc... Chosen to be the
  output profile the user wants their data turned into

- x:

  For internal use when function used by Shiny app

- Untidy:

  (logical). If TRUE and input\$tidy exists, the data will be untidy
  (changed from long to wide format, according to input information)

## Value

(data.frame) in the format given the profile selected in input.

## Details

This function takes the standardized forest inventory data.table
(returned by [`RequiredFormat()`](RequiredFormat.md)) and converts the
column names to the names of the profile given as input.

## Examples

``` r
if (FALSE) { # \dontrun{

data(ParacouSubsetFormated)
data("ForestGeoProfile")
ReversedRequiredFormat(ParacouSubsetFormated, ForestGeoProfile)
               } # }
```
