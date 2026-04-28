# General Errors Detection

General Errors Detection

## Usage

``` r
GeneralErrorsDetection(Data)
```

## Arguments

- Data:

  Dataset (data.frame or data.table)

## Value

The input dataset (data.table) with a new *Comment* column with error
type informations.

## Details

Detect errors

- Remove **duplicated rows**

- Check **missing value** in
  X-YTreeUTM/PlotArea/Plot/Subplot/Year/TreeFieldNum/
  IdTree/IdStem/Diameter/POM/HOM/Family/Genus/Species/VernName

- Check **missing value** (NA/0) in the measurement variables:
  "Diameter", "HOM", "TreeHeight", "StemHeight"

- Check of the **unique association of the IdTree with plot, subplot**
  **and TreeFieldNum** (at the site scale)

- Check **duplicated IdTree/IdStem** in a census (at the site scale)

- Check for trees **outside the subplot** (not implemented yet)

- Check **invariant coordinates per IdTree/IdStem**

- Check **fix Plot and Subplot number** (not implemented yet)

## Examples

``` r
library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%
data("TestData")

Rslt <- GeneralErrorsDetection(TestData)
#> Warning: Non-unique association of IdTree(s) with Plot, Subplot and TreeFieldNum:
#>    IdTree   Plot Subplot TreeFieldNum
#>    <char> <char>  <char>       <char>
#> 1: 100767      6       1          150
#> 2: 100767      6       1          248
#> 3: 101039      6       1          288
#> 4: 101039      6       1          428
#> 
```
