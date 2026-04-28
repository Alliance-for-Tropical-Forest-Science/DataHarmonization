# UniqueMeasurement

Of the repeated measurements of an individual in the same census, keep
only the measurements (rows) taken at the **highest POM** (*KeepMeas =
"MaxHOM"*), and/or the **most recent measurement** (same census but more
recent date) (*KeepMeas = "MaxDate"*)

## Usage

``` r
UniqueMeasurement(Data, KeepMeas = c("MaxHOM", "MaxDate"), ID = "IdStem")
```

## Arguments

- Data:

  Dataset (data.table)

- KeepMeas:

  In case of **multiple measurements** in the same census: Possible
  values: "MaxHOM", "MaxDate" (character).

  - "MaxHOM": keep the measurement taken at the **highest HOM/POM**

  - "MaxDate": keep the **most recent measurement** (same census but
    more recent date)

- ID:

  Column name indicating the identifier of the individual (character)

## Value

Dataset (data.table) with 1 measurement (1 row) per IdCensus

## Examples

``` r
library(data.table)

Data <- data.table( # 7 rows
IdStem = c(rep("a", 4), rep("b", 2), "c"),
IdCensus = c(rep(2000, 3), 2001, rep(2000, 3)),
Date = as.Date(c("2000-01-10", "2000-01-20", "2000-01-30", "2001-01-10",
                 rep("2000-01-10", 3))),
HOM = c(rep(1, 3), 2, 1, 2, 1)
)
Data
#>    IdStem IdCensus       Date   HOM
#>    <char>    <num>     <Date> <num>
#> 1:      a     2000 2000-01-10     1
#> 2:      a     2000 2000-01-20     1
#> 3:      a     2000 2000-01-30     1
#> 4:      a     2001 2001-01-10     2
#> 5:      b     2000 2000-01-10     1
#> 6:      b     2000 2000-01-10     2
#> 7:      c     2000 2000-01-10     1

UniqueMeasurement(TestData)
#>       MinDBH IdCensus  Year Month   Day       Date    Site Cluster   Plot
#>        <num>    <ord> <num> <num> <num>     <Date>  <char>  <char> <char>
#>    1:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    2:  10.03     2017  2017     7    19 2017-07-19 Paracou    <NA>      6
#>    3:  10.03     2018  2018     7     8 2018-07-08 Paracou    <NA>      6
#>    4:  10.03     2019  2019     6     7 2019-06-07 Paracou    <NA>      6
#>    5:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#>   ---                                                                    
#> 4850:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4851:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4852:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4853:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4854:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#>       PlotArea PlotElevation Subplot SubplotArea PlotViewID PlotLat PlotLon
#>          <num>         <num>  <char>       <num>     <char>   <num>   <num>
#>    1:     6.25            NA       1          NA       <NA>      NA      NA
#>    2:     6.25            NA       1          NA       <NA>      NA      NA
#>    3:     6.25            NA       1          NA       <NA>      NA      NA
#>    4:     6.25            NA       1          NA       <NA>      NA      NA
#>    5:     6.25            NA       1          NA       <NA>      NA      NA
#>   ---                                                                      
#> 4850:     6.25            NA       1          NA       <NA>      NA      NA
#> 4851:     6.25            NA       1          NA       <NA>      NA      NA
#> 4852:     6.25            NA       1          NA       <NA>      NA      NA
#> 4853:     6.25            NA       1          NA       <NA>      NA      NA
#> 4854:     6.25            NA       1          NA       <NA>      NA      NA
#>       XPlotUTM YPlotUTM SubplotLat SubplotLon XSubplotUTM YSubplotUTM
#>          <num>    <num>      <num>      <num>       <num>       <num>
#>    1:       NA       NA         NA         NA          NA          NA
#>    2:       NA       NA         NA         NA          NA          NA
#>    3:       NA       NA         NA         NA          NA          NA
#>    4:       NA       NA         NA         NA          NA          NA
#>    5:       NA       NA         NA         NA          NA          NA
#>   ---                                                                
#> 4850:       NA       NA         NA         NA          NA          NA
#> 4851:       NA       NA         NA         NA          NA          NA
#> 4852:       NA       NA         NA         NA          NA          NA
#> 4853:       NA       NA         NA         NA          NA          NA
#> 4854:       NA       NA         NA         NA          NA          NA
#>                   ScientificName      VernName        Family        Genus
#>                           <char>        <char>        <char>       <char>
#>    1:      Sandwithia guyanensis     wata tiki Euphorbiaceae   Sandwithia
#>    2:      Sandwithia guyanensis     wata tiki Euphorbiaceae   Sandwithia
#>    3:      Sandwithia guyanensis     wata tiki Euphorbiaceae   Sandwithia
#>    4:      Sandwithia guyanensis     wata tiki Euphorbiaceae   Sandwithia
#>    5:      Sandwithia guyanensis     wata tiki Euphorbiaceae   Sandwithia
#>   ---                                                                    
#> 4850:        Gustavia hexapetala  man tapuhupa Lecythidaceae     Gustavia
#> 4851:        Pradosia cochlearia       kimboto    Sapotaceae     Pradosia
#> 4852: Pogonophora schomburgkiana     geli koko      Peraceae  Pogonophora
#> 4853:        Pseudolmedia laevis satine rubane      Moraceae Pseudolmedia
#> 4854:          Inga lomatophylla    kodia weko      Fabaceae         Inga
#>              Species Subspecies Variety Voucher IdLevel Authority CommercialSp
#>               <char>     <char>  <char>  <char>  <char>    <char>       <lgcl>
#>    1:     guyanensis       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE
#>    2:     guyanensis       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE
#>    3:     guyanensis       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE
#>    4:     guyanensis       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE
#>    5:     guyanensis       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE
#>   ---                                                                         
#> 4850:     hexapetala       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE
#> 4851:     cochlearia       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE
#> 4852: schomburgkiana       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE
#> 4853:         laevis       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE
#> 4854:   lomatophylla       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE
#>       LifeForm TreeFieldNum IdTree StemFieldNum        IdStem TreeLat TreeLon
#>         <char>       <char> <char>       <char>        <char>   <num>   <num>
#>    1:     tree            1 100621         <NA> 100621_1_auto      NA      NA
#>    2:     tree            1 100621         <NA> 100621_1_auto      NA      NA
#>    3:     tree            1 100621         <NA> 100621_1_auto      NA      NA
#>    4:     tree            1 100621         <NA> 100621_1_auto      NA      NA
#>    5:     tree            1 100621         <NA> 100621_1_auto      NA      NA
#>   ---                                                                        
#> 4850:     tree         3008 418631         <NA> 418631_1_auto      NA      NA
#> 4851:     tree         3009 418632         <NA> 418632_1_auto      NA      NA
#> 4852:     tree         3010 418633         <NA> 418633_1_auto      NA      NA
#> 4853:     tree         3011 418634         <NA> 418634_1_auto      NA      NA
#> 4854:     tree         3012 418635         <NA> 418635_1_auto      NA      NA
#>       XTreeUTM YTreeUTM XTreePlot YTreePlot XTreeSubplot YTreeSubplot
#>          <num>    <num>     <num>     <num>        <num>        <num>
#>    1:       NA       NA        NA        NA           NA           NA
#>    2:       NA       NA        NA        NA           NA           NA
#>    3:       NA       NA        NA        NA           NA           NA
#>    4:       NA       NA        NA        NA           NA           NA
#>    5:       NA       NA        NA        NA           NA           NA
#>   ---                                                                
#> 4850:       NA       NA        NA        NA           NA           NA
#> 4851:       NA       NA        NA        NA           NA           NA
#> 4852:       NA       NA        NA        NA           NA           NA
#> 4853:       NA       NA        NA        NA           NA           NA
#> 4854:       NA       NA        NA        NA           NA           NA
#>       LifeStatus DeadStatus Diameter    BD  Circ BCirc   HOM    POM  BHOM
#>           <lgcl>     <lgcl>    <num> <num> <num> <num> <num> <char> <num>
#>    1:       TRUE         NA    13.37    NA  42.0    NA   1.3   <NA>    NA
#>    2:       TRUE         NA    13.37    NA  42.0    NA   1.3   <NA>    NA
#>    3:       TRUE         NA    13.37    NA  42.0    NA   1.3   <NA>    NA
#>    4:       TRUE         NA    13.37    NA  42.0    NA   1.3   <NA>    NA
#>    5:      FALSE         NA    13.37    NA  42.0    NA   1.3   <NA>    NA
#>   ---                                                                    
#> 4850:       TRUE         NA    10.19    NA  32.0    NA   1.3   <NA>    NA
#> 4851:       TRUE         NA    10.03    NA  31.5    NA   1.3   <NA>    NA
#> 4852:       TRUE         NA    10.03    NA  31.5    NA   1.3   <NA>    NA
#> 4853:       TRUE         NA    10.19    NA  32.0    NA   1.3   <NA>    NA
#> 4854:       TRUE         NA    10.19    NA  32.0    NA   1.3   <NA>    NA
#>         BPOM TreeHeight TreeFieldNumOriginal IdTreeOriginal
#>       <char>      <num>                <num>          <int>
#>    1:   <NA>         NA                    1         100621
#>    2:   <NA>         NA                    1         100621
#>    3:   <NA>         NA                    1         100621
#>    4:   <NA>         NA                    1         100621
#>    5:   <NA>         NA                    1         100621
#>   ---                                                      
#> 4850:   <NA>         NA                 3008         418631
#> 4851:   <NA>         NA                 3009         418632
#> 4852:   <NA>         NA                 3010         418633
#> 4853:   <NA>         NA                 3011         418634
#> 4854:   <NA>         NA                 3012         418635
#>       CommercialSpOriginal DateOriginal LifeStatusOriginal
#>                     <lgcl>       <char>             <lgcl>
#>    1:                FALSE   2016-09-14               TRUE
#>    2:                FALSE   2017-07-19               TRUE
#>    3:                FALSE   2018-07-08               TRUE
#>    4:                FALSE   2019-06-07               TRUE
#>    5:                FALSE   2020-06-04              FALSE
#>   ---                                                     
#> 4850:                FALSE   2020-06-04               TRUE
#> 4851:                FALSE   2020-06-04               TRUE
#> 4852:                FALSE   2020-06-04               TRUE
#> 4853:                 TRUE   2020-06-04               TRUE
#> 4854:                FALSE   2020-06-04               TRUE
```
