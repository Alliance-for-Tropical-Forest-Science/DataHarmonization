# Status Correction

Detect errors, or detect errors and correct, the tree life status
evolution over the censuses. Inspired by the code of Nino Page package
(ForestData::correct_alive() and .correct_alive_tree())

## Usage

``` r
StatusCorrection(
  Data,
  DeathConfirmation = 2,
  UseSize = FALSE,
  AddRowsForForgottenCensuses = TRUE,
  KeepMeas = c("MaxHOM", "MaxDate")
)
```

## Arguments

- Data:

  Dataset (data.frame or data.table) The *LifeStatus* column must be
  coded as: - TRUE = alive, - FALSE = dead, - NA = unseen

- DeathConfirmation:

  Number of times (censuses) needed for an unseen tree to be considered
  dead (numeric) (Default = 2 censuses)

- UseSize:

  Use the size presence (\> min DBH) as a witness of the living status
  of the tree (logical) (Default = FALSE)

- AddRowsForForgottenCensuses:

  TRUE: adds rows for forgotten censuses, FALSE: does not add any rows
  (logical).

- KeepMeas:

  In case of **multiple diameter measurements** in the same census year:
  Possible values: "MaxHOM", "MaxDate" (character).

  - "MaxHOM": apply the correction to the measurement taken at the
    **highest POM**

  - "MaxDate": apply the correction to the **most recent measurement**
    (same year but more recent date)

## Value

Fill the *Comment_DataHarmonization* column with error type information
and add a *LifeStatus_DataHarmonizationCor* column with corrected trees
life status. If AddRowsForForgottenCensuses is TRUE, a column
*MissedStem_DataHarmonizationCor* is added with value TRUE for new rows.

## Details

- if UseSize : if Diameter != NA -\> Alive If (the value in bold is
  modified by the value given after the arrow): (the "\>" gives the
  chronological order of the sequence)

- *Dead* \> Alive -\> NA

- add rows for the forgotten censuses between 2 'Alive' if chosen

- Alive \> *Dead*/*NA* \> Alive -\> Alive

- Alive \> *NA* \> Dead -\> NA

- Alive \> *Dead* \> NA -\> Dead

- Alive \> *NA* \> *NA*: if DeathConfirmation \> unseens -\> NA if
  DeathConfirmation =\< unseens -\> Dead

## Examples

``` r
library(data.table)
data(TestData)

selection <- c("101184", "101433","101435","101436")

# Write the sequence
TestData <- TestData[order(IdCensus)] # arrange IdCensus in ascending order
TestData[IdTree == "101184", LifeStatus := c(TRUE, TRUE, TRUE, TRUE, FALSE)]
#> Index: <IdTree>
#>       MinDBH IdCensus  Year Month   Day       Date    Site Cluster   Plot
#>        <num>    <ord> <num> <num> <num>     <Date>  <char>  <char> <char>
#>    1:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    2:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    3:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    4:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    5:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>   ---                                                                    
#> 4851:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4852:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4853:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4854:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4855:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#>       PlotArea PlotElevation Subplot SubplotArea PlotViewID PlotLat PlotLon
#>          <num>         <num>  <char>       <num>     <char>   <num>   <num>
#>    1:     6.25            NA       1          NA       <NA>      NA      NA
#>    2:     6.25            NA       1          NA       <NA>      NA      NA
#>    3:     6.25            NA       1          NA       <NA>      NA      NA
#>    4:     6.25            NA       1          NA       <NA>      NA      NA
#>    5:     6.25            NA       1          NA       <NA>      NA      NA
#>   ---                                                                      
#> 4851:     6.25            NA       1          NA       <NA>      NA      NA
#> 4852:     6.25            NA       1          NA       <NA>      NA      NA
#> 4853:     6.25            NA       1          NA       <NA>      NA      NA
#> 4854:     6.25            NA       1          NA       <NA>      NA      NA
#> 4855:     6.25            NA       1          NA       <NA>      NA      NA
#>       XPlotUTM YPlotUTM SubplotLat SubplotLon XSubplotUTM YSubplotUTM
#>          <num>    <num>      <num>      <num>       <num>       <num>
#>    1:       NA       NA         NA         NA          NA          NA
#>    2:       NA       NA         NA         NA          NA          NA
#>    3:       NA       NA         NA         NA          NA          NA
#>    4:       NA       NA         NA         NA          NA          NA
#>    5:       NA       NA         NA         NA          NA          NA
#>   ---                                                                
#> 4851:       NA       NA         NA         NA          NA          NA
#> 4852:       NA       NA         NA         NA          NA          NA
#> 4853:       NA       NA         NA         NA          NA          NA
#> 4854:       NA       NA         NA         NA          NA          NA
#> 4855:       NA       NA         NA         NA          NA          NA
#>               ScientificName   VernName           Family       Genus    Species
#>                       <char>     <char>           <char>      <char>     <char>
#>    1:         Eperua falcata       wapa         Fabaceae      Eperua    falcata
#>    2:  Conceveiba guianensis     kusisi    Euphorbiaceae  Conceveiba guianensis
#>    3:     Moronobea coccinea  Moronobea       Clusiaceae   Moronobea   coccinea
#>    4:    Pouteria melanopoda    bakuman       Sapotaceae    Pouteria melanopoda
#>    5:  Sandwithia guyanensis  wata tiki    Euphorbiaceae  Sandwithia guyanensis
#>   ---                                                                          
#> 4851: Micropholis guyanensis    bakuman       Sapotaceae Micropholis guyanensis
#> 4852:    Lecythis persistens maho rouge    Lecythidaceae    Lecythis persistens
#> 4853:    Lecythis persistens maho rouge    Lecythidaceae    Lecythis persistens
#> 4854:         Eperua falcata       wapa         Fabaceae      Eperua    falcata
#> 4855:           Licania alba       koko Chrysobalanaceae     Licania       alba
#>       Subspecies Variety Voucher IdLevel Authority CommercialSp LifeForm
#>           <char>  <char>  <char>  <char>    <char>       <lgcl>   <char>
#>    1:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#>    2:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>    3:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#>    4:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>    5:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>   ---                                                                   
#> 4851:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4852:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4853:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4854:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#> 4855:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>       TreeFieldNum IdTree StemFieldNum        IdStem TreeLat TreeLon XTreeUTM
#>             <char> <char>       <char>        <char>   <num>   <num>    <num>
#>    1:           45 100665         <NA> 100665_1_auto      NA      NA       NA
#>    2:          148 100765         <NA> 100765_1_auto      NA      NA       NA
#>    3:           46 100666         <NA> 100666_1_auto      NA      NA       NA
#>    4:           49 100669         <NA> 100669_1_auto      NA      NA       NA
#>    5:            1 100621         <NA> 100621_1_auto      NA      NA       NA
#>   ---                                                                        
#> 4851:          922 101517         <NA> 101517_1_auto      NA      NA       NA
#> 4852:          884 101479         <NA> 101479_1_auto      NA      NA       NA
#> 4853:          887 101482         <NA> 101482_1_auto      NA      NA       NA
#> 4854:          896 101491         <NA> 101491_1_auto      NA      NA       NA
#> 4855:          905 101500         <NA> 101500_1_auto      NA      NA       NA
#>       YTreeUTM XTreePlot YTreePlot XTreeSubplot YTreeSubplot LifeStatus
#>          <num>     <num>     <num>        <num>        <num>     <lgcl>
#>    1:       NA        NA        NA           NA           NA       TRUE
#>    2:       NA        NA        NA           NA           NA       TRUE
#>    3:       NA        NA        NA           NA           NA       TRUE
#>    4:       NA        NA        NA           NA           NA       TRUE
#>    5:       NA        NA        NA           NA           NA       TRUE
#>   ---                                                                  
#> 4851:       NA        NA        NA           NA           NA       TRUE
#> 4852:       NA        NA        NA           NA           NA       TRUE
#> 4853:       NA        NA        NA           NA           NA       TRUE
#> 4854:       NA        NA        NA           NA           NA       TRUE
#> 4855:       NA        NA        NA           NA           NA       TRUE
#>       DeadStatus Diameter    BD  Circ BCirc   HOM    POM  BHOM   BPOM
#>           <lgcl>    <num> <num> <num> <num> <num> <char> <num> <char>
#>    1:         NA    53.16    NA 167.0    NA   1.3   <NA>    NA   <NA>
#>    2:         NA    14.32    NA  45.0    NA   1.3   <NA>    NA   <NA>
#>    3:         NA    19.89    NA  62.5    NA   1.3   <NA>    NA   <NA>
#>    4:         NA    41.38    NA 130.0    NA   1.3   <NA>    NA   <NA>
#>    5:         NA    13.37    NA  42.0    NA   1.3   <NA>    NA   <NA>
#>   ---                                                                
#> 4851:         NA    22.60    NA  71.0    NA   1.3   <NA>    NA   <NA>
#> 4852:         NA    16.39    NA  51.5    NA   1.3   <NA>    NA   <NA>
#> 4853:         NA    20.85    NA  65.5    NA   1.3   <NA>    NA   <NA>
#> 4854:         NA    29.60    NA  93.0    NA   1.3   <NA>    NA   <NA>
#> 4855:         NA    27.53    NA  86.5    NA   1.3   <NA>    NA   <NA>
#>       TreeHeight TreeFieldNumOriginal IdTreeOriginal CommercialSpOriginal
#>            <num>                <num>          <int>               <lgcl>
#>    1:         NA                   45         100665                 TRUE
#>    2:         NA                  148         100765                FALSE
#>    3:         NA                   46         100666                 TRUE
#>    4:         NA                   49         100669                FALSE
#>    5:         NA                    1         100621                FALSE
#>   ---                                                                    
#> 4851:         NA                  922         101517                FALSE
#> 4852:         NA                  884         101479                FALSE
#> 4853:         NA                  887         101482                FALSE
#> 4854:         NA                  896         101491                 TRUE
#> 4855:         NA                  905         101500                FALSE
#>       DateOriginal LifeStatusOriginal
#>             <char>             <lgcl>
#>    1:   2016-09-14               TRUE
#>    2:   2016-09-14               TRUE
#>    3:   2016-09-14               TRUE
#>    4:   2016-09-14               TRUE
#>    5:   2016-09-14               TRUE
#>   ---                                
#> 4851:   2020-06-04               TRUE
#> 4852:   2020-06-04               TRUE
#> 4853:   2020-06-04               TRUE
#> 4854:   2020-06-04               TRUE
#> 4855:   2020-06-04               TRUE
TestData[IdTree == "101433", LifeStatus := c(FALSE, TRUE, TRUE, TRUE, TRUE)]
#> Index: <IdTree>
#>       MinDBH IdCensus  Year Month   Day       Date    Site Cluster   Plot
#>        <num>    <ord> <num> <num> <num>     <Date>  <char>  <char> <char>
#>    1:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    2:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    3:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    4:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    5:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>   ---                                                                    
#> 4851:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4852:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4853:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4854:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4855:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#>       PlotArea PlotElevation Subplot SubplotArea PlotViewID PlotLat PlotLon
#>          <num>         <num>  <char>       <num>     <char>   <num>   <num>
#>    1:     6.25            NA       1          NA       <NA>      NA      NA
#>    2:     6.25            NA       1          NA       <NA>      NA      NA
#>    3:     6.25            NA       1          NA       <NA>      NA      NA
#>    4:     6.25            NA       1          NA       <NA>      NA      NA
#>    5:     6.25            NA       1          NA       <NA>      NA      NA
#>   ---                                                                      
#> 4851:     6.25            NA       1          NA       <NA>      NA      NA
#> 4852:     6.25            NA       1          NA       <NA>      NA      NA
#> 4853:     6.25            NA       1          NA       <NA>      NA      NA
#> 4854:     6.25            NA       1          NA       <NA>      NA      NA
#> 4855:     6.25            NA       1          NA       <NA>      NA      NA
#>       XPlotUTM YPlotUTM SubplotLat SubplotLon XSubplotUTM YSubplotUTM
#>          <num>    <num>      <num>      <num>       <num>       <num>
#>    1:       NA       NA         NA         NA          NA          NA
#>    2:       NA       NA         NA         NA          NA          NA
#>    3:       NA       NA         NA         NA          NA          NA
#>    4:       NA       NA         NA         NA          NA          NA
#>    5:       NA       NA         NA         NA          NA          NA
#>   ---                                                                
#> 4851:       NA       NA         NA         NA          NA          NA
#> 4852:       NA       NA         NA         NA          NA          NA
#> 4853:       NA       NA         NA         NA          NA          NA
#> 4854:       NA       NA         NA         NA          NA          NA
#> 4855:       NA       NA         NA         NA          NA          NA
#>               ScientificName   VernName           Family       Genus    Species
#>                       <char>     <char>           <char>      <char>     <char>
#>    1:         Eperua falcata       wapa         Fabaceae      Eperua    falcata
#>    2:  Conceveiba guianensis     kusisi    Euphorbiaceae  Conceveiba guianensis
#>    3:     Moronobea coccinea  Moronobea       Clusiaceae   Moronobea   coccinea
#>    4:    Pouteria melanopoda    bakuman       Sapotaceae    Pouteria melanopoda
#>    5:  Sandwithia guyanensis  wata tiki    Euphorbiaceae  Sandwithia guyanensis
#>   ---                                                                          
#> 4851: Micropholis guyanensis    bakuman       Sapotaceae Micropholis guyanensis
#> 4852:    Lecythis persistens maho rouge    Lecythidaceae    Lecythis persistens
#> 4853:    Lecythis persistens maho rouge    Lecythidaceae    Lecythis persistens
#> 4854:         Eperua falcata       wapa         Fabaceae      Eperua    falcata
#> 4855:           Licania alba       koko Chrysobalanaceae     Licania       alba
#>       Subspecies Variety Voucher IdLevel Authority CommercialSp LifeForm
#>           <char>  <char>  <char>  <char>    <char>       <lgcl>   <char>
#>    1:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#>    2:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>    3:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#>    4:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>    5:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>   ---                                                                   
#> 4851:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4852:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4853:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4854:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#> 4855:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>       TreeFieldNum IdTree StemFieldNum        IdStem TreeLat TreeLon XTreeUTM
#>             <char> <char>       <char>        <char>   <num>   <num>    <num>
#>    1:           45 100665         <NA> 100665_1_auto      NA      NA       NA
#>    2:          148 100765         <NA> 100765_1_auto      NA      NA       NA
#>    3:           46 100666         <NA> 100666_1_auto      NA      NA       NA
#>    4:           49 100669         <NA> 100669_1_auto      NA      NA       NA
#>    5:            1 100621         <NA> 100621_1_auto      NA      NA       NA
#>   ---                                                                        
#> 4851:          922 101517         <NA> 101517_1_auto      NA      NA       NA
#> 4852:          884 101479         <NA> 101479_1_auto      NA      NA       NA
#> 4853:          887 101482         <NA> 101482_1_auto      NA      NA       NA
#> 4854:          896 101491         <NA> 101491_1_auto      NA      NA       NA
#> 4855:          905 101500         <NA> 101500_1_auto      NA      NA       NA
#>       YTreeUTM XTreePlot YTreePlot XTreeSubplot YTreeSubplot LifeStatus
#>          <num>     <num>     <num>        <num>        <num>     <lgcl>
#>    1:       NA        NA        NA           NA           NA       TRUE
#>    2:       NA        NA        NA           NA           NA       TRUE
#>    3:       NA        NA        NA           NA           NA       TRUE
#>    4:       NA        NA        NA           NA           NA       TRUE
#>    5:       NA        NA        NA           NA           NA       TRUE
#>   ---                                                                  
#> 4851:       NA        NA        NA           NA           NA       TRUE
#> 4852:       NA        NA        NA           NA           NA       TRUE
#> 4853:       NA        NA        NA           NA           NA       TRUE
#> 4854:       NA        NA        NA           NA           NA       TRUE
#> 4855:       NA        NA        NA           NA           NA       TRUE
#>       DeadStatus Diameter    BD  Circ BCirc   HOM    POM  BHOM   BPOM
#>           <lgcl>    <num> <num> <num> <num> <num> <char> <num> <char>
#>    1:         NA    53.16    NA 167.0    NA   1.3   <NA>    NA   <NA>
#>    2:         NA    14.32    NA  45.0    NA   1.3   <NA>    NA   <NA>
#>    3:         NA    19.89    NA  62.5    NA   1.3   <NA>    NA   <NA>
#>    4:         NA    41.38    NA 130.0    NA   1.3   <NA>    NA   <NA>
#>    5:         NA    13.37    NA  42.0    NA   1.3   <NA>    NA   <NA>
#>   ---                                                                
#> 4851:         NA    22.60    NA  71.0    NA   1.3   <NA>    NA   <NA>
#> 4852:         NA    16.39    NA  51.5    NA   1.3   <NA>    NA   <NA>
#> 4853:         NA    20.85    NA  65.5    NA   1.3   <NA>    NA   <NA>
#> 4854:         NA    29.60    NA  93.0    NA   1.3   <NA>    NA   <NA>
#> 4855:         NA    27.53    NA  86.5    NA   1.3   <NA>    NA   <NA>
#>       TreeHeight TreeFieldNumOriginal IdTreeOriginal CommercialSpOriginal
#>            <num>                <num>          <int>               <lgcl>
#>    1:         NA                   45         100665                 TRUE
#>    2:         NA                  148         100765                FALSE
#>    3:         NA                   46         100666                 TRUE
#>    4:         NA                   49         100669                FALSE
#>    5:         NA                    1         100621                FALSE
#>   ---                                                                    
#> 4851:         NA                  922         101517                FALSE
#> 4852:         NA                  884         101479                FALSE
#> 4853:         NA                  887         101482                FALSE
#> 4854:         NA                  896         101491                 TRUE
#> 4855:         NA                  905         101500                FALSE
#>       DateOriginal LifeStatusOriginal
#>             <char>             <lgcl>
#>    1:   2016-09-14               TRUE
#>    2:   2016-09-14               TRUE
#>    3:   2016-09-14               TRUE
#>    4:   2016-09-14               TRUE
#>    5:   2016-09-14               TRUE
#>   ---                                
#> 4851:   2020-06-04               TRUE
#> 4852:   2020-06-04               TRUE
#> 4853:   2020-06-04               TRUE
#> 4854:   2020-06-04               TRUE
#> 4855:   2020-06-04               TRUE
TestData[IdTree == "101435", LifeStatus := c(TRUE, TRUE, NA, FALSE, TRUE)]
#> Index: <IdTree>
#>       MinDBH IdCensus  Year Month   Day       Date    Site Cluster   Plot
#>        <num>    <ord> <num> <num> <num>     <Date>  <char>  <char> <char>
#>    1:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    2:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    3:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    4:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    5:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>   ---                                                                    
#> 4851:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4852:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4853:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4854:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4855:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#>       PlotArea PlotElevation Subplot SubplotArea PlotViewID PlotLat PlotLon
#>          <num>         <num>  <char>       <num>     <char>   <num>   <num>
#>    1:     6.25            NA       1          NA       <NA>      NA      NA
#>    2:     6.25            NA       1          NA       <NA>      NA      NA
#>    3:     6.25            NA       1          NA       <NA>      NA      NA
#>    4:     6.25            NA       1          NA       <NA>      NA      NA
#>    5:     6.25            NA       1          NA       <NA>      NA      NA
#>   ---                                                                      
#> 4851:     6.25            NA       1          NA       <NA>      NA      NA
#> 4852:     6.25            NA       1          NA       <NA>      NA      NA
#> 4853:     6.25            NA       1          NA       <NA>      NA      NA
#> 4854:     6.25            NA       1          NA       <NA>      NA      NA
#> 4855:     6.25            NA       1          NA       <NA>      NA      NA
#>       XPlotUTM YPlotUTM SubplotLat SubplotLon XSubplotUTM YSubplotUTM
#>          <num>    <num>      <num>      <num>       <num>       <num>
#>    1:       NA       NA         NA         NA          NA          NA
#>    2:       NA       NA         NA         NA          NA          NA
#>    3:       NA       NA         NA         NA          NA          NA
#>    4:       NA       NA         NA         NA          NA          NA
#>    5:       NA       NA         NA         NA          NA          NA
#>   ---                                                                
#> 4851:       NA       NA         NA         NA          NA          NA
#> 4852:       NA       NA         NA         NA          NA          NA
#> 4853:       NA       NA         NA         NA          NA          NA
#> 4854:       NA       NA         NA         NA          NA          NA
#> 4855:       NA       NA         NA         NA          NA          NA
#>               ScientificName   VernName           Family       Genus    Species
#>                       <char>     <char>           <char>      <char>     <char>
#>    1:         Eperua falcata       wapa         Fabaceae      Eperua    falcata
#>    2:  Conceveiba guianensis     kusisi    Euphorbiaceae  Conceveiba guianensis
#>    3:     Moronobea coccinea  Moronobea       Clusiaceae   Moronobea   coccinea
#>    4:    Pouteria melanopoda    bakuman       Sapotaceae    Pouteria melanopoda
#>    5:  Sandwithia guyanensis  wata tiki    Euphorbiaceae  Sandwithia guyanensis
#>   ---                                                                          
#> 4851: Micropholis guyanensis    bakuman       Sapotaceae Micropholis guyanensis
#> 4852:    Lecythis persistens maho rouge    Lecythidaceae    Lecythis persistens
#> 4853:    Lecythis persistens maho rouge    Lecythidaceae    Lecythis persistens
#> 4854:         Eperua falcata       wapa         Fabaceae      Eperua    falcata
#> 4855:           Licania alba       koko Chrysobalanaceae     Licania       alba
#>       Subspecies Variety Voucher IdLevel Authority CommercialSp LifeForm
#>           <char>  <char>  <char>  <char>    <char>       <lgcl>   <char>
#>    1:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#>    2:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>    3:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#>    4:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>    5:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>   ---                                                                   
#> 4851:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4852:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4853:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4854:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#> 4855:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>       TreeFieldNum IdTree StemFieldNum        IdStem TreeLat TreeLon XTreeUTM
#>             <char> <char>       <char>        <char>   <num>   <num>    <num>
#>    1:           45 100665         <NA> 100665_1_auto      NA      NA       NA
#>    2:          148 100765         <NA> 100765_1_auto      NA      NA       NA
#>    3:           46 100666         <NA> 100666_1_auto      NA      NA       NA
#>    4:           49 100669         <NA> 100669_1_auto      NA      NA       NA
#>    5:            1 100621         <NA> 100621_1_auto      NA      NA       NA
#>   ---                                                                        
#> 4851:          922 101517         <NA> 101517_1_auto      NA      NA       NA
#> 4852:          884 101479         <NA> 101479_1_auto      NA      NA       NA
#> 4853:          887 101482         <NA> 101482_1_auto      NA      NA       NA
#> 4854:          896 101491         <NA> 101491_1_auto      NA      NA       NA
#> 4855:          905 101500         <NA> 101500_1_auto      NA      NA       NA
#>       YTreeUTM XTreePlot YTreePlot XTreeSubplot YTreeSubplot LifeStatus
#>          <num>     <num>     <num>        <num>        <num>     <lgcl>
#>    1:       NA        NA        NA           NA           NA       TRUE
#>    2:       NA        NA        NA           NA           NA       TRUE
#>    3:       NA        NA        NA           NA           NA       TRUE
#>    4:       NA        NA        NA           NA           NA       TRUE
#>    5:       NA        NA        NA           NA           NA       TRUE
#>   ---                                                                  
#> 4851:       NA        NA        NA           NA           NA       TRUE
#> 4852:       NA        NA        NA           NA           NA       TRUE
#> 4853:       NA        NA        NA           NA           NA       TRUE
#> 4854:       NA        NA        NA           NA           NA       TRUE
#> 4855:       NA        NA        NA           NA           NA       TRUE
#>       DeadStatus Diameter    BD  Circ BCirc   HOM    POM  BHOM   BPOM
#>           <lgcl>    <num> <num> <num> <num> <num> <char> <num> <char>
#>    1:         NA    53.16    NA 167.0    NA   1.3   <NA>    NA   <NA>
#>    2:         NA    14.32    NA  45.0    NA   1.3   <NA>    NA   <NA>
#>    3:         NA    19.89    NA  62.5    NA   1.3   <NA>    NA   <NA>
#>    4:         NA    41.38    NA 130.0    NA   1.3   <NA>    NA   <NA>
#>    5:         NA    13.37    NA  42.0    NA   1.3   <NA>    NA   <NA>
#>   ---                                                                
#> 4851:         NA    22.60    NA  71.0    NA   1.3   <NA>    NA   <NA>
#> 4852:         NA    16.39    NA  51.5    NA   1.3   <NA>    NA   <NA>
#> 4853:         NA    20.85    NA  65.5    NA   1.3   <NA>    NA   <NA>
#> 4854:         NA    29.60    NA  93.0    NA   1.3   <NA>    NA   <NA>
#> 4855:         NA    27.53    NA  86.5    NA   1.3   <NA>    NA   <NA>
#>       TreeHeight TreeFieldNumOriginal IdTreeOriginal CommercialSpOriginal
#>            <num>                <num>          <int>               <lgcl>
#>    1:         NA                   45         100665                 TRUE
#>    2:         NA                  148         100765                FALSE
#>    3:         NA                   46         100666                 TRUE
#>    4:         NA                   49         100669                FALSE
#>    5:         NA                    1         100621                FALSE
#>   ---                                                                    
#> 4851:         NA                  922         101517                FALSE
#> 4852:         NA                  884         101479                FALSE
#> 4853:         NA                  887         101482                FALSE
#> 4854:         NA                  896         101491                 TRUE
#> 4855:         NA                  905         101500                FALSE
#>       DateOriginal LifeStatusOriginal
#>             <char>             <lgcl>
#>    1:   2016-09-14               TRUE
#>    2:   2016-09-14               TRUE
#>    3:   2016-09-14               TRUE
#>    4:   2016-09-14               TRUE
#>    5:   2016-09-14               TRUE
#>   ---                                
#> 4851:   2020-06-04               TRUE
#> 4852:   2020-06-04               TRUE
#> 4853:   2020-06-04               TRUE
#> 4854:   2020-06-04               TRUE
#> 4855:   2020-06-04               TRUE
TestData[IdTree == "101436", LifeStatus := c(TRUE, NA, NA, FALSE, NA)]
#> Index: <IdTree>
#>       MinDBH IdCensus  Year Month   Day       Date    Site Cluster   Plot
#>        <num>    <ord> <num> <num> <num>     <Date>  <char>  <char> <char>
#>    1:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    2:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    3:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    4:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>    5:  10.03     2016  2016     9    14 2016-09-14 Paracou    <NA>      6
#>   ---                                                                    
#> 4851:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4852:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4853:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4854:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#> 4855:  10.03     2020  2020     6     4 2020-06-04 Paracou    <NA>      6
#>       PlotArea PlotElevation Subplot SubplotArea PlotViewID PlotLat PlotLon
#>          <num>         <num>  <char>       <num>     <char>   <num>   <num>
#>    1:     6.25            NA       1          NA       <NA>      NA      NA
#>    2:     6.25            NA       1          NA       <NA>      NA      NA
#>    3:     6.25            NA       1          NA       <NA>      NA      NA
#>    4:     6.25            NA       1          NA       <NA>      NA      NA
#>    5:     6.25            NA       1          NA       <NA>      NA      NA
#>   ---                                                                      
#> 4851:     6.25            NA       1          NA       <NA>      NA      NA
#> 4852:     6.25            NA       1          NA       <NA>      NA      NA
#> 4853:     6.25            NA       1          NA       <NA>      NA      NA
#> 4854:     6.25            NA       1          NA       <NA>      NA      NA
#> 4855:     6.25            NA       1          NA       <NA>      NA      NA
#>       XPlotUTM YPlotUTM SubplotLat SubplotLon XSubplotUTM YSubplotUTM
#>          <num>    <num>      <num>      <num>       <num>       <num>
#>    1:       NA       NA         NA         NA          NA          NA
#>    2:       NA       NA         NA         NA          NA          NA
#>    3:       NA       NA         NA         NA          NA          NA
#>    4:       NA       NA         NA         NA          NA          NA
#>    5:       NA       NA         NA         NA          NA          NA
#>   ---                                                                
#> 4851:       NA       NA         NA         NA          NA          NA
#> 4852:       NA       NA         NA         NA          NA          NA
#> 4853:       NA       NA         NA         NA          NA          NA
#> 4854:       NA       NA         NA         NA          NA          NA
#> 4855:       NA       NA         NA         NA          NA          NA
#>               ScientificName   VernName           Family       Genus    Species
#>                       <char>     <char>           <char>      <char>     <char>
#>    1:         Eperua falcata       wapa         Fabaceae      Eperua    falcata
#>    2:  Conceveiba guianensis     kusisi    Euphorbiaceae  Conceveiba guianensis
#>    3:     Moronobea coccinea  Moronobea       Clusiaceae   Moronobea   coccinea
#>    4:    Pouteria melanopoda    bakuman       Sapotaceae    Pouteria melanopoda
#>    5:  Sandwithia guyanensis  wata tiki    Euphorbiaceae  Sandwithia guyanensis
#>   ---                                                                          
#> 4851: Micropholis guyanensis    bakuman       Sapotaceae Micropholis guyanensis
#> 4852:    Lecythis persistens maho rouge    Lecythidaceae    Lecythis persistens
#> 4853:    Lecythis persistens maho rouge    Lecythidaceae    Lecythis persistens
#> 4854:         Eperua falcata       wapa         Fabaceae      Eperua    falcata
#> 4855:           Licania alba       koko Chrysobalanaceae     Licania       alba
#>       Subspecies Variety Voucher IdLevel Authority CommercialSp LifeForm
#>           <char>  <char>  <char>  <char>    <char>       <lgcl>   <char>
#>    1:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#>    2:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>    3:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#>    4:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>    5:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>   ---                                                                   
#> 4851:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4852:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4853:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#> 4854:       <NA>    <NA>    <NA>    <NA>      <NA>         TRUE     tree
#> 4855:       <NA>    <NA>    <NA>    <NA>      <NA>        FALSE     tree
#>       TreeFieldNum IdTree StemFieldNum        IdStem TreeLat TreeLon XTreeUTM
#>             <char> <char>       <char>        <char>   <num>   <num>    <num>
#>    1:           45 100665         <NA> 100665_1_auto      NA      NA       NA
#>    2:          148 100765         <NA> 100765_1_auto      NA      NA       NA
#>    3:           46 100666         <NA> 100666_1_auto      NA      NA       NA
#>    4:           49 100669         <NA> 100669_1_auto      NA      NA       NA
#>    5:            1 100621         <NA> 100621_1_auto      NA      NA       NA
#>   ---                                                                        
#> 4851:          922 101517         <NA> 101517_1_auto      NA      NA       NA
#> 4852:          884 101479         <NA> 101479_1_auto      NA      NA       NA
#> 4853:          887 101482         <NA> 101482_1_auto      NA      NA       NA
#> 4854:          896 101491         <NA> 101491_1_auto      NA      NA       NA
#> 4855:          905 101500         <NA> 101500_1_auto      NA      NA       NA
#>       YTreeUTM XTreePlot YTreePlot XTreeSubplot YTreeSubplot LifeStatus
#>          <num>     <num>     <num>        <num>        <num>     <lgcl>
#>    1:       NA        NA        NA           NA           NA       TRUE
#>    2:       NA        NA        NA           NA           NA       TRUE
#>    3:       NA        NA        NA           NA           NA       TRUE
#>    4:       NA        NA        NA           NA           NA       TRUE
#>    5:       NA        NA        NA           NA           NA       TRUE
#>   ---                                                                  
#> 4851:       NA        NA        NA           NA           NA       TRUE
#> 4852:       NA        NA        NA           NA           NA       TRUE
#> 4853:       NA        NA        NA           NA           NA       TRUE
#> 4854:       NA        NA        NA           NA           NA       TRUE
#> 4855:       NA        NA        NA           NA           NA       TRUE
#>       DeadStatus Diameter    BD  Circ BCirc   HOM    POM  BHOM   BPOM
#>           <lgcl>    <num> <num> <num> <num> <num> <char> <num> <char>
#>    1:         NA    53.16    NA 167.0    NA   1.3   <NA>    NA   <NA>
#>    2:         NA    14.32    NA  45.0    NA   1.3   <NA>    NA   <NA>
#>    3:         NA    19.89    NA  62.5    NA   1.3   <NA>    NA   <NA>
#>    4:         NA    41.38    NA 130.0    NA   1.3   <NA>    NA   <NA>
#>    5:         NA    13.37    NA  42.0    NA   1.3   <NA>    NA   <NA>
#>   ---                                                                
#> 4851:         NA    22.60    NA  71.0    NA   1.3   <NA>    NA   <NA>
#> 4852:         NA    16.39    NA  51.5    NA   1.3   <NA>    NA   <NA>
#> 4853:         NA    20.85    NA  65.5    NA   1.3   <NA>    NA   <NA>
#> 4854:         NA    29.60    NA  93.0    NA   1.3   <NA>    NA   <NA>
#> 4855:         NA    27.53    NA  86.5    NA   1.3   <NA>    NA   <NA>
#>       TreeHeight TreeFieldNumOriginal IdTreeOriginal CommercialSpOriginal
#>            <num>                <num>          <int>               <lgcl>
#>    1:         NA                   45         100665                 TRUE
#>    2:         NA                  148         100765                FALSE
#>    3:         NA                   46         100666                 TRUE
#>    4:         NA                   49         100669                FALSE
#>    5:         NA                    1         100621                FALSE
#>   ---                                                                    
#> 4851:         NA                  922         101517                FALSE
#> 4852:         NA                  884         101479                FALSE
#> 4853:         NA                  887         101482                FALSE
#> 4854:         NA                  896         101491                 TRUE
#> 4855:         NA                  905         101500                FALSE
#>       DateOriginal LifeStatusOriginal
#>             <char>             <lgcl>
#>    1:   2016-09-14               TRUE
#>    2:   2016-09-14               TRUE
#>    3:   2016-09-14               TRUE
#>    4:   2016-09-14               TRUE
#>    5:   2016-09-14               TRUE
#>   ---                                
#> 4851:   2020-06-04               TRUE
#> 4852:   2020-06-04               TRUE
#> 4853:   2020-06-04               TRUE
#> 4854:   2020-06-04               TRUE
#> 4855:   2020-06-04               TRUE


Rslt <- StatusCorrection(TestData[IdTree %in% selection])


StatusCorrectionPlot(Rslt)
#> [1] 1
```
