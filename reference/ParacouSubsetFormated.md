# ParacouSubsetFormated

This is the standardized, formated version of ParacouSubset (see
help(ParacouSubset))

## Usage

``` r
ParacouSubsetFormated
```

## Format

A tibble

- MinDBH:

  The minimum diameter of trees included in the inventory

- IdCensus:

  Census identifier

- Year:

  Year of measurement

- Month:

  Month of measurement

- Day:

  Day of measurement

- Date, DateOriginal:

  Date of measurement

- Site:

  Site level name

- Cluster:

  Cluster ID or name (This was created to accommodate ForestPlots
  network's cluster, which groups similar plots and is used by their
  data packages to create local allometries. Note that it is useful to
  also keep Site (PlotName) so as users able to check for duplicate
  datasets from multiple networks).

- Plot:

  Plot level name

- PlotArea:

  Size of the plot

- PlotElevation:

  plot's elevation in meters above sea level

- Subplot:

  Subplot level name

- SubplotArea:

  Size of the plot

- PlotViewID:

  additional plot protocol identifier useful to indicate if the plot
  protocol has changed and/or different soil type/disturbance... (This
  was created to accommodate ForestPlots network).

- PlotLat:

  Plot's latitude in degrees

- PlotLon:

  Plot's longitude in degrees

- XPlotUTM:

  Plot's X UTM coordinates

- YPlotUTM:

  Plot's Y UTM coordinates

- SubplotLat:

  Subplot's latitude in degrees

- SubplotLon:

  Subplot's longitude in degrees

- XSubplotUTM:

  Subplot's X UTM coordinates

- YSubplotUTM:

  Subplot's Y UTM coordinates

- ScientificName:

  Scientific name

- VernName:

  Vernacular name

- Family:

  Family name

- Genus:

  Genus name

- Species:

  Species name

- Subspecies:

  Subspecies name

- Variety:

  Variety name

- Voucher:

  Voucher code for the species

- IdLevel:

  Deepest taxonomic level for which full identification is known

- Authority:

  Taxonomic authority for the ID level

- CommercialSp, CommercialSpOriginal:

  Logical: TRUE if species is considered commercial, FALSE if not

- LifeForm:

  Life form

- TreeFieldNum, TreeFieldNumOriginal:

  Tree unique identifiers, matching the tag number in the field.

- IdTree, IdTreeOriginal:

  Tree unique identifiers. This was automatically generated if it was
  not provided in the input data (using plot and tree tag information).

- StemFieldNum:

  Stem identifiers within the tree.

- IdStem:

  Stem unique identifiers. This was automatically generated if it was
  not provided in the input data (using plot tree and stem tag
  information).

- TreeLat:

  Tree Latitude in degrees

- TreeLon:

  Tree Longitude in degrees

- XTreeUTM:

  Tree X UTM coordinates

- YTreeUTM:

  Tree Y UTM coordinates

- XTreePlot:

  Tree X euclidean position in plot

- YTreePlot:

  Tree Y euclidean position in plot

- XTreeSubplot:

  Tree X euclidean position in subplot

- YTreeSubplot:

  Tree Y euclidean position in subplot

- LifeStatus, LifeStatusOriginal:

  Logical: TRUE if tree was alive, FALSE if it was dead

- DeadStatus:

  \-

- Diameter:

  Tree Diameter

- BD:

  Basal diameter

- Circ:

  Circumference

- BCirc:

  Basal circumference

- HOM:

  Height of measurement

- POM:

  Code for point of measurement of tree diameter

- BHOM:

  Height of measurement of basal diameter

- BPOM:

  Code for point of measurement of basal diameter

- TreeHeight:

  Tree Height
