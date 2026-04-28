# ParacouSubsetWide

Subset of tree inventories of Paracou research station (French Guiana),
in wide format (1 col per census).

- Plot 6

- SubPlot 1

- 2016-2020

- 5 last censuses

- 1000 random individuals

- 4 dummy sub-sub-squares created for the package to be use as plots
  Dataset extracted from the Guyafor database.

## Usage

``` r
ParacouSubsetWide
```

## Format

A tibble with 1000 rows and 31 variables:

- 2016-2020:

  Circumference of the tree at 1.30m above ground (DBH level), in cm for
  each census year between 2016 and 2020 (5)(numeric)

- Forest:

  Forest name (character)

- Plot:

  Plot number (character)

- PlotArea:

  Plot area, in hectare (numeric)

- SubPlot:

  Subplot number (integer)

- SubSubPlot:

  Dummy sub-sub-plots created for the package (integer)

- TreeFieldNum:

  Tree unique identifiers within the subplot (matching the tag number in
  the field) (numeric)

- idTree:

  Unique tree identifier from the database (integer)

- Protocole:

  Protocole name (character)

- Xfield:

  Euclidean position of the tree relative to the Southwestern corner of
  the subplot, in m (numeric)

- Yfield:

  Euclidean position of the tree relative to the Southwestern corner of
  the subplot, in m (numeric)

- Xutm:

  Coordinate X for the tree in UTM 22 N (EPSG: 32 622) (numeric)

- Yutm:

  Coordinate Y for the tree in UTM 22 N (EPSG: 32 622) (numeric)

- UTMZone:

  UTM Zone (integer)

- Lat:

  Tree latitude (WGS 84, EPSG : 4326) (numeric)

- Lon:

  Tree longitude (WGS 84, EPSG : 4326) (numeric)

- Family:

  Botanical family (character)

- Genus:

  Botanical genus (character)

- Species:

  Botanical species (character)

- BotaSource:

  Source of botanical name (character)

- BotaCertainty:

  Level of certainty for the botanical identification (numeric)

- idVern:

  Unique vernacular identifier from the database (numeric)

- VernName:

  Vernacular name (character)

- CommercialSp:

  Is the tree considered as a commercial species according to the list
  of species that were logged during the sylvicultural treatment in
  Paracou (TRUE) or not (FALSE) ? (logical)

- CodeAlive:

  Is the tree alive (TRUE) or dead (FALSE)? (logical)

- MeasCode:

  Information on the method for measuring the circumference or on the
  state of the tree (integer)

- Circ:

  Circumference of the tree at 1.30m above ground (DBH level), in m
  (numeric)

## Source

<http://paracou.cirad.fr> ;
<https://paracoudata.cirad.fr/public/pdf/Paracou_data_dictionnary.pdf>
