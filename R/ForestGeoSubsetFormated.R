#' ForestGeoSubsetFormated
#'
#' This is the standardized, formated version of ForestGeoSubset (see help(ForestGeoSubset))
#'
#' @format A tibble
#' \describe{
#'   \item{Site}{Site level name}
#'   \item{Plot}{Plot level name}
#'   \item{SubPlot}{SubPlot level name}
#'   \item{PlotArea}{Size of the plot}
#'   \item{SubPlotArea}{Size of the plot}
#'   \item{IdCensus}{Year of measurement}
#'   \item{Year}{Year of measurement}
#'   \item{Date, DateOriginal}{Date of measurement}
#'   \item{TreeFieldNum}{Tree unique identifiers, matching the tag number in the field.}
#'   \item{IdTree}{Tree unique identifiers. This was automatically generated if it was not provided in the input data (using plot and tree tag information).}
#'   \item{IdStem}{Stem unique identifier}
#'   \item{IdMeasure}{measure unique identifier if you had more than one measure per stem per year}
#'   \item{LifeStatus, LifeStatusOriginal}{Logical: TRUE if tree was alive, FALSE if it was dead}
#'   \item{Diameter}{Tree Diameter}
#'   \item{BD}{Basal diameter}
#'   \item{Circ}{Circumference}
#'   \item{BCirc}{Basal circumference}
#'   \item{HOM}{Height of measurement}
#'   \item{POM}{Code for point of measurement of tree diameter}
#'   \item{BHOM}{Height of measurement of basal diameter}
#'   \item{BPOM}{Code for point of measurement of basal diameter}
#'   \item{TreeHeight}{Tree Height}
#'   \item{Lat}{Latitude}
#'   \item{Lon}{Longitude}
#'   \item{Xutm}{X UTM coordinates}
#'   \item{Yutm}{Y UTM coordinats}
#'   \item{Xplot}{Tree X euclidean position in plot}
#'   \item{Yplot}{Tree Y euclidean position in plot}
#'   \item{Xsubplot}{Tree X euclidean position in subplot}
#'   \item{Ysubplot}{Tree Y euclidean position in subplot}
#'   \item{ScientificName}{Scientific name}
#'   \item{VernName}{Vernacular name}
#'   \item{Family}{Family name}
#'   \item{Genus}{Genus name}
#'   \item{Species}{Species name}
#'   \item{Subspecies}{Subspecies name}
#'   \item{Variety}{Variety name}
#'   \item{Voucher}{Voucher code for the species}
#'   \item{IdLevel}{Deepest taxonomic level for which full identification is known}
#'   \item{Authority}{Taxonomic authority for the ID level}
#'   \item{CommercialSp}{Logical: TRUE if species is considered commercial, FALSE if not}
#'   \item{LifeForm}{Life form}
#'   \item{DateOriginal}{Date as it was given in the input table, without any correction or transformation}
#'   \item{LifeStatusOriginal}{LifeStatus as it was given in the input table, without any correction or transformation}
#'   ...
#' }

"ForestGeoSubsetFormated"
