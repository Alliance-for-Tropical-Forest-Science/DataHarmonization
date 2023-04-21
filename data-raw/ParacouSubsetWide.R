## code to prepare `ParacouSubsetWide` dataset goes here

#### Packages libraries ####
library(DataHarmonization)
library(data.table)

#### Import data ####
data(ParacouSubset)

#### lONG TO WIDE FORMAT ####
ParacouSubsetWide <- dcast(ParacouSubset, idTree ~ CensusYear, value.var = "Circ")
OtherCols <- copy(ParacouSubset)
OtherCols[, c("CensusYear", "CensusDate", "CensusDateCertainty",
              "Circ", "CircCorr", "CorrCode",
              "CodeAlive", "MeasCode") := NULL]
ParacouSubsetWide <- unique(merge(ParacouSubsetWide, OtherCols, by = "idTree")) #1000 ind -> 1000 rows

#### Circ: cm -> m for the exemple ####
ParacouSubsetWide[, Circ := Circ/100]


#### Save this data in the package ####
usethis::use_data(ParacouSubsetWide, overwrite = TRUE)
