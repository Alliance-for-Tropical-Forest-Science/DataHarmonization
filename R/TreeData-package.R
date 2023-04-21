#' DataHarmonization-package
#'
#' Forest Inventories Harmonization & Correction
#'
#' @name DataHarmonization
#' @docType package
#'
#' @section DataHarmonization functions:
#' RequiredFormat
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
### quiets concerns of R CMD check "no visible binding for global variables"
utils::globalVariables(c("Site", "Plot", "Subplot", "PlotArea", "SubplotArea", "PlotSubNum",
                         "XTreeUTM", "YTreeUTM", "XTreePlot", "YTreePlot", "XTreeSubplot", "YTreeSubplot",

                         "CensusYear", "Date", "CensusDate", "Month", "Day", "IdCensus",
                         "Year", "Time", "DateOriginal", "CensusDateOriginal", "SitYearID",

                         "IdTree", "IdStem", "StemFieldNum", "TreeFieldNum", "NewIdTree",

                         "Circ", "Diameter", "MinDBH", "TreeHeight",
                         "POM", "HOM",


                         "Family", "VernName", "Genus", "Species", "Subspecies", "ScientificName",


                         "LifeStatus", "CommercialSp", "LifeStatusOriginal", "CommercialSpOriginal",



                         "BD", "BCirc", "BPOM", "BHOM", "site",

                         ".", ".N", ".SD", ".NATURAL", "..ColumnsToReturn", "..ByCols",
                         "rn", "value", "LifeForm",

                         "IDYear", "V1", "V2", "MaxHOM", "MaxDBH", "MaxDate", "POMChange",

                         "LifeStatusCor", 'LifeStatus_DataHarmonizationCor', "Comment_DataHarmonization",
                         "CorrectedRecruit",
                         "DBHCor", "Diameter_DataHarmonizationCor", "DiameterCorrectionMeth_DataHarmonization", "Cresc",
                         "HOM_DataHarmonizationCor", "POM_DataHarmonizationCor",
                         "GenusCor", "SpeciesCor", "ScientificNameCor", "FamilyCor", "VernNameCor",
                         "Genus_DataHarmonizationCor", "Species_DataHarmonizationCor", "ScientificName_DataHarmonizationCor", "Family_DataHarmonizationCor", "VernName_DataHarmonizationCor",
                         "New.Genus", "New.Species", "Taxonomic.status", "Typo", "family",
                         "Taxon", "GenspFamily", "taxonomicStatus", "spec.name", "scientificName",
                         "Old.status", "spec.name.ORIG",
                         "Variety", "..Vars", "..V", "..keep", ".I", "Overall_score_returned_by_TNRS",
                         "StatusCorrectionMeth_DataHarmonization", "StatusCorrectionMeth_DataHarmonization",
                         "BotaCorSource",
                         "BotanicalCorrectionSource", "FamilyCorSource",
                         "raised", "InvariantColumns"
))

## usethis namespace: end
NULL
