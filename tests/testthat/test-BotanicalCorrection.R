test_that("BotanicalCorrection", {

  # options(warn = 2) # trace warning

  suppressWarnings(library(data.table))

  # WFO Dataset -----------------------------------------------------------------------------------------------------------
  # load("D:/VSC TmFO/Data/WFO_Backbone.rda")
  # WFOdataSubset <- WFO_Backbone[scientificName %in% c("Dicorynia guianensis", "Licania alba",
  #                                                     "Eperua falcata", "Eperua grandiflora", "Protium opacum")|
  #                                 genus %in% c("Eschweilera", "Indothuidium", "Tovomita")]
  #
  # WFOdataSubset[, c("localID", "subfamily", "tribe", "subtribe", "subgenus",
  #                   "originalNameUsageID", "taxonRemarks", "source", "majorGroup", "tplId"):= NULL]
  #
  # usethis::use_data(WFOdataSubset, overwrite = TRUE)
#
#   data(WFOdataSubset)

  # Create test data ------------------------------------------------------------------------------------------------------

  # Fabaceae Dicorynia guianensis (angelique) "a"
  # Lecythidaceae Eschweilera sagotiana (maho noir) "b"
  # Chrysobalanaceae Licania alba (koko) "c"
  # Fabaceae Eperua falcata (wapa) "d"
  Data <- data.table(Site = "Nowhere",
                     IdTree = c(rep("a", 4), rep("b", 4), rep("c", 4), rep("d", 4), rep("e", 1), rep("f", 1), rep("g", 1), rep("h", 1)), # 8 ind
                     Year = c(rep(c(2000:2003), 4), rep(2000, 4)) # 4 years each
  )
  Data <- Data[order(IdTree, Year)]
  Data[, Family := c(rep("Fabaceae", 4), rep("Lecythidaceae", 4), rep("Chrysobalanaceae", 4), rep("Fabaceae", 4), rep("Sapindaceae", 1), rep("Clusiaceae", 1), rep("Burseraceae", 1), rep("Clusiaceae", 1))]
  Data[, Genus := c(rep("Dicorynia", 4), rep("Eschweilera", 4), rep("Licania", 4), rep("EperuaFabaceae", 4), rep("Indet.Sapindaceae", 1), rep("Tovomita", 1), rep("Protium", 1), rep("Tovomita", 1))]
  Data[, Species := c(rep("guianensis", 4), rep("sagotiana", 4), rep("alba", 4), rep("falcata", 4), rep("Indet.", 1), rep("sp.5-CAY", 1), rep("opacum_subsp.rabelianum", 1), rep("Indet.", 1))]
  Data[, VernName := c(rep("angelique", 4), rep("maho noir", 4), rep("koko", 4), rep("wapa", 4), rep(NA, 4))]
  Data[, Subspecies := NA_character_]

  # Create errors ---------------------------------------------------------------------------------------------------------

  ## Missing value in  Family, ScientificName/Genus, species, VernName
  Data[IdTree %in% "d" & Year %in% 2000, ("Family") := NA_character_]
  Data[IdTree %in% "b" & Year %in% 2000, ("Genus") := NA_character_]
  Data[IdTree %in% "b", ("Family") := NA_character_]
  Data[IdTree %in% "c" & Year %in% 2000, ("Species") := NA_character_]
  Data[IdTree %in% "a" & Year %in% 2000, ("VernName") := NA_character_]
  Data[IdTree %in% "d" & Year %in% 2000, ("VernName") := NA_character_]

  ## Special characters
  Data[IdTree %in% "a", ("Family") := "Fabacé"] # good answer: "Fabaceae"
  Data[IdTree %in% "c" & Year %in% 2003, ("Genus") := "Licanï_a"] # good answer: "Licania"

  ## Variant botanical informations per IdTree
  Data[IdTree %in% "d" & Year %in% 2002, ("Species") := "grandi!flora"] # good answer: "falcata"

  ## Family name in the genus/species columns
  Data[IdTree %in% "b", ("Species") := "Lecythidaceae"] # good answer: "sagotiana"

  ## Family & Scientific names unmatch
  Data[IdTree %in% "c", ("Family") := "Lecythidaceae"] # good answer: "Chrysobalanaceae"

  ## Scientific & vernacular names unmatch
  Data[IdTree %in% "d" & Year %in% 2001, ("VernName") := "leaf"]  # good answer: "wapa"

  ## Old scientific name (A trouver)

  Data[, ScientificName := paste(Genus, Species)]

  # Create bad test data --------------------------------------------------------------------------------------------------
  MatrixData <- as.matrix(Data)

  # Check the function argument -------------------------------------------------------------------------------------------
  expect_error(BotanicalCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  # Check the function work -----------------------------------------------------------------------------------------------

  ## Detect Only: no correction, only comments ----------------------------------------------------------------------------
  DataCor <- suppressWarnings(BotanicalCorrection(Data))

  DataCor <- DataCor$Data
  LOG <- DataCor$log

  OriginalColumns <- names(Data)


    # -aceae in Genus or Species
    expect_true(all(is.na(DataCor[grepl("aceae", Data$Genus), Genus_DataHarmonizationCor] )))
    expect_true(all(is.na(DataCor[grepl("aceae", Data$Species), Species_DataHarmonizationCor] )))

    # Variant botanical info per IdTree (A FAIRE)
    VarIdTree <- unique(Data[, .(IdTree, Family, Genus, Species, Subspecies)])[duplicated(IdTree), IdTree]

    expect_true(all(grepl("Incongruent taxonomic information within Site x IdTree combinations.",
                          DataCor[IdTree %in% VarIdTree, Comment_DataHarmonization])))


    # make sure Genus ans species are NA if ScientificName_DataHarmonizationCor
    expect_true(all(is.na(DataCor$ScientificName_DataHarmonizationCor) == ( is.na(DataCor$Genus_DataHarmonizationCor) & is.na(DataCor$Species_DataHarmonizationCor)) ))

    # No "aceae" in Genus or Species column --------------------------------------------------------------------------------
    expect_true(!any(grepl("aceae", DataCor$ScientificName_DataHarmonizationCor)))


    # All Family names with -aceae
    expect_true(all(grepl("aceae", na.omit(DataCor$Family_DataHarmonizationCor))))

    # No special character in Genus and Family columns ---------------------------------------------------------------------
    expect_true(!any(grepl("[[:punct:]]", DataCor$Genus_DataHarmonizationCor)))
    expect_true(!any(grepl("[[:punct:]]", DataCor$Family_DataHarmonizationCor)))

    # No space or underscore in Species column
    expect_true(!any(grepl("[[:blank:]]", DataCor$Species_DataHarmonizationCor)))
    expect_true(!any(grepl("_", DataCor$Species_DataHarmonizationCor)))

    # No Indet in Family, no subsp in Species
    expect_true(!any(grepl("Indet", DataCor$Family_DataHarmonizationCor)))
    expect_true(!any(grepl("subsp", DataCor$Species_DataHarmonizationCor)))

    # No adding rows
    expect_true( nrow(DataCor) == nrow(Data) )


})

# Detect Only: no correction, only comments
# no "aceae" in Genus or Species column
# Family if Genus
# no special character in Genus and Family columns

# Comment column ?
# Source column ?
