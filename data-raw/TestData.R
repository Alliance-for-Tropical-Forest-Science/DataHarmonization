## code to prepare `TestData` dataset goes here

#### Packages libraries ####
library(DataHarmonization)
library(data.table)


#### Import data ####

data(ParacouSubsetFormated) # import data
Data = ParacouSubsetFormated
## data.frame to data.table
setDT(Data) # with "set" "<-" is not necessary

nError <- 1 # number of error to add for each case


#### Tree size = 0 ####
modif <- Data[, .I[sample(.N, nError)]] # .I = seq_len(nrow(Data)), .N = nrows in the group -> sample 2 rows number in Data
Data[modif, Diameter := 0]
# Data[modif] # to check

#### Missing coordinates ####
modif <- Data[, .I[sample(.N, nError)]] # .I = seq_len(nrow(Data)), .N = nrows in the group -> sample 2 rows number in Data
Data[modif, XTreeUTM := NA]
Data[modif, YTreeUTM := NA]
# Data[modif] # to check

#### Other missing values ####
Vars <- c("PlotArea", "Plot", "Subplot", "Year", "TreeFieldNum", "IdTree",
          "Diameter", "LifeStatus", "POM", "Family", "Genus", "Species",
          "VernName", "ScientificName", "CommercialSp")

modifs <- c()

for (v in 1:length(Vars)) {

  modif <- Data[, .I[sample(.N, nError)]] # .I = seq_len(nrow(Data)), .N = nrows in the group -> sample 2 rows number in Data
  Data[modif, Vars[v] := NA]

  modifs <- c(modifs, modif)

}
# Data[modifs] # to check


#### Size with bad precision (pas .0 ou .5) ####

wrong <- c(0.2, 0.3, 0.569, 0.8)
modif <- Data[, .I[sample(.N, nError)]]
Data[modif, Diameter := Diameter + sample(wrong,1)]
# Data[modif] # to check


#### Resurrected tree ####

Last_census <- Data[Year == 2020]
Previous_census <- Data[Year == 2019]

# See if a resurrected tree already exists
MortPrev <- Previous_census[LifeStatus == 0 & IdTree %in% Last_census[LifeStatus == 1, IdTree], IdTree]
# dead in 2019, alive in 2020

Previous_census[IdTree == 101410] # dead
Last_census[IdTree == 101410] # alive


#### Duplicated TreeFieldNum in plot-subplot association ####

modif <- Data[, .I[sample(.N, nError)]] # 1 row to change
duplicatedFieldNum <- Data[!(row.names(Data)) %in% modif & # all rows != modif
                             Plot == Data[modif, Plot] & # same plot as modif
                             Subplot == Data[modif, Subplot], # same subplot as modif
                           sample(TreeFieldNum,1)] # 1 TreeFieldNum to duplicate

Data[modif, TreeFieldNum := duplicatedFieldNum] # on the row to modif, we duplicate the TreeFieldNum
# Data[TreeFieldNum == duplicatedFieldNum] # to check

#### Duplicated IdTree in a census ####

idModif <- Last_census[, sample(IdTree, nError)] # selectionner 1 IdTree (de 2019) à modifier

duplicatedID <- Last_census[!(IdTree %in% idModif), sample(IdTree, 1)] # select 1 (2019) IdTree != modif to duplicate

Data[IdTree %in% idModif, IdTree := duplicatedID] # we duplicate the IdTree on the previous selected IdTree

Data[Year == 2020 & IdTree == duplicatedID] # to check

#### Unseen tree but alive tree after ####

#### Abnormal growth ####
#### Abnormal recruit ####

#### Duplicated row ####
AddR <- Data[1] # the rows to copy

Data <- rbindlist(list(Data, AddR)) # add rows

#### Save this test data in the package ####
TestData <- Data

usethis::use_data(TestData, overwrite = TRUE)


## For TestData.Rmd  run next two line of code and paste in the item section of R/TestData.R
x <- read.csv("inst/app/data/interactive_items.csv")
idx = names(TestData) %in% x$ItemID
write.csv(
  paste0("#'   \\item{", names(TestData)[idx], ifelse(paste0(names(TestData)[idx], "Original") %in% names(TestData), paste0(", ", names(TestData)[idx], "Original"), ""), "}{", x$Description[match(names(TestData)[idx], x$ItemID)], "}"), "clipboard",
  quote = F, row.names = F)

