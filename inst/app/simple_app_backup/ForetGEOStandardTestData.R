## code to prepare `ForetGEOStandardTestData` dataset goes here

spptable <- read.csv(paste0(dirname(getwd()), "/demo/scbi.spptable.csv"))
stem1 <- read.csv(paste0(dirname(getwd()), "/demo/scbi.stem1.csv"))
stem2 <- read.csv(paste0(dirname(getwd()), "/demo/scbi.stem2.csv"))
stem3 <- read.csv(paste0(dirname(getwd()), "/demo/scbi.stem3.csv"))


StackedTables <- do.call(rbind, list(stem1, stem2, stem3))

MergedTables <- merge(StackedTables, spptable, by.x="sp", by.y="sp", all.x=TRUE)

OneTable <- MergedTables # no need to tidy

# keep only a ubset for this package

ForetGEOStandardTestData <- OneTable[OneTable$dbh %in% "NULL" | rownames(OneTable) %in% sample(rownames(OneTable), size= 10000), ] # make sure we keep dbh = NULL, because that is the one causing issues in RequiredFormat function and I want to trouble shoot



usethis::use_data(ForetGEOStandardTestData, overwrite = TRUE)




## For ForetGEOStandardTestData.Rmd  run next line of code and paste in the item section of R/ForetGEOStandardTestData.R
write.csv(
  paste0("#'   \\item{", names(ForetGEOStandardTestData), "}{Value or column name in data set @ParacouSubset (", ParacouProfile, ") corresponding to ", x$Label[match(names(ParacouProfile), x$ItemID)], "}"), "clipboard",
  quote = F, row.names = F)

