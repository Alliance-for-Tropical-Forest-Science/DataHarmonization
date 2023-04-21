## code to prepare `ForestGeoSubsetFormated` dataset goes here
data("ForestGeoSubset")
data("ForestGeoProfile")

ForestGeoSubsetFormated <- RequiredFormat(
  ForestGeoSubset,
  input = ForestGeoProfile)

usethis::use_data(ForestGeoSubsetFormated, overwrite = TRUE)

## For ParacouProfileFormated.Rmd  run next two line of code and paste in the item section of R/ParacouProfile.R
x <- read.csv("inst/app/data/interactive_items.csv")
idx = names(ForestGeoSubsetFormated) %in% x$ItemID
write.csv(
  paste0("#'   \\item{", names(ForestGeoSubsetFormated)[idx], ifelse(paste0(names(ForestGeoSubsetFormated)[idx], "Original") %in% names(ForestGeoSubsetFormated), paste0(", ", names(ForestGeoSubsetFormated)[idx], "Original"), ""), "}{", x$Description[match(names(ForestGeoSubsetFormated)[idx], x$ItemID)], "}"), "clipboard",
  quote = F, row.names = F)
