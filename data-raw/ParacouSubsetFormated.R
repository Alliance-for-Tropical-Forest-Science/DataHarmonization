## code to prepare `ParacouSubsetFormated` dataset goes here
data("ParacouSubset")
data("ParacouProfile")


# format to App's standards
ParacouSubsetFormated <- RequiredFormat(
  ParacouSubset,
  input = ParacouProfile)

usethis::use_data(ParacouSubsetFormated, overwrite = TRUE)
# usethis::use_R(ParacouSubsetFormated, overwrite = TRUE)



## For ParacouProfileFormated.Rmd  run next two line of code and paste in the item section of R/ParacouProfile.R
x <- read.csv("inst/app/data/interactive_items.csv")
idx = names(ParacouSubsetFormated) %in% x$ItemID
write.csv(
  paste0("#'   \\item{", names(ParacouSubsetFormated)[idx], ifelse(paste0(names(ParacouSubsetFormated)[idx], "Original") %in% names(ParacouSubsetFormated), paste0(", ", names(ParacouSubsetFormated)[idx], "Original"), ""), "}{", x$Description[match(names(ParacouSubsetFormated)[idx], x$ItemID)], "}"), "clipboard",
  quote = F, row.names = F)
