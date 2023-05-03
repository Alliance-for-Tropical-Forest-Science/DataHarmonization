## code to prepare `SCBISubsetFormated` dataset goes here

SCBISubsetFormated <- readRDS("C:/Users/herrmannV/Dropbox (Smithsonian)/GitHub/VincyaneBadouard/demo/scbi_formated.rds")

SCBISubsetFormated[c(1, 2, 15, 389), c("LifeStatus", "LifeStatusOriginal")] <- NA # making some NA's
SCBISubsetFormated[151006,  c("LifeStatus", "LifeStatusOriginal")] <- FALSE #making some dead
SCBISubsetFormated  <- SCBISubsetFormated[-c(258,3243, 567)] # removing some records

SCBISubsetFormated[IdStem %in% 58921 & IdCensus %in% 2, LifeStatus := FALSE ] # this is to create a case of "Tree can't be dead before bein alive
SCBISubsetFormated <- SCBISubsetFormated[IdStem %in% c("67276", "1304", "65905", "58921", "1", "10012", "11012", "66114", "31258", "10032", "3631"), ]

usethis::use_data(SCBISubsetFormated, overwrite = TRUE)



## For SCBISubsetFormated.Rmd  run next two line of code and paste in the item section of R/ParacouProfile.R
x <- read.csv("inst/app/data/interactive_items.csv")
idx = names(SCBISubsetFormated) %in% x$ItemID
write.csv(
  paste0("#'   \\item{", names(SCBISubsetFormated)[idx], ifelse(paste0(names(SCBISubsetFormated)[idx], "Original") %in% names(SCBISubsetFormated), paste0(", ", names(SCBISubsetFormated)[idx], "Original"), ""), "}{", x$Description[match(names(SCBISubsetFormated)[idx], x$ItemID)], "}"), "clipboard",
  quote = F, row.names = F)
