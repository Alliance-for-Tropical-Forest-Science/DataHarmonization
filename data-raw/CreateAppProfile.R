# get the interactive items
xall <- read.csv("inst/app/data/interactive_items.csv")
x <- xall[xall$Activate, ]

# create the list

AppProfile <- lapply(1:nrow(x), function(i) x[i, "AppDefaultProfile"])
AppProfile <- setNames(AppProfile, x$ItemID)

# add AllCodes
AppProfile$AllCodes <- data.frame(Column = "You have not selected columns for codes",
                                  Value = "You have not selected columns for codes",
                                  Definition = "You have not selected columns for codes")


# save
saveRDS(AppProfile, "inst/app/data/AppProfile.rds")
