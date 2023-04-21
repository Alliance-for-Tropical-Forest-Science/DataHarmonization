test_that("RequiredFormat", {

  # WITH ParacouSubset ####
  data(ParacouSubset)
  data(ParacouProfile)
  Data <- ParacouSubset
  input <- ParacouProfile

  expect_warning(
    DataFormated <- RequiredFormat(Data, input, MeasLevel = "Tree" ),
    "You did not provide a Census ID column. We will use year as census ID.
MinDBH was calculated.
You did not specify a subplot area.")

  # make sure no IdTree is NA
  expect_false(any(is.na(DataFormated$IdTree)))

  # make sure no IdStem is NA
  expect_false(any(is.na(DataFormated$IdStem)))

  # make sure all IdStem has _auto
  expect_true(all(grepl("_auto", DataFormated$IdStem)))

  # make sure Diameter or Circ units were converted correctly
  if(!input$Diameter %in% "none")   expect_equal(DataFormated$Diameter, Data[,input$Diameter] * switch(input$DiameterUnitMan , mm = 0.1, cm = 1, dm = 10, m = 100))
  if(!input$Circ %in% "none")   expect_equal(DataFormated$Circ, Data[,input$Circ] * switch(input$CircUnitMan, mm = 0.1, cm = 1, dm = 10, m = 100), tolerance = 0.01)


  # make sure Diameter is calculated correcly if only Circ is given
  if(input$Diameter %in% "none" & !input$Circ %in% "none") expect_equal(DataFormated$Diameter, round(DataFormated$Circ / pi, 2))


  # make sure date format is handled correctly (all are in yyyy-mm-dd format, and if not it is NA)
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)) |  all(is.na(DataFormated$Date[!grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)])))


  # expect ScientificName to be filled
  if(input$ScientificName %in% "none" & !input$Genus %in% "none") expect_true(all(!is.na(DataFormated$ScientificName[!is.na(DataFormated$Genus)])))


  # cAREFULL, EDITING DATA OR INPUT AFTER THIS LINE #
  input$MinDBHMan = 1 # adding this so we don't get warnings
  input$SubplotAreaMan = 0.04 # adding this so we don't get warnings
  input$IdStem = input$IdTree # adding this so we don't get warnings
  input$IdCensus = input$Year # adding this so we don't get warnings

  # expect error is size units are not correct
  # input$DiameterUnitMan = input$CircUnitMan = "centimeter"

  # expect_error(RequiredFormat(Data, input ), "Your tree size units are not one of: mm, cm, dm, m")

  input$DiameterUnitMan <- ParacouProfile$DiameterUnitMan
  input$CircUnitMan <- ParacouProfile$CircUnitMan

  expect_error(expect_error(RequiredFormat(Data, input, MeasLevel = "Tree" ))) # don't expect the error anymore, but still warning

  # expect warning if no IdTree and no Tree Tag
  input$IdTree = input$TreeFieldNum = "none"
  expect_warning(RequiredFormat(Data, input, MeasLevel = "Tree" ), "You are missing treeIDs")

  input$IdTree <- ParacouProfile$IdTree
  input$TreeFieldNum <- ParacouProfile$TreeFieldNum
  expect_error(expect_warning(RequiredFormat(Data, input ))) # don't expect the warning anymore

  # expect IdTree to be have Tree Tag if IdTree is "none"
  input$IdTree =  "none"
  if( input$IdTree %in% "none" & !  input$TreeFieldNum %in% "none") expect_warning(expect_true(all(apply(RequiredFormat(Data, input, MeasLevel = "Tree"), 1, function(x) grepl(x["TreeFieldNum"] , x["IdTree"])))), "You are missing treeIDs")

  input$IdTree <- ParacouProfile$IdTree
  input$TreeFieldNum <- ParacouProfile$TreeFieldNum
  expect_error(expect_warning(RequiredFormat(Data, input, MeasLevel = "Tree" ))) # don't expect the warning anymore

  # expect IdTree to be filled with "auto" if is NA
  input$IdStem <- "IdStem"
  Data$IdStem <- Data$idTree
  Data[, input$IdTree][sample(10)] <- NA

  expect_warning(expect_true(all(grepl("_auto", RequiredFormat(Data, input, MeasLevel = "Tree" )$IdTree[is.na(Data[, input$IdTree])]))), "You are missing treeIDs")

  input$IdTree <- ParacouProfile$IdTree
  input$TreeFieldNum <- ParacouProfile$TreeFieldNum
  # expect_error(expect_warning(RequiredFormat(Data, input ))) # don't expect the warning anymore

  # expect IdTree to have Site, Plot and Subplot name, even when those are not in columns
  input$IdTree =  "none"
  expect_warning(expect_true(all(apply(RequiredFormat(Data, input, MeasLevel = "Tree" ), 1, function(x) {all(
    grepl(x["Site"] , x["IdTree"]) &
      grepl(x["Plot"] , x["IdTree"]) &
      grepl(x["Subplot"] , x["IdTree"]))}))), "You are missing treeIDs")

  input$Site <- "none"
  input$SiteMan = ""
  input$Plot <- "none"
  input$PlotMan = "BB"
  input$Subplot <- "none"
  input$SubplotMan = ""

  expect_true(all(apply(suppressWarnings(RequiredFormat(Data, input, MeasLevel = "Tree" )), 1, function(x) {all(
    grepl("SiteA" , x["IdTree"]) &
      grepl(x["Plot"] , x["IdTree"]) &
      grepl("SubplotA" , x["IdTree"]))})))

  # RequiredFormat(Data, input )$IdTree

  input$IdTree <- ParacouProfile$IdTree
  input$Site <- ParacouProfile$Site
  input$SiteMan <- ParacouProfile$SiteMan
  input$Plot <- ParacouProfile$Plot
  input$PlotMan <- ParacouProfile$PlotMan
  input$Subplot <- ParacouProfile$Subplot
  input$SubplotMan <- ParacouProfile$SubplotMan
  Data$idTree <- ParacouSubset$idTree




 # make sure measurement units gets converted correctly or throw error if units not selected
 Data$HOM <- 1.3
 input$HOM = "HOM"
 input$HOMUnitMan = "cm"
 Data$BCirc <- Data[, input$Circ]
 input$BCirc = "BCirc"
 input$BCircUnitMan = "cm"
 input$BD = "none"
 Data$BHOM <- 0.1
 input$BHOM = "BHOM"
 input$BHOMUnitMan = "cm"
 Data$TreeHeight = 20
 input$TreeHeight = "TreeHeight"
 input$TreeHeightUnitMan = "m"
 Data$XSubplot <- Data$Xfield
 Data$YSubplot <- Data$Yfield
 input$XTreeSubplot = "XSubplot"
 input$YTreeSubplot = "YSubplot"
 input$TreeSubplotUnitMan = "cm"


 appdir <- system.file(package = "DataHarmonization", "app")
 x <- read.csv(paste0(appdir, "/data/interactive_items.csv"))
 x <- x[x$Activate, ]
 StandardUnitTable <- do.call(rbind, lapply(grep("UnitMan", x$ItemID, value = T), function(i) {

   ItemID <-  sub("UnitMan", "", i)

   if(i %in%  c("TreeUTMUnitMan","TreePlotUnitMan", "TreeSubplotUnitMan")) ItemID <- paste0(c("X", "Y"), ItemID)


   data.frame(ItemID = ItemID,
              UnitMan = i,
              StandardUnit = x$Unit[match(ItemID, x$ItemID)]
   )

 }))


 # simple unit conversions

  for(i in c("mm", "cm", "dm", "m", "none")){

    for (u in c("cm", "m")) {
        for(j in StandardUnitTable$ItemID[StandardUnitTable$StandardUnit %in%  u]) {

          if(u %in% "cm") cf <- switch(i, mm = 0.1, cm = 1, dm = 10, m = 100)
          if(u %in% "m")  cf <- switch(i , mm = 0.001, cm = .01, dm = .10, m = 1)

      if(!input[[j]] %in% "none") {
        UnitItemID <- paste0(j, "UnitMan")

        if(grepl("^X|^Y", UnitItemID)) UnitItemID <- gsub("^X|^Y", "", UnitItemID)

        oi <-  input[[UnitItemID]]
        input[[UnitItemID]]= i

        if(i %in% "none") expect_error(RequiredFormat(Data, input, MeasLevel = "Tree" ), "is not recognized by udunits.") else expect_equal(RequiredFormat(Data, input, MeasLevel = "Tree" )[,(j),with = F][[1]], Data[,input[[j]]] * cf)

        input[[UnitItemID]] = oi # so that does not through an error anymore

      }

    }
    }

  }

 # raised unit conversions

 for(i in c("mm2", "cm2", "m2", "ha", "km2", "none")){

   for (u in c("cm2", "ha")) {

     for(j in StandardUnitTable$ItemID[StandardUnitTable$StandardUnit %in%  u]) {


       if(u %in% "cm2") cf = switch(i , mm2 = 0.01, cm2 = 1, m2 = 10000,ha = 100000000, km2 = 100000000)
       if(u %in% "ha") cf = switch(i , mm2 = 1/10000000000, cm2 = 1/100000000 , m2 = 1/10000, ha = 1, km2 = 100)

       if(!input[[j]] %in% "none") {

         oi <-  input[[paste0(j, "UnitMan")]]
         input[[paste0(j, "UnitMan")]]= i

         if(i %in% "none") expect_error(RequiredFormat(Data, input, MeasLevel = "Tree" ), "is not recognized by udunits") else expect_equal(RequiredFormat(Data, input, MeasLevel = "Tree" )[,(j),with = F][[1]], Data[,input[[j]]] * cf)
         input[[paste0(j, "UnitMan")]] = oi # so that does not through an error anymore

       }

     }
   }

 }

 # simple Quotient unit conversions
 for(i in c("individual/mm2", "individual/cm2", "individual/m2", "individual/ha", "individual/km2", "none")){

   for (u in c("individual/ha")) {

     for(j in StandardUnitTable$ItemID[StandardUnitTable$StandardUnit %in%  u]) {


       if(u %in% "individual/ha") cf = switch(i ,
                                               "individual/mm2" = 1/10000000000,
                                               "individual/cm2" = 1/100000000 ,
                                               "individual/m2" = 1/10000,
                                               "individual/ha" = 1,
                                               "individual/km2" = 100)


       if(!input[[j]] %in% "none") {

         oi <-  input[[paste0(j, "UnitMan")]]
         input[[paste0(j, "UnitMan")]]= i

         if(i %in% "none") expect_error(RequiredFormat(Data, input, MeasLevel = "Tree" ), "is not recognized by udunits") else expect_equal(RequiredFormat(Data, input, MeasLevel = "Tree" )[,(j),with = F][[1]], Data[,input[[j]]] * cf)
         input[[paste0(j, "UnitMan")]] = oi # so that does not through an error anymore

       }

     }
   }

 }

 # raised2 Quotient unit conversions
 for(i in c("mm2/m2", "cm2/m2", "m2/m2",
            "mm2/ha", "cm2/ha", "m2/ha",
            "mm2/km2", "cm2/km2", "m2/km2",
            "none"
            )){

   for (u in c("cm2/ha")) {

     for(j in StandardUnitTable$ItemID[StandardUnitTable$StandardUnit %in%  u]) {


       if(u %in% "cm2/ha") cf = switch(i ,
                                      "mm2/m2" = 0.01/10000,
                                      "cm2/m2" = 1/10000 ,
                                      "m2/m2" = 100/10000,
                                      "mm2/ha" = 0.01/1,
                                      "cm2/ha" = 1/1 ,
                                      "m2/ha" = 100/1,
                                      "mm2/km2" = 0.01/100,
                                      "cm2/km2" = 1/100 ,
                                      "m2/km2" = 100/100)


       if(!input[[j]] %in% "none") {

         oi <-  input[[paste0(j, "UnitMan")]]
         input[[paste0(j, "UnitMan")]]= i

         if(i %in% "none") expect_error(RequiredFormat(Data, input, MeasLevel = "Tree" ), "is not recognized by udunits") else expect_equal(RequiredFormat(Data, input, MeasLevel = "Tree" )[,(j),with = F][[1]], Data[,input[[j]]] * cf)
         input[[paste0(j, "UnitMan")]] = oi # so that does not through an error anymore

       }

     }
   }

 }

 # raised3 Quotient unit conversions
 for(i in c("mm3/m2", "cm3/m2", "m3/m2",
            "mm3/ha", "cm3/ha", "m3/ha",
            "mm3/km2", "cm3/km2", "m3/km2",
            "none"
 )){

   for (u in c("cm3/ha")) {

     for(j in StandardUnitTable$ItemID[StandardUnitTable$StandardUnit %in%  u]) {


       if(u %in% "cm3/ha")  cf = switch(i ,
                                        "mm3/m2" = 0.001/10000,
                                        "cm3/m2" = 1/10000 ,
                                        "m3/m2" = 1000000/10000,
                                        "mm3/ha" = 0.001/1,
                                        "cm3/ha" = 1/1 ,
                                        "m3/ha" = 1000000/1,
                                        "mm3/km2" = 0.001/100,
                                        "cm3/km2" = 1/100 ,
                                        "m3/km2" = 1000000/100)

       if(!input[[j]] %in% "none") {

         oi <-  input[[paste0(j, "UnitMan")]]
         input[[paste0(j, "UnitMan")]]= i

         if(i %in% "none") expect_error(RequiredFormat(Data, input, MeasLevel = "Tree" ), "is not recognized by udunits") else expect_equal(RequiredFormat(Data, input, MeasLevel = "Tree" )[,(j),with = F][[1]], Data[,input[[j]]] * cf)
         input[[paste0(j, "UnitMan")]] = oi # so that does not through an error anymore

       }

     }
   }

 }



 # put parameters back to what they were
 # input$HOM = ParacouProfile$HOM
 # input$BCirc = ParacouProfile$BCirc
 # input$BD = ParacouProfile$BD
 # input$BHOM = ParacouProfile$BHOM
 # input$TreeHeight = ParacouProfile$TreeHeight
 # input$HOMUnitMan = ParacouProfile$HOMUnitMan
 # input$BHOMUnitMan = ParacouProfile$BHOMUnitMan
 # input$CircUnitMan = ParacouProfile$CircUnitMan
 # input$BDUnitMan = ParacouProfile$BDUnitMan
 # input$BCircUnitMan = ParacouProfile$BCircUnitMan
 # input$TreeHeightUnitMan = ParacouProfile$TreeHeightUnitMan
 # input$TreeUTMUnitMan = ParacouProfile$TreeUTMUnitMan
 # input$TreePlotUnitMan = ParacouProfile$TreePlotUnitMan
 # input$TreeSubplotUnitMan = ParacouProfile$TreeSubplotUnitMan




 # put parameters back to what they were
 # input$SubplotArea = ParacouProfile$SubplotArea
 # input$PlotAreaUnitMan = ParacouProfile$PlotAreaUnitMan
 # input$SubplotAreaMan = ParacouProfile$SubplotAreaMan


  # expect year to be filled
  input$Year = "none"
  input$YearMan = -999
  expect_equal(RequiredFormat(Data, input, MeasLevel = "Tree" )$Year, DataFormated$Year)

  input$Year <- ParacouProfile$Year

  # expect Genus and species to be filled
  Data$Latin = DataFormated$ScientificName
  Data$Genus = NULL
  Data$Species = NULL
  input$ScientificName = "Latin"
  input$Genus =  input$Species = "none"
  input$ScientificNameSepMan = " "

  DataFormated <- RequiredFormat(Data, input, MeasLevel = "Tree" )

  expect_true(all(!is.na(DataFormated$Genus[!is.na(DataFormated$ScientificName)])) & all(!is.na(DataFormated$Species[!is.na(DataFormated$ScientificName)])))

  # make sure date format is handled correctly even when numeric or decimal
  Data[,  input$Date] <- as.numeric(DataFormated$Date)
  input$DateFormatMan = "numeric"
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", RequiredFormat(Data, input, MeasLevel = "Tree" )$Date)) |  all(is.na(RequiredFormat(Data, input, MeasLevel = "Tree" )$Date[!grepl("\\d{4}-\\d{2}-\\d{2}", RequiredFormat(Data, input , MeasLevel = "Tree" )$Date)])))


  Data[,  input$Date] <- lubridate::decimal_date(DataFormated$Date)
  input$DateFormatMan = "decimal"
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", RequiredFormat(Data, input, MeasLevel = "Tree"  )$Date)) |  all(is.na(RequiredFormat(Data, input, MeasLevel = "Tree"  )$Date[!grepl("\\d{4}-\\d{2}-\\d{2}",RequiredFormat(Data, input, MeasLevel = "Tree"  )$Date)])))


  # expect warning if some dates were not translated correctly
  Data[sample(10), input$Date] <- "doubidou"
  expect_warning(RequiredFormat(Data, input, MeasLevel = "Tree" ), "Some dates were translated as NA")

  # WITH ForestGEO ####
  data(ForestGeoSubset)
  data(ForestGeoProfile)
  Data <- ForestGeoSubset
  input <- ForestGeoProfile

  expect_warning(RequiredFormat(Data, input, MeasLevel = "Stem" ), "You did not specify a Site column or name, we will consider you have only one site called 'SiteA'.
You did not specify a plot area.
You did not specify a subplot area.")




  # WITH ForestPlot example ####
  appdir <- system.file(package = "DataHarmonization", "app")
  Data <- merge( data.table::fread(paste0(appdir, "/tests/shinytest/ForestPlots_test2_trees_small.csv")),
                 data.table::fread(paste0(appdir, "/tests/shinytest/ForestPlots_test2_plots_small.csv")), by.x= "PlotID", by.y = "Plot ID", suffixes = c("", ".y"))
  input <- readRDS(paste0(appdir, "/tests/shinytest/ForestPlots_test2_trees_small_Profile.rds"))
  input$TreeCodes <- "none"
  input$MinDBH = "none"
  input$MinDBHMan =1
  input$MinDBHUnitMan = "none"
  input$DateFormatMan = input$DateFormat
  input$DateFormat = NULL
  input$MeasLevel = "Stem"

  expect_warning(DataFormated <- RequiredFormat(Data, input, MeasLevel = "Stem"), "You are missing treeID")

  # make sure no IdTree is NA
  expect_false(any(is.na(DataFormated$IdTree)))

  # make sure Diameter or Circ units were converted correctly
  if(!input$Diameter %in% "none")   expect_equal(DataFormated$Diameter, Data[,input$Diameter] * switch(input$DiameterUnitMan , mm = 0.1, cm = 1, dm = 10, m = 100))
  if(!input$Circ %in% "none")   expect_equal(DataFormated$Circ, Data[,input$Circ] * switch(input$CircUnitMan, mm = 0.1, cm = 1, dm = 10, m = 100))


  # make sure Diameter is calculated correcly if only Circ is given
  if(input$Diameter %in% "none" & !input$Circ %in% "none") expect_equal(DataFormated$Diameter, round(DataFormated$Circ / pi, 2))


  # make sure date format is handled correctly (all are in yyyy-mm-dd format, and if not it is NA)
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)) |  all(is.na(DataFormated$Date[!grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)])))


  # expect ScientificName to be filled
  if(input$ScientificName %in% "none" & !input$Genus %in% "none") expect_true(all(!is.na(DataFormated$ScientificName[!is.na(DataFormated$Genus)])))


})
