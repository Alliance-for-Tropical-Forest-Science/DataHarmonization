test_that("ReversedRequiredFormat", {

  # WITH ParacouSubset ####
  data(ParacouSubsetFormated)
  data(ForestGeoProfile)
  Data <- ParacouSubsetFormated
  input <- ForestGeoProfile

  DataFormated <- ReversedRequiredFormat(Data, input )

  # make sure no IdTree is NA
  expect_false(any(is.na(DataFormated[, get(input$IdTree)])))

  # make sure Diameter or Circ units were converted correctly
  if(!input$Diameter %in% "none")   expect_equal(DataFormated[, get(input$Diameter)], Data$Diameter * switch(input$DiameterUnitMan , mm = 10, cm = 1, dm = 0.1, m = 0.01))
  if(!input$Circ %in% "none")   expect_equal(DataFormated$Circ, Data[,input$Circ] * switch(input$CircUnitMan, mm = 10, cm = 1, dm = 0.1, m = 0.01), tolerance = 0.01)


  # make sure Diameter is calculated correctly if only Circ is given
  if(input$Diameter %in% "none" & !input$Circ %in% "none") expect_equal(DataFormated[, get(input$Diameter)], round(DataFormated[, get(input$Circ)]/ pi, 2))


  # cAREFUL, EDITING DATA OR INPUT AFTER THIS LINE #


  # make sure date format is handled correctly

  for(DateFormat in c("yyyy-mm-dd", "dd/mm/yyyy", "decimal", "numeric")){
    input$DateFormatMan = DateFormat
    if(DateFormat %in% "yyyy-mm-dd") expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", ReversedRequiredFormat(Data, input )[, get(input$Date)])) | all(is.na(ReversedRequiredFormat(Data, input )[, get(input$Date)][!grepl("\\d{4}-\\d{2}-\\d{2}", ReversedRequiredFormat(Data, input )[, get(input$Date)])])))
    if(DateFormat %in% "dd/mm/yyyy") expect_true(all(grepl("\\d{2}/\\d{2}/\\d{4}", ReversedRequiredFormat(Data, input )[, get(input$Date)])) | all(is.na(ReversedRequiredFormat(Data, input )[, get(input$Date)][!grepl("\\d{4}-\\d{2}-\\d{2}", ReversedRequiredFormat(Data, input )[, get(input$Date)])])))
    if(DateFormat %in% "numeric") expect_true(all(grepl("^\\d{5}$", ReversedRequiredFormat(Data, input )[, get(input$Date)])) | all(is.na(ReversedRequiredFormat(Data, input )[, get(input$Date)][!grepl("^\\d{5}$", ReversedRequiredFormat(Data, input )[, get(input$Date)])])))
    if(DateFormat %in% "Decimal") expect_true(all(grepl("\\.", ReversedRequiredFormat(Data, input )[, get(input$Date)])) | all(is.na(ReversedRequiredFormat(Data, input )[, get(input$Date)][!grepl("\\.", ReversedRequiredFormat(Data, input )[, get(input$Date)])])))

  }

  input$DateFormatMan = ForestGeoProfile$DateFormat

  # make sure measurement units gets converted correctly or throw error if units not selected
  Data$HOM <- 1.3
  input$BCirc = "none"
  Data$BD <- Data$Diameter
  input$BD = "BD"
  input$BDUnitMan = "cm"
  Data$BHOM <- 0.1
  input$BHOM = "BHOM"
  input$BHOMUnitMan = "cm"
  Data$TreeHeight = 20
  input$TreeHeight = "TreeHeight"
  input$TreeHeightUnitMan = "m"
  Data$XTreeSubplot <- Data$XTreePlot
  Data$YTreeSubplot <- Data$YTreePlot
  input$XTreeSubplot = "Xsubplot"
  input$YTreeSubplot = "Ysubplot"
  input$TreeSubplotUnitMan = "cm"

  # for(i in c("mm", "cm", "dm", "m")){
  #
  #
  #   if(!input$Diameter %in% "none") {
  #     input$DiameterUnitMan = i
  #     expect_equal(ReversedRequiredFormat(Data, input )[, get(input$Diameter)], Data$Diameter * switch(i, mm =10, cm = 1, dm = 0.1, m = 0.01))
  #     input$DiameterUnitMan = "cm" # so that does not through an error anymore
  #   }
  #
  #   if(!input$Circ %in% "none") {
  #     input$CircUnitMan = i
  #    expect_equal(ReversedRequiredFormat(Data, input )[, get(input$Circ)], Data$Circ * switch(i, mm =10, cm = 1, dm = 0.1, m = 0.01), tolerance = 0.1)
  #     input$CircUnitMan = "cm" # so that does not through an error anymore
  #   }
  #
  #   if(!input$HOM %in% "none") {
  #     input$HOMUnitMan = i
  #    expect_equal(ReversedRequiredFormat(Data, input )[, get(input$HOM)], Data$HOM * switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
  #     input$HOMUnitMan = "cm" # so that does not through an error anymore
  #   }
  #
  #   if(!input$BD %in% "none") {
  #     input$BDUnitMan = i
  #     expect_equal(ReversedRequiredFormat(Data, input )[, get(input$BD)], Data$BD * switch(i, mm =10, cm = 1, dm = 0.1, m = 0.01), tolerance = 0.1)
  #     input$BDUnitMan = "cm" # so that does not through an error anymore
  #   }
  #
  #   if(!input$BCirc %in% "none")  {
  #     input$BCircUnitMan = i
  #     expect_equal(ReversedRequiredFormat(Data, input )[, get(input$BCirc)], Data$BCirc * switch(i, mm =10, cm = 1, dm = 0.1, m = 0.01), tolerance = 0.1)
  #     input$BCircUnitMan = "cm" # so that does not through an error anymore
  #   }
  #
  #   if(!input$BHOM %in% "none") {
  #     input$BHOMUnitMan = i
  #     expect_equal(ReversedRequiredFormat(Data, input )[, get(input$BHOM)], Data$BHOM * switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
  #     input$BHOMUnitMan = "cm" # so that does not through an error anymore
  #   }
  #
  #   if(!input$TreeHeight %in% "none")  {
  #     input$TreeHeightUnitMan = i
  #    expect_equal(ReversedRequiredFormat(Data, input )[, get(input$TreeHeight)], Data$TreeHeight *  switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
  #     input$TreeHeightUnitMan = "cm" # so that does not through an error anymore
  #   }
  #
  #   if(!input$XTreeUTM %in% "none")  {
  #     input$TreeUTMUnitMan = i
  #     expect_equal(ReversedRequiredFormat(Data, input )[, get(input$XTreeUTM)], Data$Xutm *  switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
  #     input$TreeUTMUnitMan = "cm" # so that does not through an error anymore
  #   }
  #
  #   if(!input$XTreePlot %in% "none") {
  #     input$TreePlotUnitMan = i
  #    expect_equal(ReversedRequiredFormat(Data, input )[, get(input$XTreePlot)], Data$Xplot *  switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
  #     input$TreePlotUnitMan = ParacouProfile$TreePlotUnitMan
  #   }
  #
  #   if(!input$XTreeSubplot %in% "none"){
  #     input$TreeSubplotUnitMan = i
  #    expect_equal(ReversedRequiredFormat(Data, input )[, get(input$XTreeSubplot)], Data$Xsubplot *  switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
  #     input$TreeSubplotUnitMan = "cm" # so that does not through an error anymore
  #   }
  #
  # }

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



  # make sure AREA gets converted correctly or throw error if units not selected
  Data$SubplotArea <- Data$PlotArea
  input$SubplotArea <- "SubplotArea"
  input$SubplotAreaUnitMan <- "ha"



  for(i in c("m2", "ha", "km2")){

    if(!input$PlotArea %in% "none")   {
      input$PlotAreaUnitMan = i
      expect_equal(ReversedRequiredFormat(Data, input )[, get(input$PlotArea)], Data$PlotArea * switch(i , m2 = 10000, ha = 1, km2 = 0.01))
      input$PlotAreaUnitMan = "ha"
    }

    if(!input$SubplotArea %in% "none")  {
      input$SubplotAreaUnitMan = i
      expect_equal(ReversedRequiredFormat(Data, input )[, get(input$SubplotArea)], Data$SubplotArea *  switch(i , m2 = 10000, ha = 1, km2 = 0.01))
      input$SubplotAreaUnitMan = "ha"
    }


  }

  # put parameters back to what they were
  # input$SubplotArea = ParacouProfile$SubplotArea
  # input$PlotAreaUnitMan = ParacouProfile$PlotAreaUnitMan
  # input$SubplotAreaMan = ParacouProfile$SubplotAreaMan


})

