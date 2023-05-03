test_that("StatusCorrection", {

  # Import data
  # library(data.table)
  TestData <- data.table(Site = "Nowhere",
                         Plot = "1",
                         IdTree = c("a", "b", "c", "d", "e"), # 5 ind
                         ScientificName = "Plant",
                         IdCensus =  rep(c(2012:2020), 5),
                         Year = rep(c(2012:2020), 5), # 9 census

                         Diameter = NA_real_)
  TestData <- TestData[order(IdTree, Year)]
  TestData[,LifeStatus := c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, # "a"
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, # "b"
    TRUE, TRUE, TRUE, NA, FALSE, TRUE, TRUE, TRUE, FALSE, # "c"
    TRUE, TRUE, TRUE, TRUE, TRUE, NA, NA, FALSE, NA, # "d"
    FALSE, TRUE, NA, FALSE, TRUE, NA, NA, FALSE, NA) # "e"
  ]

  TestData[IdTree %in% "e", ("Diameter") := c(13:21)] # "e" Diameter seq
  TestData[IdTree %in% "e" & Year == 2014, ("Diameter") := NA] # a NA in the "e" Diameter seq
  TestData <- TestData[!(IdTree %in% "a" & Year %in% 2014),] # remove one record

  suppressWarnings(TestData <- RequiredFormat(TestData,
                             input = list(MeasLevel = "Tree",
                                          Site = "Site",
                                          Plot = "Plot",
                                          IdTree = "IdTree",
                                          ScientificName = "ScientificName",
                                          IdCensus= "IdCensus",
                                          Year = "Year",
                                          YearMan = -999,
                                          Month = "none",
                                          Day = "none",
                                          Date = "none",
                                          Diameter = "Diameter",
                                          LifeStatus = "LifeStatus",
                                          DeadStatus = "LifeStatus",
                                          DiameterUnitMan = "cm",
                                          PlotArea = "none",
                                          SubplotArea = "none",
                                          IsLiveMan = T,
                                          IsDeadMan = F),
                             MeasLevel = "Stem")
  )

  # Create test data
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("Diameter")]
  NoPlotData <- TestData[, !c("Plot")]


  # Check the function argument
  expect_error(StatusCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  expect_error(StatusCorrection(NoPlotData),
               regexp = "The column 'Plot' must be present in the dataset")


  expect_error(StatusCorrection(TestData, DeathConfirmation = TRUE),
               regexp = "'DeathConfirmation' argument must be numeric")

  expect_error(StatusCorrection(TestData,
                                UseSize = "yes",
                                AddRowsForForgottenCensuses = "no"),
               regexp = "The 'UseSize' and 'RemoveRAfterDeath' arguments
         of the 'SatusCorrection' function must be logicals")



  expect_error(StatusCorrection(Data = NoDBHData,
                                UseSize = TRUE),
               regexp = "the 'Diameter' column must be present in the dataset")

  # Check the function work

  expect_warning(StatusCorrection(TestData), "We added rows for missing trees and imputed average census Date")

  expect_equal(nrow(TestData), nrow(StatusCorrection(TestData, AddRowsForForgottenCensuses = FALSE)))

  Rslt <- suppressWarnings(StatusCorrection(TestData, UseSize = TRUE))

  Ids <- as.vector(na.omit(unique(TestData$IdTree))) # Tree Ids

  # Ids = "a"
  # i = "d"
  for(i in Ids){

    Seq <- Rslt[IdTree %in% i, LifeStatus]
    SeqCor <- Rslt[IdTree %in% i, LifeStatus_DataHarmonizationCor]

    ## No "dead" before "alive"
    LastAlive <- max(which(SeqCor %in% TRUE))
    Deaths <- which(SeqCor %in% FALSE)
    expect_true(all(Deaths > LastAlive))

    ## No "NA" between the first and last "alive"
    FirstAlive <- min(which(SeqCor %in% TRUE))
    Unseen <- which(SeqCor %in% NA)
    expect_true(all(Unseen < FirstAlive | Unseen > LastAlive))

    ## After the death always the death (no "NA")
    if(length(Deaths) > 0){
      FirstDead <- min(Deaths)
      expect_true(all(Unseen < FirstDead))
    }

    ## If no "dead" but "NA" nbr >= DeathConfirmation -> "dead" in "DBHCor"
    # Alive NA NA DEAD NA
    # Alive Alive NA NA
    DeathConfirmation <- 2
    Unseen_seq <- which(Seq %in% NA)
    Deaths_seq <- which(Seq %in% FALSE)

    if(length(Deaths_seq) == 0){ # if no death (Alive Alive NA NA)
      if(length(Unseen_seq) >= DeathConfirmation)
        expect_true(all(SeqCor[Unseen_seq] == FALSE))

    }else{ # if death in the seq (Alive NA NA DEAD NA)
      FirstDeath <- min(Deaths_seq)
      UnseenBfDeath <- sum(Unseen_seq < FirstDeath) # nbr of NA before the death

          if(UnseenBfDeath >= DeathConfirmation)
            expect_true(all(SeqCor[Unseen_seq] == FALSE))
    }

    ## If UseSize : if Diameter != NA -> Alive
    expect_true(all(SeqCor[!is.na(Rslt[IdTree %in% i, Diameter])] == T))

    ## Add a "Comment" value when "LifeStatus" != "LifeStatus_DataHarmonizationCor"
    Comment <- Rslt[IdTree %in% i, StatusCorrectionMeth_DataHarmonization ] != ""

    compareNA <- function(v1,v2) {
      same <- (v1 == v2) | (is.na(v1) & is.na(v2))
      same[is.na(same)] <- FALSE
      return(same)
    }

    expect_true(all(!compareNA(Seq, SeqCor) == Comment))
  }




# No "dead" before "alive"
# No "NA" between the first and last "alive"
# after the death always the death (no "NA")
# if no "dead" but "NA" nbr >= DeathConfirmation -> "dead" in "DBHCor"
# if UseSize : if Diameter != NA -> Alive



# check SCBI subset -------------------------------------------------------

data(SCBISubsetFormated)

expect_warning(SCBICorrected <- StatusCorrection(SCBISubsetFormated, UseSize = T), "We added rows for missing trees and imputed average census Date")

expect_identical(unique(SCBICorrected$StatusCorrectionMeth_DataHarmonization ), c("", "A measured tree is a living tree", "Between 2 alive occurrences, the tree was alive",
                                                                         "Tree can't be dead before first being alive"))


expect_identical(SCBICorrected[IdStem %in% "10012",LifeStatus_DataHarmonizationCor], SCBICorrected[IdStem %in% "10012",LifeStatus]) # this IdStem should not have any corrections

expect_true(all(SCBICorrected[IdStem %in% "11012", LifeStatus_DataHarmonizationCor])) # first measurement had a DBH so should be alive, then second should be Alive because inbetwen two alive

expect_identical(SCBICorrected[IdStem %in% "11012", StatusCorrectionMeth_DataHarmonization  ], c("A measured tree is a living tree", "Between 2 alive occurrences, the tree was alive",
                                                                          ""))



SCBISubsetFormated[IdStem %in% "31258",]
expect_identical(SCBICorrected[IdStem %in% "31258", Comment_DataHarmonization ], c("","Missed stem",  ""))

SCBISubsetFormated[IdStem %in% "10032",]

expect_equal(SCBICorrected[IdStem %in% "10032",.(LifeStatus_DataHarmonizationCor, StatusCorrectionMeth_DataHarmonization )],
                 structure(list(LifeStatus_DataHarmonizationCor = c(TRUE, FALSE, FALSE),
                                StatusCorrectionMeth_DataHarmonization  = c("A measured tree is a living tree","", "")),
                           row.names = c(NA, -3L), class = c("data.table","data.frame")), ignore_attr = TRUE)


expect_equal(nrow(SCBICorrected[StatusCorrectionMeth_DataHarmonization  %in% "A measured tree is a living tree",]), 5)

})

