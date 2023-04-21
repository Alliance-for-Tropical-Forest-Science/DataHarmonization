#' RecruitmentCorrection
#'
#' @param Data  (data.frame or data.table)
#' The dataset should preferably contain the column of corrected diameters:
#' 'DBH_DataHarmonizationCor', otherwise the function will take the column 'Diameter'
#'
#' @param KeepMeas In case of **multiple diameter measurements** in the same
#'   census year, on which to apply the correction:
#' Possible values: "MaxHOM", "MaxDate" (character).
#'   - "MaxHOM": apply the correction to the measurement taken at the
#'               **highest POM**
#'   - "MaxDate": apply the correction to the **most recent measurement** (same
#'                year but more recent date)
#'
#' @param MinDBH Minimum diameter of trees inventoried (in cm) (numeric, 1 value) or
#'   NULL (Default) if you wish to use the MinDBH indicated in your data, which may vary per plot
#'
#' @param OnlyDetectMissedRecruits TRUE: Only detect errors, FALSE: detect and correct errors
#'   (logical)
#'
#' @details This function detects trees that may have been missed
#' at a previous census if the estimated diameter at that census is
#' greater than MinDBH. Estimated diameter is calculated
#' by recursively removing the tree's first growth measurement to it's first
#' diameter measurement. If growth can't be calculated for a tree, we use
#' the average first growth of trees of the same species, if not possible,
#' of the same genus, and if not possible, of all trees.
#' It is strongly recommended to correct the Diameter before correcting the recruits.
#'
#' @return  When non in OnlyDetectMissedRecruits mode, add rows for forgotten recruits with their estimated DBH in the
#'   'Diameter_DataHarmonizationCor' column, create a 'CorrectedRecruit' col (logical) to indicate
#'    the rows that were added.
#'   In OnlyDetectMissedRecruits mode, fill the 'Comment' column : "This DBH is/was the 1st recorded for this
#'   tree, according to its annual growth and the census done for this plot, it
#'   should have been recruited earlier according to your protocol (MinDBH)."
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom stats na.omit lm
#' @importFrom shiny incProgress
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data(TestData)
#' setnames(TestData, "Diameter", "Diameter_DataHarmonizationCor", skip_absent=TRUE)
#'
#' Rslt <- RecruitmentCorrection(TestData)
#' IdCorr <- Rslt[CorrectedRecruit %in%  TRUE, IdStem]
#' TreesCorr <- Rslt[IdStem %in% IdCorr]
#'
#' library(ggplot2)
#' ggplot(TreesCorr) +
#' aes(x = Year, y = Diameter_DataHarmonizationCor) +
#'   geom_line(size = 0.5, colour = "#112446") +
#'   geom_point(shape = "circle", size = 1.5, mapping = aes(color = CorrectedRecruit)) +
#'   theme_minimal() +
#'   facet_wrap(vars(IdStem), scales = "free")
#'
RecruitmentCorrection <- function(
    Data,

    KeepMeas = c("MaxHOM", "MaxDate"),

    MinDBH = NULL,
    OnlyDetectMissedRecruits = FALSE
){

  ThisIsShinyApp =  shiny::isRunning() # this is for internal use when function used by Shiny app

  if(ThisIsShinyApp) incProgress(1/15)

  InvariantColumns = c("MinDBH", "Site",
                       "Cluster", "Plot", "PlotViewID", "Subplot", "PlotArea", "PlotElevation",
                       "SubplotArea", "PlotLat", "PlotLon", "XPlotUTM", "YPlotUTM",
                       "SubplotLat", "SubplotLon", "XSubplotUTM", "YSubplotUTM", "ScientificName",
                       "VernName", "Family", "Genus", "Species", "Subspecies", "Variety",
                       "Voucher", "IdLevel", "Authority", "CommercialSp", "LifeForm",
                       "TreeFieldNum", "IdTree", "StemFieldNum", "IdStem",
                       "TreeLat", "TreeLon", "XTreeUTM", "YTreeUTM", "XTreePlot", "YTreePlot",
                       "XTreeSubplot", "YTreeSubplot")

  #### Arguments check ####
  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # IdStem or IdTree? ---------------------------------------------------------------------------------------
  # If no IdStem take IdTree
  if((!"IdStem" %in% names(Data) | all(is.na(Data$IdStem))) &
     ("IdTree" %in% names(Data) & any(!is.na(Data$IdTree))) ){ ID <- "IdTree"

  }else{ ID <- "IdStem"}

  if(!any(c("IdStem", "IdTree") %in% names(Data)) | (all(is.na(Data$IdStem)) &  all(is.na(Data$IdTree))) )
    stop("The 'IdStem' or 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------

  # If MinDBH is provided, overwrite the one in the data
  if(ifelse(ThisIsShinyApp, !is.na(MinDBH), !is.null(MinDBH))) { # !is.na(MinDBH)  is needed when inside the app
    if(!inherits(MinDBH, c("numeric", "integer"))) stop("MinDBH must be numeric")
    if(length(MinDBH) > 1) stop("MinDBH must be numeric value of length 1")

    Data$MinDBH <- MinDBH
  } else {
    if(!"MinDBH" %in% names(Data)) stop("You don't have MinDBH in your dataset, please provide a MinDBH in the arguments.")
    # -----------------------------------------------------------------------
  }

  # OnlyDetectMissedRecruits (logical)
  if(!all(unlist(lapply(list(OnlyDetectMissedRecruits),
                        inherits, "logical"))))
    stop("The 'OnlyDetectMissedRecruits' argument
         of the 'RecruitmentCorrection' function must be logicals")

  # Diameter_DataHarmonizationCor column exists
  if(!"Diameter" %in% names(Data) & !"Diameter_DataHarmonizationCor" %in% names(Data))
    stop("The 'Diameter' or the 'Diameter_DataHarmonizationCor' (corrected Diameter)
           column does't exist in the dataset.")

  # Diameter_DataHarmonizationCor column exists
  if(!OnlyDetectMissedRecruits){
    if(!"Diameter_DataHarmonizationCor" %in% names(Data))
      warning("We advise to first correct the diameter measurements before correcting the recruitment")
  }


  # use corrected columns as InvariantColumns if they exist (mostly for Species names that may have been corrected)
  if(!OnlyDetectMissedRecruits) {
    idx_corrected_columns <- paste0(InvariantColumns, "_DataHarmonizationCor") %in% names(Data)
    InvariantColumns[idx_corrected_columns] <- paste0(InvariantColumns, "_DataHarmonizationCor")[idx_corrected_columns]
  }

  #### Function ####
  if(ThisIsShinyApp) incProgress(1/15)

  # data.frame to data.table
  setDT(Data)
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environment

  if(!"Comment_DataHarmonization" %in% names(Data)) Data[, Comment_DataHarmonization := ""]


  if(length(na.omit(unique(Data$IdCensus))) > 1) { # only possible if more than one census

  # Remove duplicated measurements per Year because different POM or Date ------
  CompleteData <- copy(Data)


  Data <- UniqueMeasurement(Data, KeepMeas = KeepMeas, ID = ID)

  DuplicatedRows <- CompleteData[!Data, on = .NATURAL] # rows removed





  if(!OnlyDetectMissedRecruits){
    Data[, CorrectedRecruit := FALSE] # The initial rows are not corrected recruits
  }

  # Order IDs and times in ascending order -------------------------------------
  Data <- Data[order(get(ID), Year)]

  # IDs vector -----------------------------------------------------------------
  Ids <- as.vector(na.omit(unique(Data[, get(ID)]))) # Tree Ids

  # Dataset with the rows without IDS ------------------------------------------
  DataIDNa <- Data[is.na(get(ID))]

  # Dataset with the rows without Year -----------------------------------------
  DataYearNa <- Data[is.na(Year)]

  if(ThisIsShinyApp) incProgress(1/15)

  # Apply for all the trees ------------------------------------

  if(!"Diameter_DataHarmonizationCor" %in% names(Data)) {
    Data[, Diameter_DataHarmonizationCor := Diameter]
  }
  if(!"ScientificName_DataHarmonizationCor" %in% names(Data)) {
    Data[, ScientificName_DataHarmonizationCor := ScientificName]
  }
  if(!"Genus_DataHarmonizationCor" %in% names(Data)) {
    Data[, Genus_DataHarmonizationCor := Genus]
  }

  # get a DiameterHistory
  DiameterHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "Diameter_DataHarmonizationCor", drop = FALSE)
  DiameterHistory <- as.matrix(DiameterHistory, 1)

  # get DateHistory (to be able to calculate growth)
  DateHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "Date", drop = FALSE)
  DateHistory <- as.matrix(DateHistory, 1)

  # get a MinDBHHistory (this is useful if not same threshold accross plots)
  MinDBHHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "MinDBH", drop = FALSE)
  MinDBHHistory <- as.matrix(MinDBHHistory, 1)
  MinDBHHistory[] <- apply(MinDBHHistory, 1, function(x) x[is.na(x)] <- as.numeric(names(which.max(table(x))))) # fill NA with most common MinDBH in the row . this could be a problem if MinDBH changes accross censuses but should be rare enough that it does not matter

  if(ThisIsShinyApp) incProgress(1/15)

  # get growth History
  GrowthHistory <- matrix(NA, ncol = ncol(DiameterHistory)-1, nrow = nrow(DiameterHistory), dimnames = list(rownames(DiameterHistory), NULL))
  GrowthHistory[] <- t(round(apply(DiameterHistory, 1, diff) / apply(DateHistory, 1, function(x) diff(as.Date(x))/365), 2))

  if(ThisIsShinyApp) incProgress(1/15)

  # get species and Genus
  Species <- Data[, ScientificName_DataHarmonizationCor[1], by = c(ID)]
  Species <- as.matrix(Species, 1)

  Genus <- Data[, Genus_DataHarmonizationCor[1], by = c(ID)]
  Genus <- as.matrix(Genus, 1)

  # find out leading NAs and first growth
  LeadingNAHistory <- DiameterHistory
  LeadingNAHistory [] <- t(apply(DiameterHistory, 1, function(x) {
    y <- x
    ina <- which(is.na(x))
    diffina <- diff(ina)

    y[] <- 0
    if(1 %in% ina) {
      ilna <- 1
      if(diffina[1] %in% 1)  ilna <- 1:(attr(regexpr("^1+", paste(diffina, collapse = "")), "match.length")+1)
      y[ilna] <- 1
    }
    y
  }))

  if(ThisIsShinyApp) incProgress(1/15)

  FirstDiameter <- apply(DiameterHistory, 1, function(x) {x[which(!is.na(x))[1]] })

  FirstGrowth <- apply(GrowthHistory, 1, function(x) {x[which(!is.na(x))[1]]})

  AvgSpFstGrowth <- tapply(FirstGrowth, Species[,1], mean, na.rm = T)
  AvgGnFstGrowth <- tapply(FirstGrowth, Genus[,1], mean, na.rm = T)

  FirstGrowth[is.na(FirstGrowth)] <- AvgSpFstGrowth[Species[is.na(FirstGrowth),1]] # first take species avg
  FirstGrowth[is.na(FirstGrowth)] <- AvgGnFstGrowth[Genus[is.na(FirstGrowth),1]] # then take genus avg

  FirstGrowth[is.na(FirstGrowth)] <- mean(FirstGrowth, na.rm = T) # then take overall avg

  # DiameterHistory["380189_1_auto", ]
  # LeadingNAHistory["380189_1_auto", ]
  # FirstDiameterHistory["380189_1_auto", ]
  # GrowthHistory["380189_1_auto", ]
  # FirstGrowth["380189_1_auto"]

  # DiameterHistory["391764_1_auto", ]
  # LeadingNAHistory["391764_1_auto", ]
  # # FirstDiameterHistory["391764_1_auto", ]
  # FirstDiameter["391764_1_auto"]
  # GrowthHistory["391764_1_auto", ]
  # FirstGrowth["391764_1_auto"]


  if(ThisIsShinyApp) incProgress(1/15)

  # fill in the leading NA with calculated DBH by recursively removing first growth

  IdsToFix <- names(which(LeadingNAHistory == 1, arr.ind = T)[,1])

  # only look at cases when there is at least one diameter
  IdsToFix <- IdsToFix[apply(DiameterHistory[IdsToFix, ], 1, function(x) any(!is.na(x)))]

  # only look at cases that would likely mean missed recruit (where 1st diameter > MinDBH)
  IdsToFix <- IdsToFix[mapply(function(d, g, m) {
    x <- d-g
    x[which(!is.na(x))[1]] > m
  }, FirstDiameter[IdsToFix], FirstGrowth[IdsToFix], MinDBHHistory[IdsToFix, 1]
  )]

  # MatrixFirstGrowth <- matrix(rep(FirstGrowth, each = ncol(DiameterHistory)-1), ncol =  ncol(DiameterHistory)-1, byrow = T, dimnames = list(rownames(DiameterHistory), NULL))

  DiameterHistoryCorrected <- DiameterHistory

  for(id in IdsToFix) {
    x <- DiameterHistory[id, ]
    y <- LeadingNAHistory[id,]
    g <- FirstGrowth[id]

    while(any(is.na(x[y %in% 1]))) x[rev(which(is.na(x)))[1]] <- x[which(!is.na(x))[1]] -g

    DiameterHistoryCorrected[id, ] <- x

  }

  if(ThisIsShinyApp) incProgress(1/15)


  # keep only the ones that are > MinDBH
  DiameterHistoryCorrected[LeadingNAHistory%in%1] <- ifelse(DiameterHistoryCorrected[LeadingNAHistory%in%1] <  MinDBHHistory[LeadingNAHistory%in%1], NA, DiameterHistoryCorrected[LeadingNAHistory%in%1])


  # melt the corrected diameters and find out the ones that would lead to new rows in the data
  DiameterHistoryCorrected <- melt(setDT(as.data.frame(DiameterHistoryCorrected), keep.rownames=TRUE), measure.vars = colnames(DiameterHistoryCorrected) , variable.name = "IdCensus")

  idx_new_rows  <- which(match(paste(DiameterHistoryCorrected$rn, DiameterHistoryCorrected$IdCensus), paste(Data[,get(ID)], Data[,IdCensus])) %in% NA)

  # find out which rows in Data should get a comment to indicate the previous census should have recruited this tree already (adding this comment only when OnlyDetectMissedRecruits)
  idx_CommentOnly <- match(paste(DiameterHistoryCorrected$rn, DiameterHistoryCorrected$IdCensus)[!is.na(DiameterHistoryCorrected$value)], paste(Data[,get(ID)], Data[,IdCensus]))
  idx_CommentOnly <- idx_CommentOnly[!duplicated(Data[,get(ID)][idx_CommentOnly])]
  idx_CommentOnly <- idx_CommentOnly[!is.na(idx_CommentOnly)]

  if(ThisIsShinyApp) incProgress(1/15)

  # add comment if detection only
  if(OnlyDetectMissedRecruits)
    Data[idx_CommentOnly, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "This tree should have been recruited earlier based on its growth and your protocol (MinDBH)")]


  # add rows with the corrected DBH if not detection only + indicate TRUE in the recruit column

  if(!OnlyDetectMissedRecruits) {

    # get what we know about those trees
    NewRows <- Data[get(ID) %in% DiameterHistoryCorrected$rn[idx_new_rows], ]
    NewRows <- NewRows[!duplicated(get(ID)), ]

    # remove the data that changes at each census
    NewRows[,setdiff(names(NewRows), InvariantColumns) := NA]

    # fill in info we know about the census and based on the Status we corrected
    m <- match(NewRows[,get(ID)], DiameterHistoryCorrected[idx_new_rows, rn])

    NewRows$IdCensus <- DiameterHistoryCorrected[idx_new_rows, IdCensus][m]

    NewRows$Diameter_DataHarmonizationCor <- DiameterHistoryCorrected[idx_new_rows, value][m]

    NewRows$CorrectedRecruit <- TRUE

    # make best guess at other things
    warning("We added rows for trees that were supposed to be recruited earlier based on growth pattern and MinDBH")

    NewRows$Date <- as.Date(tapply(Data$Date, Data$IdCensus, mean, na.rm = T), origin = "1970-01-01")[NewRows$IdCensus]

    NewRows$Year <- format(NewRows$Date, "%Y")
    NewRows$Month <- format(NewRows$Date, "%m")
    NewRows$Day <- format(NewRows$Date, "%d")

    # Add these rows in the dataset
    Data <- rbindlist(list(Data, NewRows), use.names=TRUE, fill=TRUE)
  }

 # add rows for those that have been missed


  if(ThisIsShinyApp) incProgress(1/15)

  # Put back duplicated rows, or rows without ID or Year -----------------------
  DuplicatedRows[, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "This duplicated measurement was not processed by RecruitmentCorrection.")]
  DataIDNa[, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "This missing ID measurement was not processed by RecruitmentCorrection.")]
  DataYearNa[, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "This missing year measurement was not processed by RecruitmentCorrection.")]

  Data <- rbindlist(list(Data, DuplicatedRows, DataIDNa, DataYearNa), use.names = TRUE, fill = TRUE)


  # Order IDs and times in ascending order -------------------------------------
  Data <- Data[order(get(ID), IdCensus), ]

  } else {
    warning("You only have one census so we can't only apply recruitment corrections.")
  }

  if(ThisIsShinyApp) incProgress(1/15)

  # return Data
  return(Data)

}
