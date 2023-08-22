#' Diameter correction
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdTree` or `IdStem` (character)
#'   - `ScientificName_DataHarmonizationCor` (character)
#'   - `Diameter` (numeric)
#'   - `IdCensus` (ordered factor)
#'   - **`POM` (Point Of Measurement) (factor)** or
#'     **`HOM` (Height Of Measurement) (numeric)** if you want to correct from
#'      the **"POM change"**
#'   If you want to apply the **"phylogenetic hierarchical"** correction, the
#'   dataset must also contain the columns:
#'   - `Genus_DataHarmonizationCor` (character)
#'   - `Family_DataHarmonizationCor` (character)
#'
#' @param UseTaperCorrection (logical) TRUE: transform the tree diameter measured at a given height
#' into the diameter corresponding to the default measurement height (`DefaultHOM`), using an allometry.
#'   FALSE: do not apply a taper correction
#'
#' @param DefaultHOM Default Height Of Measurement in meter (Default: 1.3 m)
#'   (numeric, 1 value)
#'
#' @param TaperParameter Taper parameter (unitless) formula (function)
#' Default: *TaperParameter = 0.156 - 0.023 log(DAB) - 0.021 log(HOM)*
#' of Cushman et al.2021.
#' With:
#'   - *DAB*: Diameter Above Buttress (in cm)
#'   - *HOM*: Height Of Measurement (in m)
#'
#' @param TaperFormula Taper formula (function)
#' Default: *DAB / (e^(- TaperParameter (HOM - DefaultHOM)))*
#' of Cushman et al.2021.
#' With:
#'   - *DAB*: Diameter Above Buttress (in cm)
#'   - *HOM*: Height Of Measurement (in m)
#'   - *DefaultHOM*:  Default Height Of Measurement (in m)
#'   - *TaperParameter*: Taper parameter (unitless)
#'
#' @param KeepMeas In case of **multiple diameter measurements** in the same
#'   census:
#' Possible values: "MaxHOM", "MaxDate" (character).
#'   - "MaxHOM": apply the correction to the measurement taken at the
#'               **highest POM**
#'   - "MaxDate": apply the correction to the **most recent measurement** (same
#'                IdCensus but more recent date)
#'
# param MaxDBH Maximum possible DBH (Diameter at the default HOM) of your
# stand in cm (numeric, 1 value)
#'
#' @param PositiveGrowthThreshold in cm/year: a tree
#'   widening by more than this value is considered abnormal (numeric, 1 value)
#'
#' @param NegativeGrowthThreshold in cm/census: the possible
#'   positive measurement error (+n) cannot be corrected until the growth
#'   appears abnormal, but a negative measurement error can be allowed until -n
#'   (a tree does not decrease). Thus the positive measurement error (+n) is
#'   "compensated". (numeric, 1 value)
#'
#' @param Pioneers Scientific names of the pioneer species of the site, as in
#'   the `ScientificName_DataHarmonizationCor` column (characters vector)
#'
#' @param PioneersGrowthThreshold in cm/year: a tree of a pioneer species that
#'   widens by more than this value is considered abnormal (numeric, 1 value)
#'
#' @param WhatToCorrect Possible values: "POM change", "Abnormal growth"
#'   (character). All are complementary and recommended.
#'   - "POM change": detect POM change in the column `POM` and correct the
#'                   Diameter values from it. (Ignored if taper correction is applied)
#'   - "Abnormal growth": detect if the growth is greater than PositiveGrowthThreshold ('PioneersGrowthThreshold' if species belongs to 'Pioneers')
#'    or smaller than NegativeGrowthThreshold and correct it by `CorrectionType`
#'
#' @param CorrectionType Possible values: "individual", "phylogenetic
#'   hierarchical" (character, 1 value).
#'   - "individual": replace abnormal growth by interpolation from the
#'                   individual values.
#'   - "phylogenetic hierarchical": replace abnormal growth with the average
#'          growth of other trees in the dataset, at the specific, genus, family
#'          or stand level, within a DBH range of x cm (*DBHRange* argument).
#'          If the number of these trees < n (*MinIndividualNbr* argument)
#'          at the specific level, we switch to the genus level etc.
#'
#' @param DBHRange DBH range in cm to take into account to select other trees in
#'   the dataset to apply "phylogenetic hierarchical" correction (Default: 10
#'   cm) (numeric, 1 value)
#' @param MinIndividualNbr Minimum number of individuals to take into account in
#'   "phylogenetic hierarchical" correction (Default: 5) (numeric, 1 value)
#'
#'
#' @param DBHCorForDeadTrees (logical) TRUE: return DBHCor also for dead trees.
#'   FALSE: do not return DBHCor for dead trees. In this case it is advisable to
#'   have corrected the tree life status with the *StatusCorrection()* function.
#'
#' @param AddMissedStems (logical) if TRUE, adds rows for trees that were missed between
#' two censuses, with their estimated diameter (based on linear regression)
#'
#' @param AddMissedRecruits (logical) TRUE: adds rows for stem that were supposed
#' to be recruited at a prior census, based on their estimated diameter (from linear regression)
#' and MinDBH. FALSE: will only indicate in the comment that the stem was supposed to be
#' recruited earlier.
#'
#' @param MinDBH Minimum diameter of trees inventoried (in cm) (numeric, 1 value) or
#'   NULL (Default) if you wish to use the MinDBH indicated in your data, which may vary per plot
#'
#'
#'
#' @param coef (numeric, 1 value) This is used in individual corrections, to calculate weight of the growths by temporal proximity

#'
#' @return Fill the *Comment_DataHarmonization* column with error type information and add columns:
#'   - *Diameter_DataHarmonizationCor*: corrected trees diameter at default HOM
#'   - *DiameterCorrectionMeth_DataHarmonization* = "local linear regression","weighted
#'       mean"/phylogenetic hierarchical("species"/"genus"/"family"/"stand")/
#'       "shift realignment"/"Same value".
#'   - *POM_DataHarmonizationCor* (factor): POM value at which the corrected diameters are proposed.
#'       Corresponds to the 1st POM value at which the stem was measured.
#'   - *HOM_DataHarmonizationCor* (numeric): HOM value at which the corrected diameters
#'       are proposed. Corresponds to the 1st HOM value at which the stem was
#'       measured.
#'   - *MissedStem_DataHarmonizationCor* if AddMissedStems is TRUE (with value TRUE for new rows).
#'   - *MissedRecruit_DataHarmonizationCor* if AddMissedRecruits is TRUE (with value TRUE for new rows).
#'
#' @details When there is only 1 `Diameter` value for a tree/stem,
#'   `Diameter_DataHarmonizationCor` takes the original `Diameter` value. If this value
#'   is 0 or > MaxDBH, `Diameter_DataHarmonizationCor` takes NA. Diameters not linked to
#'   an IdTree/IdStem or to a Census IdCensus are not processed.
#'   Punctual error correction only with linear regression and not quadratic,
#'   because punctual errors are corrected from a local regression with the 2
#'   framing values.
#'
#' @importFrom utils capture.output
#' @importFrom stats na.omit complete.cases weighted.mean predict
#' @importFrom shiny incProgress isRunning
#' @importFrom data.table shift
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggforce n_pages facet_wrap_paginate
#'
#' @export
#'
#' @examples
#' # library(data.table)
#' data(TestData)
#'
#' TestData$HOM[1:3] <- c(0.5,1.5,NA)
#' TestData$Diameter[21:23] <- c(31,91,14)
#' TestData <- TestData[!(IdStem %in% "100658_1_auto" & IdCensus %in% 2017), ]
#'
#' Rslt <- DiameterCorrection(
#'  TestData,
#'   WhatToCorrect = c("POM change", "Abnormal growth"),
#'     CorrectionType = c("phylo"),
#'     MinIndividualNbr = 1)
#'
#' DiameterCorrectionPlot(Rslt, OnlyCorrected = TRUE)
#'
DiameterCorrection <- function(
    Data,

    UseTaperCorrection = TRUE,
    DefaultHOM = 1.3,
    TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
    TaperFormula = function(DAB, HOM, TaperParameter, DefaultHOM) DAB / (exp(- TaperParameter*(HOM - DefaultHOM))),


    KeepMeas = c("MaxHOM", "MaxDate"),

    MinDBH = NULL,
    AddMissedRecruits = TRUE,
    AddMissedStems = TRUE,


    PositiveGrowthThreshold = 5,
    NegativeGrowthThreshold = -2,

    Pioneers = NULL,
    PioneersGrowthThreshold = 7.5,

    WhatToCorrect = c("POM change", "Abnormal growth"),
    CorrectionType = c("individual", "phylogenetic hierarchical"),

    DBHRange = 10,
    MinIndividualNbr = 5,

    DBHCorForDeadTrees = TRUE,

    coef = 0.9
){



# App-related housekeeping --------------------------------------------------------

  ThisIsShinyApp =  shiny::isRunning() # this is for internal use when function used by Shiny app


  # prepare a place to hold all warnings so we get only one pop up window
  AllWarnings <- NULL

  InvariantColumns = c("Site", "IdTree", "MinDBH",
                       "Cluster", "Plot", "PlotArea", "PlotElevation", "Subplot",
                       "SubplotArea", "PlotViewID", "PlotLat", "PlotLon", "XPlotUTM",
                       "YPlotUTM", "SubplotLat", "SubplotLon", "XSubplotUTM", "YSubplotUTM",
                       "ScientificName", "VernName", "Family", "Genus", "Species", "Subspecies",
                       "Variety", "Voucher", "IdLevel", "Authority", "CommercialSp",
                       "LifeForm", "TreeFieldNum", "StemFieldNum", "IdStem",
                       "TreeLat", "TreeLon", "XTreeUTM", "YTreeUTM", "XTreePlot", "YTreePlot",
                       "XTreeSubplot", "YTreeSubplot", "LifeStatus",
                       "HOM", "POM", "BHOM", "BPOM",
                       "ScientificNameOriginal", "CommercialSpOriginal",
                       "TreeFieldNumOriginal", "IdTreeOriginal", "StemFieldNumOriginal",
                       "IdStemOriginal", "LifeStatusOriginal",
                       "Family_DataHarmonizationCor", "Genus_DataHarmonizationCor", "Species_DataHarmonizationCor",
                       "Determination_rank_DataHarmonizationCor",
                       "Name_DataHarmonizationCor", "ScientificName_DataHarmonizationCor",
                       "StatusCorrectionMeth_DataHarmonization",
                       "HOM_DataHarmonizationCor", "POM_DataHarmonizationCor")
  # Arguments check ---------------------------------------------------------

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

  # Diameter column exists
  if(!"Diameter" %in% names(Data))
    stop("The 'Diameter' column does't exist in the dataset")

  # DefaultHOM/Min-MaxDBH/Positive-Negative-PioneersGrowthThreshold/DBHRange/MinIndividualNbr (numeric, 1 value)
  if(!all(unlist(lapply(list(DefaultHOM,
                             PositiveGrowthThreshold, NegativeGrowthThreshold, PioneersGrowthThreshold,
                             DBHRange, MinIndividualNbr),
                        length)) %in% 1) |
     !all(unlist(lapply(list(PositiveGrowthThreshold, NegativeGrowthThreshold, DefaultHOM, PioneersGrowthThreshold),
                        inherits, c("numeric", "integer")))))
    stop("The 'PositiveGrowthThreshold', 'NegativeGrowthThreshold', 'PioneersGrowthThreshold' and 'DefaultHOM' arguments
         of the 'DiameterCorrection' function must be 1 numeric value each")

  # AddMissedRecruits
  if(!inherits(AddMissedRecruits, "logical") & !is.null(AddMissedRecruits))
    stop("'AddMissedRecruits' argument must be logical")

  # AddMissedStems
  if(!inherits(AddMissedStems, "logical") & !is.null(AddMissedStems))
    stop("'AddMissedStems' argument must be logical")

  # Pioneers (characters vector)
  if(!inherits(Pioneers, "character") & !is.null(Pioneers))
    stop("'Pioneers' argument must be a characters vector, or NULL")

  # WhatToCorrect
  WhatToCorrect <- match.arg(WhatToCorrect, several.ok = TRUE)

  # CorrectionType
  CorrectionType <- match.arg(CorrectionType)


  # Taper before if 'HOM' in the dataset and 'UseTaperCorrection' = F
  if(!UseTaperCorrection & length(unique(na.omit((Data$HOM)))) > 1) {# HOM exist and UseTaperCorrection FALSE
    message("You have the 'HOM' information in your dataset.
            We advise you to correct your diameters also with UseTaperCorrection = TRUE") # only show if there are varying HOM
  }

  # can't do taper if all HOM are NA
  if(all(is.na(Data$HOM)) & UseTaperCorrection) {
    AllWarnings <- c(AllWarnings, "All your HOM are NA, so we can't do taper corrections")
    UseTaperCorrection = FALSE
  }

  # If 'POM' 'POM change' correction is advised
  if((all(is.na(Data$HOM)) | !"HOM" %in% names(Data)) &
     any(!is.na(Data$POM)) & !"POM change" %in% WhatToCorrect) # POM exists?
    message("You have the 'POM' information in your dataset.
            We advise you to correct your diameters also from the 'POM change' ('WhatToCorrect' argument)")

  # 'POM change' correction needs 'POM' or 'HOM' values
  if((!any(c("POM", "HOM") %in% names(Data)) | (all(is.na(Data$POM)) &  all(is.na(Data$HOM)))) & 'POM change' %in% WhatToCorrect )
    stop("You have chosen to make a 'POM change' correction,
       but you do not have the necessary 'POM' or HOM' column in your dataset or they are empty")

  # if 'Pioneers', need ScientificName

  if(!is.null(Pioneers) & PioneersGrowthThreshold != PositiveGrowthThreshold){

    ## ScientificName_DataHarmonizationCor or ScientificName?
    if("ScientificName_DataHarmonizationCor" %in% names(Data)){
      SfcName <- "ScientificName_DataHarmonizationCor"

    }else if(!"ScientificName_DataHarmonizationCor" %in% names(Data) & "ScientificName" %in% names(Data)){
      SfcName <- "ScientificName"

    }else if(!any(c("ScientificName_DataHarmonizationCor", "ScientificName") %in% names(Data)))

      stop("There are no 'ScientificName_DataHarmonizationCor' nor 'ScientificName' column.
           It is not possible to take into account the pioneer character of species in the diameter correction.
             If you do not want to take into account the pioneer character in the diameter correction,
             leave the argument Pioneers = NULL.")

  } # end Pioneers criteria

  if(ThisIsShinyApp) incProgress(1/15)


# set things up for function ----------------------------------------------------------------


  setDT(Data)
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environment


  Data <- unique(Data)   # if there are duplicate rows, delete them

  # Create new columns we need (if not already there)
  if(!"Comment_DataHarmonization" %in% names(Data)) Data[, Comment_DataHarmonization := ""]

  if(!"DiameterCorrectionMeth_DataHarmonization" %in% names(Data)) Data[, DiameterCorrectionMeth_DataHarmonization := ""]

  # If MinDBH is provided, overwrite the one in the data
  if(ifelse(ThisIsShinyApp, !is.na(MinDBH), !is.null(MinDBH))) { # !is.na(MinDBH)  is needed when inside the app
    if(!inherits(MinDBH, c("numeric", "integer"))) stop("MinDBH must be numeric")
    if(length(MinDBH) > 1) stop("MinDBH must be numeric value of length 1")

    Data$MinDBH <- MinDBH
  } else {
    if(!"MinDBH" %in% names(Data)) stop("You don't have MinDBH in your dataset, please provide a MinDBH in the arguments.")
    # -----------------------------------------------------------------------
  }


  # Dataset with the dead trees if no correction wanted for them --------------------------------------------
  if("LifeStatus_DataHarmonizationCor" %in% names(Data)){ Status <- "LifeStatus_DataHarmonizationCor"
  }else if ("LifeStatusCor" %in% names(Data)){ Status <- "LifeStatusCor"
  }else if ("LifeStatus" %in% names(Data)){ Status <- "LifeStatus"
  }else{stop("You have chosen DBHCorForDeadTrees = FALSE.
             To apply this choice the dataset must contain the column
             'LifeStatus_DataHarmonizationCor', 'LifeStatusCor' or 'LifeStatus'")}

  if(DBHCorForDeadTrees == FALSE){
    DeadTrees <- Data[get(Status) == FALSE]
    Data <- Data[get(Status) == TRUE | is.na(get(Status))] # AliveTrees
    # nrow(Data) == nrow(AliveTrees) + nrow(DeadTrees) # to check
  }

  # Remove duplicated measurements per IdCensus because different POM or Date -----------------------------------
  CompleteData <- copy(Data)

  Data <- UniqueMeasurement(Data, KeepMeas = KeepMeas, ID = ID)

  DuplicatedRows <- CompleteData[!Data, on = .NATURAL] # rows removed


  # If no diameter value, write a comment
  # Data[is.na(Diameter), Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, comment = "Missing value in 'Diameter'")]

  if(ThisIsShinyApp) incProgress(1/15)


# Function ----------------------------------------------------------------


  # Taper correction ------------------------------------------------------------------------------------------------------
  if(UseTaperCorrection) {

    Data <- TaperCorrection(Data,
                            DefaultHOM = DefaultHOM,
                            TaperParameter = TaperParameter, TaperFormula = TaperFormula)

    # if there is a POM column, also bring that to the first value so no risk to readjust that again
    if("POM" %in% names(Data))  Data[, POM_DataHarmonizationCor := .SD[1, POM], by = c(ID)]
  }

  if(ThisIsShinyApp) incProgress(1/15)

  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[order(get(ID), IdCensus)]

  # IDs vector --------------------------------------------------------------------------------------------------------
  Ids <- as.vector(na.omit(unique(Data[, get(ID)]))) # Tree Ids

  # Dataset with the rows without IDS ----------------------------------------------------------------------------------
  DataIDNa <- Data[is.na(get(ID))]

  # Dataset with the rows without IdCensus ----------------------------------------------------------------------------------
  DataIdCensusNa <- Data[is.na(IdCensus)]

  if(ThisIsShinyApp) incProgress(1/15)

  # Apply Corrections -----------------------------------------------------------------------------------------------


  if(length(na.omit(unique(Data$IdCensus))) > 1) { # only possible if more than one census

    if(!"Diameter_DataHarmonizationCor" %in% names(Data)) {
      Data[, Diameter_DataHarmonizationCor := Diameter]
    }

    if(!"HOM_DataHarmonizationCor" %in% names(Data)) {
      Data[, HOM_DataHarmonizationCor := HOM]
    }
    if(!"POM_DataHarmonizationCor" %in% names(Data)){
      Data[, POM_DataHarmonizationCor := POM]
    }
    if(!"LifeStatus_DataHarmonizationCor" %in% names(Data)) {
      Data[, LifeStatus_DataHarmonizationCor := LifeStatus]
    } else {
      if(AddMissedStems) Data <- Data[!Comment_DataHarmonization %in% "Missed stem", ] # remove those as they will be added again
    }
    if(!"DiameterCorrectionMeth_DataHarmonization" %in% names(Data)) {
      Data[, DiameterCorrectionMeth_DataHarmonization := ""]
    }
    if(!"Diameter_DataHarmonizationCor" %in% names(Data)) {
      Data[, Diameter_DataHarmonizationCor := ""]

    }


    # get a DiameterHistory
    DiameterHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "Diameter_DataHarmonizationCor", drop = FALSE)
    DiameterHistory <- as.matrix(DiameterHistory, 1)


    # get HOMHistory
    HOMHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "HOM_DataHarmonizationCor", drop = FALSE)
    HOMHistory <- as.matrix(HOMHistory, 1)

    # get HOMChange History
    HOMChangeHistory <- cbind(NA, t(apply(HOMHistory, 1, diff)))
    colnames(HOMChangeHistory) <- colnames(HOMHistory)

    # get POMHistory
    POMHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "POM_DataHarmonizationCor", drop = FALSE)
    POMHistory <- as.matrix(POMHistory, 1)

    # get POMChange History
    POMChangeHistory <-  t(apply(POMHistory, 1, function(x) x != data.table::shift(x, type = "lag")))

    # get DateHistory (to be able to calculate growth)
    DateHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "Date", drop = FALSE)
    DateHistory <- as.matrix(DateHistory, 1)

    # get a MinDBHHistory (this is useful if not same threshold accross plots)
    MinDBHHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "MinDBH", drop = FALSE)
    MinDBHHistory <- as.matrix(MinDBHHistory, 1)
    MinDBHHistory[] <- apply(MinDBHHistory, 1, function(x) x[is.na(x)] <- as.numeric(names(which.max(table(x))))) # fill NA with most common MinDBH in the row . this could be a problem if MinDBH changes accross censuses but should be rare enough that it does not matter

    # get LifeStatusHistory
    LifeStatusHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "LifeStatus_DataHarmonizationCor", drop = FALSE)
    LifeStatusHistory <- as.matrix(LifeStatusHistory, 1)

    #get BeforeRecruitHistory
    ## this is to figure out what was missed at an estimated dbh>minDBH at the head of the stem history
    BeforeRecruitHistory <- LifeStatusHistory
    BeforeRecruitHistory [] <- t(apply(LifeStatusHistory, 1, function(x) {
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

    # create Comment History
    CommentHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "Comment_DataHarmonization", drop = FALSE)
    CommentHistory <- as.matrix(CommentHistory, 1)
    CommentHistory[is.na(CommentHistory)] <- ""
    # CommentHistory <- matrix("", nrow(DiameterHistory), ncol(DiameterHistory), dimnames = dimnames(DiameterHistory))

    DiameterCorrectionMethHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "DiameterCorrectionMeth_DataHarmonization", drop = FALSE)
    DiameterCorrectionMethHistory <- as.matrix(DiameterCorrectionMethHistory, 1)
    DiameterCorrectionMethHistory[is.na(DiameterCorrectionMethHistory)] <- ""
    # DiameterCorrectionMethHistory <- matrix("", nrow(DiameterHistory), ncol(DiameterHistory), dimnames = dimnames(DiameterHistory))

    # small correction affecting next histories
    # DBH = 0 is impossible
    DiameterHistory[DiameterHistory %in% 0] <- NA
    # DBH > MaxDBH -> DBH = NA
    # DiameterHistory[DiameterHistory > MaxDBH] <- NA


    # get Plot, Subplot, Family, Genus and Specie in an array
    UniqueInfo <- Data[, .(Plot = unique(Plot),
                           Subplot = unique(Subplot),
                           Family = unique(Family),
                           Genus = unique(Genus),
                           Species = unique(ScientificName)), by = .(IdStem =get(ID))]

    UniqueInfo <- UniqueInfo[complete.cases(UniqueInfo),] # removing when for some reason one of these info is not specified (which deals with most duplicated)

    if(any(duplicated(UniqueInfo$get))) stop("Some individuals don't have a unique Plot, Family, Genus or species")

    for(w in c("Plot", "Subplot", "Family", "Genus", "Species")) {
      x <- UniqueInfo[, get(w)]
      names(x) <- UniqueInfo$IdStem
      x <- x[rownames(DiameterHistory)] # make sure same order as other objecys
      assign(w, x)
    } # make one object for each info (will help later)



    if(ThisIsShinyApp) incProgress(1/15)


    ## fill in date when it is NA, by most common of date in that subplot
    ### this is to be able to interpolate outside of measured dates, and detect missed recruits
    idxToFill <- which(is.na(DateHistory), arr.ind = T)
    DateLookup <- by(DateHistory, Subplot, function(x) apply(x, 2, function(y) names(sort(table(y),decreasing = T))[1]))

    if(nrow(idxToFill) > 0) {
        DateHistory[is.na(DateHistory)] <- mapply(function(x, y) DateLookup[[Subplot[x]]][y], x = idxToFill[,1], y = idxToFill[, 2] )
    }


    # get DateDiff
    DateDiff <- matrix(NA, ncol = ncol(DateHistory), nrow = nrow(DateHistory), dimnames = dimnames(DiameterHistory))
    DateDiff[] <- t(apply(DateHistory, 1, function(x) (as.Date(x) - data.table::shift(as.Date(x)))/365))


    if(ThisIsShinyApp) incProgress(1/15)

    # fill in the Diameter when the tree was missed

    MissedDiametetFilled <- t(mapply(function(d, t, c, cm, l, r, md) {

      if(any(!is.na(d))) { # only run if we have diameters to work with
        t <- as.Date(t)
        m <- lm(d~t)

        if(!is.na(coef(m)["t"])) { # if there is at least 2 diameters and corresponding dates... if not, no regression can be done
          p <- stats::predict(lm(d~t), newdata = t) # if some t are NA, still i twil

          # missed measurement (between censuses)
          idx = is.na(d) & !is.na(p) & l %in% TRUE

          c[idx] <- GenerateComment(c[idx], "Missed measurement")
          cm[idx] <- GenerateComment(cm[idx], "Initial linear interpolation")
          d[idx] <- p[idx]

          # missed stem completely (between censuses)
          idx = is.na(d) & !is.na(p) & l %in% NA & r %in% 0
          c[idx] <- GenerateComment(c[idx], "Missed stem")
          cm[idx] <- GenerateComment(cm[idx], "Initial linear interpolation")
          d[idx] <- p[idx]

          # missed recruits (before stem was censused)
          idx = is.na(d) & !is.na(p) & l %in% NA & r %in% 1 & p > md

          c[idx] <- GenerateComment(c[idx], "Missed recruit")
          cm[idx] <- GenerateComment(cm[idx], "Initial linear interpolation")
          d[idx] <- p[idx]



        }
      }


      return(list(d, c, cm))

    },
    d = split(DiameterHistory, row(DiameterHistory)),
    t = split(DateHistory, row(DateHistory)),
    c = split(CommentHistory, row(CommentHistory)),
    cm = split(DiameterCorrectionMethHistory, row(DiameterCorrectionMethHistory)),
    l = split(LifeStatusHistory, row(LifeStatusHistory)),
    r = split(BeforeRecruitHistory, row(BeforeRecruitHistory)),
    md = split(MinDBHHistory, row(MinDBHHistory))))

    DiameterHistory[] <- do.call(rbind, MissedDiametetFilled[,1])
    CommentHistory[] <-  do.call(rbind, MissedDiametetFilled[,2])
    DiameterCorrectionMethHistory[] <-  do.call(rbind, MissedDiametetFilled[,3])

    # create a function that will allow to get diameter difference and growth history (because we will need to recalculate those a few times, as we make corrections)
    CalcGrowthHist <- function(DiameterHistory, DateHistory) {
      x <-  matrix(NA, ncol = ncol(DiameterHistory), nrow = nrow(DiameterHistory), dimnames = dimnames(DiameterHistory))
      x[,-1] <- t(round(apply(DiameterHistory, 1, diff) / apply(DateHistory, 1, function(x) diff(as.Date(x))/365), 2))
      return(x[])
    }

    CalcDiameterDiffHist <- function(DiameterHistory = DiameterHistory) {
      x <-  matrix(NA, ncol = ncol(DiameterHistory), nrow = nrow(DiameterHistory), dimnames = dimnames(DiameterHistory))
      x[,-1] <- t(round(apply(DiameterHistory, 1, diff),2))
      return(x[])
    }

    if(ThisIsShinyApp) incProgress(1/15)

    # get growth History (for annual growth incrementation)
    GrowthHistory <- CalcGrowthHist(DiameterHistory = DiameterHistory, DateHistory = DateHistory)

    # get Growth difference (for absolute growth incrementation)
    # DiameterDiffHistory <- CalcDiameterDiffHist(DiameterHistory) Valentine decided to use GrowthHistory instead of DiameterDiffHistory



    if(ThisIsShinyApp) incProgress(1/15)

    # calculate weights to use with "individual correction"
    ## For each Census, compute the absolute time difference and use coefs to calculate weight of the growths by temporal proximity

    Weights <- lapply(1:ncol(DateHistory), function(j) matrix(exp(as.numeric(abs(as.Date(DateHistory) - as.Date(DateHistory[,j])))/365*-coef), nrow = nrow(DiameterHistory), ncol = ncol(DiameterHistory), dimnames = dimnames(DiameterHistory))) # list of length equal to number of censuses

    if(ThisIsShinyApp) incProgress(1/15)

    # Corrections ####
    Idx_enough_DBH <- rowSums(!is.na(DiameterHistory)) > 1
    Idx_one_DBH <- rowSums(!is.na(DiameterHistory)) %in% 1



    # detect and change to NA the growth of cases of abnormal increment

    ## positive

    if(!is.null(Pioneers)) {

      ### pioneers
      idx_sp <- Species %in% Pioneers
      idx = !is.na(GrowthHistory[idx_sp, ]) & GrowthHistory[idx_sp, ] >= PioneersGrowthThreshold

      CommentHistory[idx_sp, ][idx] <- GenerateComment(CommentHistory[idx_sp, ][idx], paste("Growth greated than threshold of", PioneersGrowthThreshold))
      GrowthHistory[idx_sp, ][idx]  <- NA
      # DiameterDiffHistory[idx_sp, ][idx] <- NA

      ### non-pioneers
      idx_sp <- !Species %in% Pioneers
      idx = !is.na(GrowthHistory[idx_sp, ]) & GrowthHistory[idx_sp, ] >= PositiveGrowthThreshold

      CommentHistory[idx_sp, ][idx] <- GenerateComment(CommentHistory[idx_sp, ][idx], paste("Growth greated than threshold of", PositiveGrowthThreshold))
      GrowthHistory[idx_sp, ][idx]  <- NA
      # DiameterDiffHistory[idx_sp, ][idx] <- NA

    } else {

      idx = !is.na(GrowthHistory) & GrowthHistory >= PositiveGrowthThreshold

      CommentHistory[idx] <- GenerateComment(CommentHistory[idx], paste("Growth greated than threshold of", PositiveGrowthThreshold))
      GrowthHistory[idx]  <- NA
      # DiameterDiffHistory[idx] <- NA
    }


    ## negative
    idx = !is.na(GrowthHistory) & GrowthHistory < NegativeGrowthThreshold # Valentine decided to use GrowthHistory instead of DiameterDiffHistory

    CommentHistory[idx] <- GenerateComment(CommentHistory[idx], paste("Growth smaller than threshold of", NegativeGrowthThreshold))
    GrowthHistory[idx]  <- NA
    # DiameterDiffHistory[idx] <- NA


    if(ThisIsShinyApp) incProgress(1/15)

    if("POM change" %in% WhatToCorrect){

      if(all(is.na(HOMHistory)) & all(is.na(POMHistory)))  stop("You have chosen to make a 'POM change' correction,
        but 'POM' and HOM' columns are empty for all trees so we can't apply corrections.")
    }

    if(ThisIsShinyApp) incProgress(1/15)

    ## POM change detection -----------------------------------------------------------------------------------------------

    # Check HOM value over time

    idx = !is.na(HOMChangeHistory) & HOMChangeHistory < 0
    CommentHistory[idx] <- GenerateComment(CommentHistory[idx], "HOM decreased")

    idx = !is.na(HOMChangeHistory) & HOMChangeHistory > 0
    CommentHistory[idx] <- GenerateComment(CommentHistory[idx], "HOM increased")

    # Check POM value over time
    CommentHistory[POMChangeHistory %in% TRUE] <- GenerateComment(CommentHistory[idx], "POM changed")


    # detect any change in HOM or POM
    idxPOMChange <- !is.na(HOMChangeHistory) & !HOMChangeHistory %in% 0 | POMChangeHistory %in% TRUE

    # Remove growth between shifts (take growth only intra seq)
    GrowthHistory[idxPOMChange]  <- NA
    # DiameterDiffHistory[idxPOMChange] <- NA

    # get indexes to replace
    idxPOMChange <- is.na(GrowthHistory) & idxPOMChange
    idxToReplace <- suppressWarnings(cbind(which(idxPOMChange, arr.ind = T), what = 1)) # 1 is for POM Change, 2 is for abnormal growth


    if(ThisIsShinyApp) incProgress(1/15)

    ## Abnormal growth detection (punctual and shift combined) -----------------------------------------------------------------------------------------------

    # get indexes to replace
    idxAbnormal <- idxPOMChange # this is to help maintaining the structure
    idxAbnormal[] <- grepl(paste("Growth greated than threshold", "Growth smaller than threshold", sep = "|"), CommentHistory) & !grepl(paste("HOM decreased", "HOM increased", "POM changed", sep = "|"), CommentHistory)

    idxToReplace <-  rbind(idxToReplace, suppressWarnings(cbind(which(idxAbnormal, arr.ind = T), what = 2))) # 1 is for POM Change, 2 is for shift or punctual




    ## get the sign of growth ---------------------------------------------
    if(nrow(idxToReplace) > 0) {
      idxToReplace <- cbind(idxToReplace, sign = NA)
      idxToReplace[idxToReplace[, 3] %in% 1, "sign"] <-  sign(CalcGrowthHist(DiameterHistory = DiameterHistory, DateHistory = DateHistory)[idxPOMChange])
      idxToReplace[idxToReplace[, 3] %in% 2, "sign"] <-  sign(CalcGrowthHist(DiameterHistory = DiameterHistory, DateHistory = DateHistory)[idxAbnormal])

    }

    ## order to make sure we are looking at each stem successively
    idxToReplace <- idxToReplace[order(idxToReplace[,1], idxToReplace[,2]),]

    if(ThisIsShinyApp) incProgress(1/15)

    # ## Leading NA in status (to see if we may be missing recruitments) -----------------------------------------------------------------------------------------------
    #
    # # get indexes to replace
    # idxLeadingNAs <- idxPOMChange # this is to help maintaining the structure
    # idxLeadingNAs[] <- LeadingNAHistory %in% 1
    #
    # idxToReplace <-  rbind(idxToReplace, suppressWarnings(cbind(which(idxLeadingNAs, arr.ind = T), what = 3, # 1 is for POM Change, 2 is for shift or punctual, 3 is for recruitment
    #                                                             sign = sign(CalcGrowthHist(DiameterHistory = DiameterHistory, DateHistory = DateHistory)[idxLeadingNAs])))) # get the sign of the shift
    # CAREFUL IF WE BRING THIS BACK IN THE FUNCTION WE NEED TO CHANGE idxToReplace <- idxToReplace[idxToReplace[, 2]> 1, ] to idxToReplace <- idxToReplace[idxToReplace[, 2]> 1 | idxToReplace[, 2] %in% 3, ] + keep working on the corrections(need to go backward instead of forward)


    ## Corrections -------------------------------------------------------------


    # remove cases where NA is in first column since that is fake data

    idxToReplace <- idxToReplace[idxToReplace[, 2]> 1, , drop = F]

    # remove cases that were not selected
    if(!"POM change" %in% WhatToCorrect) idxToReplace <- idxToReplace[!idxToReplace[,3] %in% 1, ]
    if(!"Abnormal growth" %in% WhatToCorrect) idxToReplace <- idxToReplace[!idxToReplace[,3] %in% 2, ]

    # replace Diameters that need it
    if(nrow(idxToReplace) > 0) {

      lTosktip = 0 # this is a machanism to skip idxToReplace that are return to normal

      for(l in 1:nrow(idxToReplace)) {
         if(l == lTosktip) next

        t = rownames(idxToReplace)[l]
        i = idxToReplace[l, 1]
        j = idxToReplace[l, 2]

        if(l > 1) if(idxToReplace[l, 3] %in% 2 & idxToReplace[l-1, 3] %in% 2 & t %in% rownames(idxToReplace)[l-1] & (j-1) %in% idxToReplace[l-1, 2] & idxToReplace[l,4] + idxToReplace[l-1,4] == 0) next # ignore this correction if the previous fix was a punctual error measurement and this abnormal growth is just the return to "normal"

        pd = DiameterHistory[i,j-1] # previous dbh

        SwitchToIndividual = FALSE # this will be turned to TRUE if CorrectionType %in% "phylogenetic hierarchical" but there are not enough colleagues

        if(CorrectionType %in% "phylogenetic hierarchical") {
          # find potential Colleagues


          idxSamePlot <- Plot %in% Plot[t]
          idxSameFamily <- Family %in% Family[t]
          idxSameGenus <- Genus %in% Genus[t]
          idxSameSpecies <- Species %in% Species[t]

          idxDBHWithinRange <- !is.na(DiameterHistory) & (DiameterHistory > (pd - DBHRange/2) & DiameterHistory < (pd + DBHRange/2))

          idxGrowthExists <- !is.na(GrowthHistory)

          # figure out set of colleague (need at least MinIndividualNbr, with actual growth measures)

          idxColleagues <- list(Species = idxSameSpecies & idxDBHWithinRange & idxGrowthExists,
                                Genus = idxSameGenus & idxDBHWithinRange & idxGrowthExists,
                                Family = idxSameFamily & idxDBHWithinRange & idxGrowthExists,
                                Plot = idxSamePlot & idxDBHWithinRange & idxGrowthExists)


          Method <- names(which(lapply(idxColleagues, sum, na.rm = T) > MinIndividualNbr))[1] # take the first set that meets the min requirement

          if(!is.na(Method)) { # if we can use phylo correction
            idxColleagues <- idxColleagues[[Method]]

            # compute Colleagues growth mean
            wg <- mean(GrowthHistory[idxColleagues])

            DiameterCorrectionMethHistory[i, j] <- GenerateComment( DiameterCorrectionMethHistory[i, j], paste(Method, "phylogenetic growth mean"))

            SwitchToIndividual = FALSE
          } else {
            #   stop("Not enough individuals in your dataset to apply the 'phylogenetic hierarchical' correction even at the 'stand' level.
            #              You asked for a minimum of ", MinIndividualNbr," individuals ('MinIndividualNbr' argument).
            #               The 'individual' correction is applied in this case.")
            # }
            SwitchToIndividual = TRUE
          }

        }

        if(CorrectionType %in% "individual" | SwitchToIndividual) {


          w = Weights[[j]][t, ] # weight for a NA in that census, for that tree

          g = GrowthHistory[i,] # growth history of that tree

          wg <- weighted.mean(g, w, na.rm = T) # weighted mean of growth for that tree at that census

          DiameterCorrectionMethHistory[i, j] <- GenerateComment( DiameterCorrectionMethHistory[i, j], "Weighted mean")
        }



        do = DiameterHistory[i,j] # original DBH of that IdCensus

        DiameterHistory[i,j] <- pd + wg * DateDiff[i, j]

        dn = DiameterHistory[i,j] # new DBH of that IdCensus



        # apply switch to other values if j is not last column and l+1 in idxToReplace is not of same tree with opposite sign (which would indicated a punctual errro measurement)
        shift = FALSE # initialize with no shift
        lTosktip = 0
        if(j < ncol(DiameterHistory)) {
          if(l == nrow(idxToReplace)) shift = TRUE # if this is the last abnormal growth, we know we need to shift because we already skipped it if it was a return to normwl
          if(idxToReplace[l, 3] %in% 1 )  shift = TRUE # POM change automatically shifts
          if(idxToReplace[l, 3] %in% 2) {
            if(!t %in% rownames(idxToReplace)[l+1]) shift = TRUE # next error is not in the same tree s nothing is compensating --> need to shift
            if(t %in% rownames(idxToReplace)[l+1]) {
              if(idxToReplace[l+1, 3] %in% 2 & (j+1) %in% idxToReplace[l+1, 2]  & idxToReplace[l,4] + idxToReplace[l+1,4] != 0 )  shift = TRUE # if next error is not compensating this error
              else lTosktip = l+1 # if next is compensating, we will want to remove it from idxToReplace
              }
            }

          }


        if(shift) {
          DiameterHistory[i,(j+1):ncol(DiameterHistory)] <- DiameterHistory[i,(j+1):ncol(DiameterHistory)] + dn - do

          DiameterCorrectionMethHistory[i,(j+1):ncol(DiameterHistory)]  <- GenerateComment( DiameterCorrectionMethHistory[i,(j+1):ncol(DiameterHistory)] , paste("Shift realignment after", c("POM change", "Abnormal growth")[idxToReplace[l, 3]]))
        }

        if(ThisIsShinyApp) incProgress(1/15)

      }



    }


    # If not enough Diameters or growth to help? --------------------------------------------------


    # Check that there are no more abnormal growths -----------------------------------------------------------------------------

    GrowthHistory <- CalcGrowthHist(DiameterHistory = DiameterHistory, DateHistory = DateHistory)

    ## positive

    if(!is.null(Pioneers)) {

      ### pioneers
      idx_sp <- Species %in% Pioneers
      idx = !is.na(GrowthHistory[idx_sp, ]) & GrowthHistory[idx_sp, ] >= PioneersGrowthThreshold

      if(sum(idx)>1)        AllWarnings <- c(AllWarnings, "There are still pioneers with abnormal positive growth (the selected methods are insufficient
                    or the method needs to be improved)")

      ### non-pioneers
      idx_sp <- !Species %in% Pioneers
      idx = !is.na(GrowthHistory[idx_sp, ]) & GrowthHistory[idx_sp, ] >= PositiveGrowthThreshold

      if(sum(idx)>1)   AllWarnings <- c(AllWarnings, "There are still non-pioneers with abnormal positive growth (the selected methods are insufficient
                    or the method needs to be improved)")

    } else {

      idx = !is.na(GrowthHistory) & GrowthHistory >= PositiveGrowthThreshold
      if(sum(idx)>1)   AllWarnings <- c(AllWarnings, "There are still individuals with abnormal positive growth (the selected methods are insufficient
                    or the method needs to be improved)")

    }

    ## negative
    idx = !is.na(GrowthHistory) & GrowthHistory < NegativeGrowthThreshold # Valentine decided to use GrowthHistory instead of DiameterDiffHistory
    if(sum(idx)>1)   AllWarnings <- c(AllWarnings, "There are still individuals with abnormal negative growth (the selected methods are insufficient
                    or the method needs to be improved)" )
    if(ThisIsShinyApp) incProgress(1/15)

    # Write changes in Data -------------------------------------------------------------------------------------------

    # write comments
    CommentHistoryCorrected <- melt(setDT(as.data.frame(CommentHistory), keep.rownames=TRUE), measure.vars = colnames(CommentHistory) , variable.name = "IdCensus")
    # CommentHistoryCorrected <- CommentHistoryCorrected[!value %in% "", ]

    idx <- match(paste(Data[,get(ID)], Data[,IdCensus]), paste(CommentHistoryCorrected$rn, CommentHistoryCorrected$IdCensus))

    Data$Comment_DataHarmonization <- CommentHistoryCorrected$value[idx]


    # write methods
    DiameterCorrectionMethHistoryCorrected <- melt(setDT(as.data.frame(DiameterCorrectionMethHistory), keep.rownames=TRUE), measure.vars = colnames(DiameterCorrectionMethHistory) , variable.name = "IdCensus")
    # DiameterCorrectionMethHistoryCorrected <- DiameterCorrectionMethHistoryCorrected[!value %in% "", ]

    idx <- match(paste(Data[,get(ID)], Data[,IdCensus]), paste(DiameterCorrectionMethHistoryCorrected$rn, DiameterCorrectionMethHistoryCorrected$IdCensus))

    Data$DiameterCorrectionMeth_DataHarmonization <- ifelse(is.na(DiameterCorrectionMethHistoryCorrected$value[idx]), "", DiameterCorrectionMethHistoryCorrected$value[idx])


    # write DBH corrected
    DiameterCorrected <- melt(setDT(as.data.frame(DiameterHistory), keep.rownames=TRUE), measure.vars = colnames(DiameterHistory) , variable.name = "IdCensus")

    idx <- match(paste(Data[,get(ID)], Data[,IdCensus]), paste(DiameterCorrected$rn, DiameterCorrected$IdCensus))

    Data$Diameter_DataHarmonizationCor <- as.numeric(mapply(function(x,y){as.character(round(x,y))}, DiameterCorrected$value[idx], nchar(gsub(".*\\d*\\.", "", prettyNum(Data$Diameter, drop0trailing = F))))) # rounding to the same n digits as original diameter (way more complicated that what you would think!)

    if(ThisIsShinyApp) incProgress(1/15)



# Add new rows where needed -----------------------------------------------

    DateHistoryCorrected <- melt(setDT(as.data.frame(DateHistory), keep.rownames=TRUE), measure.vars = colnames(DateHistory) , variable.name = "IdCensus")


    for(addrows in c("AddMissedRecruits", "AddMissedStems")) {

      if(addrows %in% "AddMissedStems" & !AddMissedStems) next


      if(addrows %in% "AddMissedRecruits")  pattern <- "Missed recruit"
      if(addrows %in% "AddMissedStems")  pattern <- "Missed stem"


      missedComment <- CommentHistoryCorrected[grepl(pattern, CommentHistoryCorrected$value), ]
      missedMethod <- DiameterCorrectionMethHistoryCorrected[grepl(pattern, CommentHistoryCorrected$value), ]
      missedDiameter <- DiameterCorrected[grepl(pattern, CommentHistoryCorrected$value), ]
      missedDate <- DateHistoryCorrected[grepl(pattern, CommentHistoryCorrected$value), ]


      idx_new_rows <- which(match(paste(missedComment$rn, missedComment$IdCensus), paste(Data[,get(ID)], Data[,IdCensus])) %in% NA)

      if(length(idx_new_rows) > 0) {

      # find out which rows in Data should get a comment to indicate the previous census should have recruited this tree already (adding this comment only when OnlyDetectMissedRecruits)
      idx_CommentOnly <- match(apply(missedComment[, levels(missedComment$IdCensus)[max(as.numeric(IdCensus)) +1], by = rn], 1, paste, collapse = " "), paste(Data[, get(ID)], Data$IdCensus))
      idx_CommentOnly <- idx_CommentOnly[!duplicated(Data[,get(ID)][idx_CommentOnly])]
      idx_CommentOnly <- idx_CommentOnly[!is.na(idx_CommentOnly)]

      if(addrows %in% "AddMissedRecruits" & !AddMissedRecruits)  Data[idx_CommentOnly, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "This tree should have been recruited earlier based on its growth and your protocol (MinDBH)")]

      if(get(addrows)) {

        # get what we know about those trees
        NewRows <- Data[get(ID) %in% missedComment[idx_new_rows, rn], ]
        NewRows <- NewRows[!duplicated(get(ID)), ]

        # remove the data that changes at each census
        NewRows[,setdiff(names(NewRows), InvariantColumns) := NA]

        # repeat each row the number of times necessary
        nreps <- table(missedComment[idx_new_rows, rn])

        NewRows <- NewRows[rep(1:nrow(NewRows), times = nreps[match(NewRows[,get(ID)], names(nreps))]), ]


        # fill in info we know about the census and based on the Status we corrected
        m <- order(missedComment[idx_new_rows, rn])
        NewRows$IdCensus <- missedComment[idx_new_rows, IdCensus][m]

        NewRows$Comment_DataHarmonization <- missedComment[idx_new_rows, value][m]

        NewRows$DiameterCorrectionMeth_DataHarmonization <- missedMethod[idx_new_rows, value][m]

        NewRows$Diameter_DataHarmonizationCor <- missedDiameter[idx_new_rows, value][m]

        NewRows$Date <- as.Date(missedDate[idx_new_rows, value][m])

        NewRows$LifeStatus_DataHarmonizationCor <- TRUE

        NewRows$MissedTree_DataHarmonizationCor <- TRUE
        Data$MissedTree_DataHarmonizationCor <- FALSE

        # make best guess at other things


        NewRows$Year <- format(NewRows$Date, "%Y")
        NewRows$Month <- format(NewRows$Date, "%m")
        NewRows$Day <- format(NewRows$Date, "%d")

        # Add these rows in the dataset
        Data <- rbindlist(list(Data, NewRows), use.names=TRUE, fill=TRUE)
        if(addrows %in% "AddMissedRecruits")  {
          AllWarnings <- c(AllWarnings, "We added rows for trees that were supposed to be recruited earlier based on linear extrapolation of growth and MinDBH")
          NewRows$MissedRecruit_DataHarmonizationCor <- TRUE
          Data$MissedRecruit_DataHarmonizationCor <- FALSE
        }
        if(addrows %in% "AddMissedStems" ) {
          AllWarnings <- c(AllWarnings, "We added rows for trees that were missed and estimated their missed DBH by linear interpolation")
          NewRows$MissedStem_DataHarmonizationCor <- TRUE
          Data$MissedStem_DataHarmonizationCor <- FALSE
        }

      }


}
     if(ThisIsShinyApp) incProgress(1/15)
     }



    # Re-put the rows duplicated, or without ID or IdCensus -----------------------------------------------------------------
    DuplicatedRows[, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "Duplicated measurement")]
    DuplicatedRows[, DiameterCorrectionMeth_DataHarmonization := GenerateComment(DiameterCorrectionMeth_DataHarmonization, "Not processed")]

    DataIDNa[, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "Missing ID.")]
    DataIDNa[, DiameterCorrectionMeth_DataHarmonization := GenerateComment(DiameterCorrectionMeth_DataHarmonization, "Not processed.")]

    DataIdCensusNa[, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "Missing IdCensus")]
    DataIdCensusNa[, DiameterCorrectionMeth_DataHarmonization := GenerateComment(DiameterCorrectionMeth_DataHarmonization, "Not processed")]

    Data <- rbindlist(list(Data, DuplicatedRows, DataIDNa, DataIdCensusNa), use.names = TRUE, fill = TRUE)

    # Re-put the dead trees in the dataset (if there are not corrected by choice)
    if(DBHCorForDeadTrees == FALSE){
      Data <- rbindlist(list(Data, DeadTrees), use.names = TRUE, fill = TRUE)
    }

    if(ThisIsShinyApp) incProgress(1/15)

    # Order IDs and times in ascending order ----------------------------------------------------------------------------
    Data <- Data[order(get(ID), IdCensus)]
  } else {
    AllWarnings <- c(AllWarnings, "You only have one census so we can only apply Taper Corrections (if you have HOM).")
  }


  # show warnings
  if(!is.null(AllWarnings)) warning(paste(AllWarnings, collapse = "\n"))

  if(ThisIsShinyApp) incProgress(1/15)

  # return Data
  return(Data)

}
