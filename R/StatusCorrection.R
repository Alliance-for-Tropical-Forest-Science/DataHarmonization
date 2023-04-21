#' Status Correction
#'
#' @description Detect errors, or detect errors and correct, the tree life
#'   status evolution over the censuses.
#'   Inspired by the code of Nino Page package (ForestData::correct_alive() and
#'   .correct_alive_tree())
#'
#' @param Data Dataset (data.frame or data.table)
#'   The *LifeStatus* column must be coded as:
#'     - TRUE = alive,
#'     - FALSE = dead,
#'     - NA = unseen
#'
#' @param DeathConfirmation Number of times (censuses) needed for an unseen tree
#'   to be considered dead (numeric)  (Default = 2 censuses)
#'
#' @param UseSize Use the size presence  (> min DBH) as a witness of the living status of the
#'   tree (logical) (Default = FALSE)
#'
#' @param KeepMeas In case of **multiple diameter measurements** in the same
#'   census year:
#' Possible values: "MaxHOM", "MaxDate" (character).
#'   - "MaxHOM": apply the correction to the measurement taken at the
#'               **highest POM**
#'   - "MaxDate": apply the correction to the **most recent measurement** (same
#'                year but more recent date)
#'
#' @param AddRowsForForgottenCensuses TRUE: adds rows for forgotten censuses, FALSE: does not add any rows (logical).
#'
#'
#' @details
#' - if UseSize : if Diameter != NA -> Alive
#' If (the value in bold is modified by the value given after the arrow):
#' (the ">" gives the chronological order of the sequence)
#' - *Dead* > Alive -> NA
#' - add rows for the forgotten censuses between 2 'Alive' if chosen
#' - Alive > *Dead*/*NA* > Alive -> Alive
#' - Alive > *NA* > Dead -> NA
#' - Alive > *Dead* > NA -> Dead
#'
#' - Alive > *NA* > *NA*:
#'   if DeathConfirmation > unseens -> NA
#'   if DeathConfirmation =< unseens -> Dead
#'
#' @return Fill the *Comment_DataHarmonization* column with error type information and add
#'  a *LifeStatus_DataHarmonizationCor* column with corrected trees life status. If AddRowsForForgottenCensuses is TRUE, a column *MissedStem_DataHarmonizationCor* is added with value TRUE for new rows.
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom stats na.omit
#' @importFrom shiny incProgress
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data(TestData)
#'
#' selection <- c("101184", "101433","101435","101436")
#'
#'# Write the sequence
#' TestData <- TestData[order(IdCensus)] # arrange IdCensus in ascending order
#' TestData[IdTree == "101184", LifeStatus := c(TRUE, TRUE, TRUE, TRUE, FALSE)]
#' TestData[IdTree == "101433", LifeStatus := c(FALSE, TRUE, TRUE, TRUE, TRUE)]
#' TestData[IdTree == "101435", LifeStatus := c(TRUE, TRUE, NA, FALSE, TRUE)]
#' TestData[IdTree == "101436", LifeStatus := c(TRUE, NA, NA, FALSE, NA)]
#'
#'
#' Rslt <- StatusCorrection(TestData[IdTree %in% selection])
#'
#'
#' StatusCorrectionPlot(Rslt)
#'
StatusCorrection <- function(
    Data,
    DeathConfirmation = 2,
    UseSize = FALSE,
    AddRowsForForgottenCensuses = TRUE,
    KeepMeas = c("MaxHOM", "MaxDate")){


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
                       "XTreeSubplot", "YTreeSubplot",
                       "HOM", "POM", "BHOM", "BPOM",
                       "ScientificNameOriginal", "CommercialSpOriginal",
                       "TreeFieldNumOriginal", "IdTreeOriginal", "StemFieldNumOriginal",
                       "IdStemOriginal", "LifeStatusOriginal",
                       "Family_DataHarmonizationCor", "Genus_DataHarmonizationCor", "Species_DataHarmonizationCor",
                       "Determination_rank_DataHarmonizationCor",
                       "Name_DataHarmonizationCor", "ScientificName_DataHarmonizationCor",
                       "StatusCorrectionMeth_DataHarmonization",
                       "HOM_DataHarmonizationCor", "POM_DataHarmonizationCor")

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

  # Plot column exists
  if (!"Plot" %in% names(Data)){
    stop("The column 'Plot' must be present in the dataset
    to add rows to the census where the plot was inventoried, where the tree was alive, but not recorded")
  }


  # DeathConfirmation
  if (!inherits(DeathConfirmation, "numeric"))
    stop("'DeathConfirmation' argument must be numeric")

  # UseSize and AddRowsForForgottenCensuses
  if (!all(unlist(lapply(list(UseSize,AddRowsForForgottenCensuses),
                         inherits, "logical"))))
    stop("The 'UseSize' and 'RemoveRAfterDeath' arguments
         of the 'SatusCorrection' function must be logicals")

  # use corrected columns as InvariantColumns if they exist (mostly for Species names that may have been corrected)

    idx_corrected_columns <- paste0(InvariantColumns, "_DataHarmonizationCor") %in% names(Data)
    InvariantColumns[idx_corrected_columns] <- paste0(InvariantColumns, "_DataHarmonizationCor")[idx_corrected_columns]


  # UseSize-Diameter
  if(UseSize %in% TRUE){ # if it is desired (TRUE) to use the presence of measurement to consider the tree alive
    if (!"Diameter" %in% names(Data)){
      stop("If you wish to use the size presence (UseSize=TRUE) as a witness of the living status of the tree,
           the 'Diameter' column must be present in the dataset")
    }
  }

  #### Function ####

  # data.frame to data.table
  setDT(Data)
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environment

  if(!"Comment_DataHarmonization" %in% names(Data)) Data[, Comment_DataHarmonization:= ""]
  if(!"StatusCorrectionMeth_DataHarmonization" %in% names(Data)) Data[, StatusCorrectionMeth_DataHarmonization:= ""]

  CompleteData <- copy(Data)

  Data <- UniqueMeasurement(Data, KeepMeas = KeepMeas, ID = ID)

  DuplicatedRows <- CompleteData[!Data, on = .NATURAL] # rows removed
  # Data[, (ID) := as.character(get(ID))]


  # Order IDs and times in ascending order
  Data <- Data[order(get(ID), IdCensus), ]

  # IDs vector
  Ids <- as.vector(na.omit(unique(Data[, get(ID)]))) # Tree Ids

  # Dataset with the rows without ID
  DataIDNa <-  Data[is.na(get(ID))]


  # get status history for each ID
  StatusHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "LifeStatus", drop = FALSE)
  StatusHistory <- as.matrix(StatusHistory, 1)


  # get measurement history for each ID (so if UseSize we can know that the tree was alive)

  if(UseSize) {
    MeasHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "Diameter", drop = FALSE)
    MeasHistory <- as.matrix(MeasHistory, 1)

    MinDBHHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "MinDBH", drop = FALSE)
    MinDBHHistory <- as.matrix(MinDBHHistory, 1)


    MeasHistory[] <- ifelse(!is.na(MeasHistory[]) & MeasHistory >= MinDBHHistory[], 1, 0)

  }

  # get comment History
  CommentHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "StatusCorrectionMeth_DataHarmonization", drop = FALSE)
  # rownames(CommentHistory) <- CommentHistory[[1]]
  # CommentHistory[,1] <- NULL
  CommentHistory <- as.matrix(CommentHistory, 1)



  # PlotCensuses
  # if (!inherits(PlotCensuses, c("numeric", "integer")))
  #   stop("'PlotCensuses' argument must be numeric or integer")
  #



  if(UseSize) {
    if(!all(dim(StatusHistory) == dim(MeasHistory) &  dim(StatusHistory) == dim(CommentHistory))) stop(" not all matrix are of same dimentions")
  } else {
      if(!all.equal(dim(StatusHistory), dim(CommentHistory))) stop(" not all matrix are of same dimentions")
    }


  # concatenate
  StatusHistoryC <- StatusHistory
  StatusHistoryC[] <- as.numeric(StatusHistoryC)
  StatusHistoryC <- apply(StatusHistoryC, 1, paste, collapse = "")
  StatusHistoryC <- gsub("NA", "N", StatusHistoryC)

  if(UseSize) {
    MeasHistoryC <- apply(MeasHistory, 1, paste, collapse = "")
  }

  NewCommentsC <- rep(strrep("0", ncol(StatusHistory)), nrow(StatusHistory))



  # detect and correct (if user requested) each scenarios ###
  scenarios <- c("0" = "", # when nothing was corrected
                 A = "A measured tree is a living tree", # Use the size presence as a witness of the living status of the tree
                 B = "Between 2 alive occurrences, the tree was alive", # Alive > *Alive* > Alive
                 C = paste("When a tree is unseen >=", DeathConfirmation, "times, it is considered dead"), # Enough/not enough occurrences of unseen to validate death
                 D = "Tree can't be dead before first being alive", # Dead before first alive
                 E = "A dead tree remains dead" # remains dead after last death
  )

  # scenario A - Use measurement history ####
  if(UseSize) {
    measured <- gregexpr("1", MeasHistoryC)

    NewCommentsC <- mapply(function(x, m, t) {
      invisible(sapply(m, function(so) if(substring(x, so, so) %in% c("N", "0")) substring(t, so, so) <<- "A"))
      t
    }, StatusHistoryC, measured, NewCommentsC, USE.NAMES = F)

  StatusHistoryC <- mapply(function(x, m) {
      invisible(sapply(m, function(so) if(so > 0) substring(x, so, so) <<- "1"))
      x
    }, StatusHistoryC, measured, USE.NAMES = F)

  }

  if(ThisIsShinyApp) incProgress(1/15)

  # scenario B - Alive > *Alive* > Alive ####

  resucitate <- gregexpr("(?<=1)[^1]*(?=1)", StatusHistoryC, perl = T)

  NewCommentsC <- mapply(function(x, m, t) {
    invisible(mapply(function(so, ml) if(grepl("N|0", substring(x, so, so + ml - 1L))) substring(t, so, so + ml - 1L) <<- strrep("B", ml), m, attr(m, "match.length")))
    t
  }, StatusHistoryC, resucitate, NewCommentsC, USE.NAMES = F)

StatusHistoryC <- mapply(function(x, m) {
    invisible(mapply(function(so, ml) if(grepl("N|0", substring(x, so, so + ml - 1L))) substring(x, so, so + ml - 1L) <<- strrep("1", ml), m, attr(m, "match.length")))
    x
  }, StatusHistoryC, resucitate, USE.NAMES = F)

if(ThisIsShinyApp) incProgress(1/15)

  # scenario C - Enough/not enough occurrences of death to validate it ####

  unseen <- gregexpr(paste0("(?<=1)N{", DeathConfirmation, ",}$|(?<=1)N{", DeathConfirmation, ",}(?=0)"), StatusHistoryC, perl = T)

  NewCommentsC <- mapply(function(x, m, t) {
    invisible(mapply(function(so, ml) if(ml>0) substring(t, so, so + ml - 1L) <<- strrep("C", ml), m, attr(m, "match.length")))
    t
  }, StatusHistoryC, unseen, NewCommentsC, USE.NAMES = F)

StatusHistoryC <- mapply(function(x, m) {
    invisible(mapply(function(so, ml) if(ml>0) substring(x, so, so + ml - 1L) <<- strrep("0", ml), m, attr(m, "match.length")))
    x
  }, StatusHistoryC, unseen, USE.NAMES = F)

if(ThisIsShinyApp) incProgress(1/15)

  # scenario D - Dead before alive ####

  deadfirst <- gregexpr("(?<!1)0*(?=1)", StatusHistoryC, perl = T)

  NewCommentsC <- mapply(function(x, m, t) {
    invisible(mapply(function(so, ml) if(ml>0) substring(t, so, so + ml - 1L) <<- strrep("D", ml), m, attr(m, "match.length")))
    t
  }, StatusHistoryC, deadfirst, NewCommentsC, USE.NAMES = F)

StatusHistoryC <- mapply(function(x, m) {
    invisible(mapply(function(so, ml) if(ml>0) substring(x, so, so + ml - 1L) <<- strrep("N", ml), m, attr(m, "match.length")))
    x
  }, StatusHistoryC, deadfirst, USE.NAMES = F)


if(ThisIsShinyApp) incProgress(1/15)

  # scenario E - remains dead ####

  deadNAend <- gregexpr("(?<=1)0+(N+)*$", StatusHistoryC, perl = T)

  NewCommentsC <- mapply(function(x, m, t) {
    invisible(mapply(function(so, ml) if(ml>0) substring(t, so, so + ml - 1L) <<- strrep("E", ml),  attr(m, "capture.start"), attr(m, "capture.length")))
    t
  }, StatusHistoryC, deadNAend, NewCommentsC, USE.NAMES = F)

StatusHistoryC <- mapply(function(x, m) {
    invisible(mapply(function(so, ml) if(ml>0) substring(x, so, so + ml - 1L) <<- strrep("0", ml), attr(m, "capture.start"), attr(m, "capture.length")))
    x
  }, StatusHistoryC, deadNAend, USE.NAMES = F)


if(ThisIsShinyApp) incProgress(1/15)

  # convert the Status and Comments history tables back into the data ###

  NewComments <- do.call(rbind, strsplit(NewCommentsC, ""))
  NewComments[] <- scenarios[ NewComments[] ]

  if(!all.equal(dim(NewComments), dim(CommentHistory))) stop("NewComments and CommentHistory are not of same dimensions")

  rownames(NewComments) <- rownames(CommentHistory)
  colnames(NewComments) <- colnames(CommentHistory)

  suppressWarnings(NewComments <- melt(setDT(as.data.frame(NewComments), keep.rownames=TRUE), measure.vars = colnames(NewComments) , variable.name = "IdCensus"))


  Data$StatusCorrectionMeth_DataHarmonization <- GenerateComment(Data$StatusCorrectionMeth_DataHarmonization, NewComments$value[match(paste(Data[,get(ID)], Data[,IdCensus]), paste(NewComments$rn, NewComments$IdCensus))])




    NewStatusHistory <- do.call(rbind, strsplit(StatusHistoryC, ""))
    NewStatusHistory[] <- suppressWarnings(as.logical(as.numeric(NewStatusHistory)))
    class(NewStatusHistory) <- "logical"

    if(!all.equal(dim(StatusHistory), dim(NewStatusHistory))) stop("StatusHistory and NewStatusHistory are not of same dimensions")

    rownames(NewStatusHistory) <- rownames(StatusHistory)
    colnames(NewStatusHistory) <- colnames(StatusHistory)

    suppressWarnings(NewStatusHistory <- melt(setDT(as.data.frame(NewStatusHistory), keep.rownames=TRUE), measure.vars = colnames(StatusHistory) , variable.name = "IdCensus"))

    Data <- cbind(Data, LifeStatus_DataHarmonizationCor = NewStatusHistory$value[match(paste(Data[,get(ID)], Data[,IdCensus]), paste(NewStatusHistory$rn, NewStatusHistory$IdCensus))])


    if(ThisIsShinyApp) incProgress(1/15)

  # Creating rows for absents ####
  if(AddRowsForForgottenCensuses) {

    missingNewStatusHistory <-  NewStatusHistory[!NewComments$value%in% "", ]
    missingNewComment <- NewComments[!value%in% "", ]

    # identify cases we need to add
    idx_new_rows <- which(match(paste(missingNewStatusHistory$rn, missingNewStatusHistory$IdCensus), paste(Data[,get(ID)], Data[,IdCensus])) %in% NA)

    if(length(idx_new_rows) > 0) {

    # get what we know about those trees
    NewRows <- Data[get(ID) %in% missingNewStatusHistory$rn[idx_new_rows], ]
    NewRows <- NewRows[!duplicated(get(ID)), ]

    # remove the data that changes at each census
    NewRows[,setdiff(names(NewRows), InvariantColumns) := NA]

    # repeat each row the number of times necessary
    nreps <- table(missingNewStatusHistory[idx_new_rows, rn])

    NewRows <- NewRows[rep(1:nrow(NewRows), times = nreps[match(NewRows[,get(ID)], names(nreps))]), ]



    # fill in info we know about the census and based on the Status we corrected
    m <- order(missingNewStatusHistory[idx_new_rows, rn])
    # m <- match(NewRows[,get(ID)], NewStatusHistory[idx_new_rows, rn])

    NewRows$IdCensus <- missingNewStatusHistory[idx_new_rows, IdCensus][m]

    NewRows$LifeStatus_DataHarmonizationCor <- missingNewStatusHistory[idx_new_rows, value][m]

    NewRows$StatusCorrectionMeth_DataHarmonization <- missingNewComment[idx_new_rows, value][m]

    NewRows$Comment_DataHarmonization <- "Missed stem"

    # make best guess at other things
    AllWarnings <- c(AllWarnings, "We added rows for missing trees and imputed average census Date")

    NewRows$Date <- as.Date(tapply(Data$Date, Data$IdCensus, mean, na.rm = T), origin = "1970-01-01")[NewRows$IdCensus]

    NewRows$Year <- format(NewRows$Date, "%Y")
    NewRows$Month <- format(NewRows$Date, "%m")
    NewRows$Day <- format(NewRows$Date, "%d")

    NewRows$MissedStem_DataHarmonizationCor <- TRUE
    Data$MissedStem_DataHarmonizationCor <- FALSE

    # Add these rows in the dataset
    Data <- rbindlist(list(Data, NewRows), use.names=TRUE, fill=TRUE)
  }
}


    if(ThisIsShinyApp) incProgress(1/15)

  # Re-put the rows duplicated, or without ID or IdCensus -----------------------------------------------------------------
  DuplicatedRows[, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "Duplicated measurement")]
  DuplicatedRows[, StatusCorrectionMeth_DataHarmonization := GenerateComment(StatusCorrectionMeth_DataHarmonization, "Not processed")]
  DuplicatedRows[, LifeStatus_DataHarmonizationCor := LifeStatus]

  DataIDNa[, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "Missing ID")]
  DataIDNa[, StatusCorrectionMeth_DataHarmonization := GenerateComment(StatusCorrectionMeth_DataHarmonization, "Not processed")]
  DataIDNa [, LifeStatus_DataHarmonizationCor := LifeStatus]


  Data <- rbindlist(list(Data, DuplicatedRows, DataIDNa), use.names = TRUE, fill = TRUE)


  # order by time
  Data <- Data[order(get(ID), IdCensus )]

  if(ThisIsShinyApp) incProgress(1/15)

  if(!is.null(AllWarnings)) warning(paste(AllWarnings, collapse = "\n"))

  # return Data
  return(Data)

}

