#' Full error processing
#'
#' @inheritParams GeneralErrorsDetection
#' @inheritParams BotanicalCorrection
#' @inheritParams StatusCorrection
#' @inheritParams DiameterCorrection
#' @inheritParams RecruitmentCorrection
#'
#' @details Detect errors or detect and correct errors:
#' - Check general errors (*GeneralErrorsDetection*)
#' - Check botanical identification (*BotanicalCorrection*)
#' - Check the life status evolution of the trees/stems (*StatusCorrection*)
#' - Apply a taper allometry on diameters measured at heights different from the
#'    default HOM (*TaperCorrection*)
#' - Check diameter evolution of the trees (*DiameterCorrection*)
#' - Check tree/stem recruitment (*RecruitmentCorrection*)
#'
#' @seealso \link{GeneralErrorsDetection}, \link{BotanicalCorrection},
#'   \link{StatusCorrection}, \link{TaperCorrection}, \link{DiameterCorrection},
#'   \link{RecruitmentCorrection}
#'
#' @return The original dataset (data.table) with a *Comment* column containing
#'   information on the errors detected per row, the correction columns, and
#'   columns containing correction methods.
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' data(TestData)
#' Rslt <- FullErrorProcessing(TestData,
#'  OnlyDetectMissedRecruits = TRUE,
#'   AddRowsForForgottenCensuses = FALSE)
#'
#' Rslt_Test <- FullErrorProcessing(TestData)
#' Rslt_Panama <- FullErrorProcessing(PanamaFormated)
#' }
#'
FullErrorProcessing <- function(

  Data,
  # Life status

  DeathConfirmation = 2,
  UseSize = FALSE,
  AddRowsForForgottenCensuses = TRUE,


  # Diameter

  ## Taper

  UseTaperCorrection = TRUE,
  DefaultHOM = 1.3,
  TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  TaperFormula = function(DAB, HOM, TaperParameter, DefaultHOM) DAB / (exp(- TaperParameter*(HOM - DefaultHOM))),

  ## other diameter corrections
  KeepMeas = c("MaxHOM", "MaxDate"),
  PositiveGrowthThreshold = 5,
  NegativeGrowthThreshold = -2,

  Pioneers = NULL,
  PioneersGrowthThreshold = 7.5,

  WhatToCorrect = c("POM change", "Abnormal growth"),
  CorrectionType = c("individual", "phylogenetic hierarchical"),

  DBHRange = 10,
  MinIndividualNbr = 5,

  MinDBH = NULL,
  AddMissedRecruits = T,
  AddMissedStems = T,

  DBHCorForDeadTrees = TRUE,

  coef = 0.9,

  # Recruitment
  OnlyDetectMissedRecruits = FALSE
){

  #### Arguments check ####

  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # Plot column exists
  if (!"Plot" %in% names(Data)){
    stop("The column 'Plot' must be present in the dataset
    to add rows to the census where the plot was inventoried, where the tree was alive, but not recorded")
  }

  # InvariantColumns
  # if (!inherits(InvariantColumns, "character"))
  #   stop("'InvariantColumns' argument must be of character class")

  # DeathConfirmation
  if (!inherits(DeathConfirmation, "numeric"))
    stop("'DeathConfirmation' argument must be numeric")

  # UseSize/OnlyDetectMissedRecruits
  if (!all(unlist(lapply(list(UseSize, OnlyDetectMissedRecruits),
                         inherits, "logical"))))
    stop("The 'UseSize' and 'OnlyDetectMissedRecruits' arguments
         of the 'SatusCorrection' function must be logicals")

  # UseSize-Diameter
  if(UseSize %in% TRUE){ # if it is desired (TRUE) to use the presence of measurement to consider the tree alive
    if (!"Diameter" %in% names(Data)){
      stop("If you wish to use the size presence (UseSize=TRUE) as a witness of the living status of the tree,
           the 'Diameter' column must be present in the dataset")
    }
  }

  if(UseTaperCorrection){
    # Check if the HOM column exists
    if(!"HOM" %in% names(Data)){
      stop("You have chosen to make a taper correction,
       but you do not have the necessary 'HOM' column in your dataset")
    }

    # TaperParameter/TaperFormula (function)
    if(!all(unlist(lapply(list(TaperParameter, TaperFormula),
                          inherits, "function"))))
      stop("The 'TaperParameter' and 'TaperFormula' arguments must be functions")

  } # end: if taper

  # Diameter column exists
  if(!"Diameter" %in% names(Data))
    stop("The 'Diameter' column does't exist in the dataset")

  # DefaultHOM/Min-MaxDBH/Positive-Negative-PioneersGrowthThreshold/DBHRange/MinIndividualNbr (numeric, 1 value)
  if(!all(unlist(lapply(list(DefaultHOM, # MaxDBH,
                             PositiveGrowthThreshold, NegativeGrowthThreshold, PioneersGrowthThreshold,
                             DBHRange, MinIndividualNbr),
                        length)) %in% 1) |
     !all(unlist(lapply(list(PositiveGrowthThreshold, NegativeGrowthThreshold, DefaultHOM, PioneersGrowthThreshold),
                        inherits, c("numeric", "integer")))))
    stop("The 'PositiveGrowthThreshold', 'NegativeGrowthThreshold', 'PioneersGrowthThreshold' and 'DefaultHOM' arguments
         must be 1 numeric value each")

  # Pioneers (characters vector)
  if(!inherits(Pioneers, "character") & !is.null(Pioneers))
    stop("'Pioneers' argument must be a characters vector, or NULL")

  # WhatToCorrect
  WhatToCorrect <- match.arg(WhatToCorrect, several.ok = TRUE)

  # CorrectionType
  CorrectionType <- match.arg(CorrectionType, several.ok = TRUE)

  # Taper before if 'HOM' and no taper correction asked
  if(any(!is.na(Data$HOM)) & !UseTaperCorrection) # HOM exists?
    message("You have the 'HOM' information in your dataset.
            We advise you to correct your diameters also with the taper correction (UseTaperCorrection = TRUE)")

  # If 'POM' 'POM change' correction is advised
  if((all(is.na(Data$HOM)) | !"HOM" %in% names(Data)) &
     any(!is.na(Data$POM)) & !any(WhatToCorrect %in% "POM change")) # POM exists?
    message("You have the 'POM' information in your dataset.
            We advise you to correct your diameters also from the 'POM change' ('WhatToCorrect' argument)")

  # 'POM change' correction needs 'POM' or 'HOM' values
  if(!any(c("POM", "HOM") %in% names(Data)) | (all(is.na(Data$POM)) &  all(is.na(Data$HOM))) )
    stop("You have chosen to make a 'POM change' correction,
       but you do not have the necessary 'POM' or HOM' column in your dataset or they are empty")

  #### General errors detection ####
  Data <- GeneralErrorsDetection(Data)

  #### Botanical informations ####

  Data <- BotanicalCorrection(Data = Data)$Data

  #### Life status ####

  Data <- StatusCorrection(Data,
                           DeathConfirmation = DeathConfirmation,
                           UseSize = UseSize,
                           AddRowsForForgottenCensuses = AddRowsForForgottenCensuses)


  #### Diameter ####

  if(any(c("individual", 'phylogenetic hierarchical') %in% CorrectionType) |
     any(c("POM change", "Abnormal growth") %in% WhatToCorrect)){

    Data <- DiameterCorrection(Data,
                               UseTaperCorrection = UseTaperCorrection,
                               DefaultHOM = DefaultHOM,

                               TaperParameter = TaperParameter,
                               TaperFormula = TaperFormula,

                               KeepMeas = KeepMeas,

                               PositiveGrowthThreshold = PositiveGrowthThreshold,
                               NegativeGrowthThreshold = NegativeGrowthThreshold,

                               Pioneers = Pioneers,
                               PioneersGrowthThreshold = PioneersGrowthThreshold,

                               WhatToCorrect = WhatToCorrect,
                               CorrectionType = CorrectionType,

                               DBHRange = DBHRange,
                               MinIndividualNbr = MinIndividualNbr,
                               DBHCorForDeadTrees = DBHCorForDeadTrees,
                               MinDBH = MinDBH,
                               AddMissedRecruits = AddMissedRecruits,
                               AddMissedStems = AddMissedStems,

                               coef = coef)
  }

  #### Recruitment ####

  Data <- RecruitmentCorrection(Data,
                                KeepMeas = KeepMeas,
                                MinDBH = MinDBH,
                                OnlyDetectMissedRecruits = OnlyDetectMissedRecruits
  )

  return(Data)

}
