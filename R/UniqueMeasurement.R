#' UniqueMeasurement
#'
#' @description Of the repeated measurements of an individual in the same census,
#'   keep only the measurements (rows)
#'   taken at the **highest POM** (*KeepMeas = "MaxHOM"*),
#'   and/or the **most recent measurement** (same census but more recent date)
#'   (*KeepMeas = "MaxDate"*)
#'
#' @param Data Dataset (data.table)
#'
#' @param KeepMeas In case of **multiple measurements** in the same census:
#' Possible values: "MaxHOM", "MaxDate" (character).
#'   - "MaxHOM": keep the measurement taken at the **highest HOM/POM**
#'   - "MaxDate": keep the **most recent measurement** (same
#'                census but more recent date)
#'
#' @param ID Column name indicating the identifier of the individual (character)
#'
#' @return Dataset (data.table) with 1 measurement (1 row) per IdCensus
#'
#' @export
#'
#' @examples
#'
#' library(data.table)
#'
#' Data <- data.table( # 7 rows
#' IdStem = c(rep("a", 4), rep("b", 2), "c"),
#' IdCensus = c(rep(2000, 3), 2001, rep(2000, 3)),
#' Date = as.Date(c("2000-01-10", "2000-01-20", "2000-01-30", "2001-01-10",
#'                  rep("2000-01-10", 3))),
#' HOM = c(rep(1, 3), 2, 1, 2, 1)
#' )
#' Data
#'
#' UniqueMeasurement(TestData)
#'
UniqueMeasurement <- function(
  Data,
  KeepMeas = c("MaxHOM", "MaxDate"), # 1st HOM, then Date, or only one
  ID = "IdStem"
){

  Data <- unique(Data)   # if there are duplicate rows, delete them

  ByCols <- c(ID, "IdCensus")

  KeepMeas <- match.arg(KeepMeas, several.ok = T)

  # Separate duplicated and non duplicated --------------------------------------------------------------------------------

  DuplicatedID <- Data[duplicated(Data[, ..ByCols]), ..ByCols] # duplicated ID-IdCensus

  if(nrow(DuplicatedID) > 0){

    DuplicatedRows <- Data[paste(get(ID), IdCensus, sep = "/") %in% DuplicatedID[, paste(get(ID), IdCensus, sep = "/")]] # rows with duplicated ID-IdCensuss

    UniqueData <- Data[!DuplicatedRows, on = .NATURAL] # rows that do not have duplicate measurements


    # Keep the row with the upper POM -------------------------------------------------------------------------------------

    if("MaxHOM" %in% KeepMeas){


      # POM or HOM? ---------------------------------------------------------------------------------------------------
      # If no HOM take POM
      if((!"HOM" %in% names(Data) | all(is.na(Data$HOM))) &
         ("POM" %in% names(Data) & any(!is.na(Data$POM))) ){ POMv <- "POM"

      }else{ POMv <- "HOM"}

      if(!any(c("POM", "HOM") %in% names(Data)) | (all(is.na(Data$POM)) &  all(is.na(Data$HOM))) )
        message("You have chosen to make corrections to the measurements taken at the highest POM,
        but 'POM' and HOM' columns are empty for ", IdStem,".
             The 'MaxHOM' criterion can therefore not be taken into account.")

      DuplicatedRows <- DuplicatedRows[, .SD[get(POMv) == max(get(POMv))], by = ByCols] # keep only the row with max HOM


    } # end "MaxHOM"


    # Keep the row with the upper date ------------------------------------------------------------------------------------

    if("MaxDate" %in% KeepMeas & nrow(DuplicatedRows) > 1){


      DuplicatedRows <- DuplicatedRows[, .SD[ Date == max(Date)],  by = ByCols] # keep only the row with max HOM
    } # end "MaxDate"

    if(any(duplicated(DuplicatedRows[, ..ByCols]))) stop(paste0("There are still several measurements per census despite the choice of ", paste(KeepMeas, collapse = " and "), ".", ifelse(length(KeepMeas) <2, paste("you may want to try to add 'KeepMeas=", setdiff(eval(formals(sys.function(sysP <- sys.parent()))$KeepMeas), KeepMeas), "'"), "")))


    Data <-  rbind(UniqueData, DuplicatedRows)

  } # end if duplicated

  return(Data[order(get(ID), IdCensus), ])

}
