#'ReversedRequiredFormat
#'
#'@param Data Standardized data, returned from [RequiredFormat()]
#'
#'@param input A named list, typically the output of function
#'  RequiredFormat_interactive, also called site profile with information on
#'  column names correspondence, size units etc... Chosen to be the output
#'  profile the user wants their data turned into
#'
#'@param x For internal use when function used by Shiny app
#'
#'@param Untidy (logical). If TRUE and input$tidy exists, the data will be
#'  untidy (changed from long to wide format, according to input information)
#'
#'
#'@details This function takes the standardized forest inventory data.table
#'  (returned by [RequiredFormat()]) and converts the column names to the names
#'  of the profile given as input.
#'
#'@return (data.frame) in the format given the profile selected in input.
#'
#'@export
#'
#'@importFrom data.table copy setDT setDF dcast setnames melt tstrsplit :=
#'@importFrom utils read.csv
#'@importFrom stats formula
#'
#' @examples
#'\dontrun{
#'
#' data(ParacouSubsetFormated)
#' data("ForestGeoProfile")
#' ReversedRequiredFormat(ParacouSubsetFormated, ForestGeoProfile)
#'                }
#'

ReversedRequiredFormat <- function(
  Data,
  input,
  x = NULL,
  Untidy = FALSE
){
  # data(ParacouSubsetFormated)
  # data("ForestGeoProfile")
  # Data <- ParacouSubsetFormated
  # input <- ForestGeoProfile


  ThisIsShinyApp =  shiny::isRunning() # this is for internal use when function used by Shiny app


  # Arguments check
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")


  if(!ThisIsShinyApp & !inherits(input, "list")) {
    stop("input must be a list (typically, the output of funcion RequireFormat_interactive.R,
         or a profile saved viw the Shiny App")
  }

  # Load interactive items to see what we are missing ####

  if(!ThisIsShinyApp) {
    x <- try(expr = read.csv(system.file("/app/data/", "interactive_items.csv", package = "DataHarmonization", mustWork = TRUE)), silent = T)

    if (class(x) %in% "try-error"){
      warning("DataHarmonization package not loaded. Assuming you are in the root of the package instead.")
      x <- read.csv("inst/app/data/interactive_items.csv")
    }
    x <- x[x$Activate,]
  }


  ## set as data.table
  setDT(Data)
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environment

  ## format date of measurement like output profile ####
  if(!input$Date %in% "none"){

    DateFormat <- trimws(input$DateFormatMan)

    Data[, Date := as.Date(Date)]


    if(grepl("num|dec", DateFormat, ignore.case = T)) {

      if(grepl("num", DateFormat, ignore.case = T)) Data[, Date := as.numeric(Date)]

      if(grepl("dec", DateFormat, ignore.case = T)) Data[, Date := lubridate::decimal_date(Date)]

    } else {

      DateFormat <- gsub("(?<=^)\\w|(?<=[[:punct:]])\\w", "%", DateFormat, perl = T, ignore.case = T) # replace first letter of each word by '%'
      DateFormat <- gsub("yyy", "Y",DateFormat, ignore.case = T)# if remains 3 `y`, change to upper case Y

      Data[, Date := format(Date, format = DateFormat)]

    }

  }

  if(!input$Month %in% "none" & !input$Day %in% "none") {

    Data[, Date := as.Date(Date)]
    Data[, Month := format(Date, format = "%m")]
    Data[, Day := format(Date, format = "%d")]

  }



  # Units reverting from standard one ####

  units::remove_unit(c("ha", "ind", "gC"), c("hectare", "individual", "carbon"))

  units::install_unit("ha", "10000 m2", "hectare")
  units::install_unit("ind", name =  "individual")
  units::install_unit("gC", "0.47 g", "carbon")


  StandardUnitTable <- do.call(rbind, lapply(grep("UnitMan", x$ItemID, value = T), function(i) {

    ItemID <-  sub("UnitMan", "", i)

    if(is.na(x$Unit[match(ItemID, x$ItemID)])) ItemID <- paste0(c("X", "Y"), ItemID)


    data.frame(ItemID = ItemID,
               UnitMan = i,
               StandardUnit = x$Unit[match(ItemID, x$ItemID)]
    )

  }))

  StandardUnitTable <- StandardUnitTable[!input[StandardUnitTable$UnitMan] %in% "" & !input[StandardUnitTable$UnitMan] %in% "none" & !is.na(names(input[StandardUnitTable$UnitMan] )), ] # keep only the ones we actually have in the output

  setDF(Data)

  # if(any(input[StandardUnitTable$UnitMan[idx]] %in% "none")) stop(paste0("You did not specify units for ", gsub("UnitMan", "", StandardUnitTable$UnitMan[idx][input[StandardUnitTable$UnitMan[idx]]%in%"none"]), "."))
  #
  # idx <- idx[!input[StandardUnitTable$UnitMan[idx]] %in% "none"]

  for(i in 1:nrow(StandardUnitTable)) {
    # setting units
    units(Data[, StandardUnitTable$ItemID[i]]) <- StandardUnitTable$StandardUnit[i]
    # converting units
    units(Data[, StandardUnitTable$ItemID[i]]) <- input[[StandardUnitTable$UnitMan[i]]]
    # remove units class
    units(Data[, StandardUnitTable$ItemID[i]]) <- NULL


  }

  setDT(Data)
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environment

  # recalculate Circ if needed
  if(!input$Circ %in% "none") Data[, Circ := round(Diameter*pi, 2)]
  if(!input$BCirc %in% "none") Data[, BCirc := round(BD*pi, 2)]


  # untidy if wanted ####
  if(Untidy && input$Tidy > 0) {

    VariableName <- names(input)[input == input$VariableName & names(input) %in% x$ItemID]

    idx = sort(grep("TickedMelt", names(input), value = T))
    idx = which(unlist(input[idx]))


    ValueNames <- sort(grep("^ValueName", names(input), value = T))[idx]
    ValueNames <- names(input)[input %in% input[ValueNames] & names(input) %in% x$ItemID]

    Variablecolumns <- sort(grep("^Variablecolumns", names(input), value = T))[idx]
    Variablecolumns <-  unlist(input[Variablecolumns])


    # remove Circ or Diameter if one or the other is involved in tidy (otherwise that duplicates rows)
    if("Diameter" %in% ValueNames & "Circ" %in% names(Data)) Data$Circ <- NULL
    if("Circ" %in% ValueNames & "Diameter" %in% names(Data)) Data$Diameter <- NULL

    Data <- dcast(Data, formula(bquote(...~.(str2lang(VariableName)))), value.var = ValueNames)

    if(!any(grepl("_", Variablecolumns))) {
      old = grep(paste0(paste0('(', ValueNames, '_)', collapse = "|"), "|", paste0('(_', ValueNames, ')', collapse = "|")) , names(Data), value = TRUE)

      setnames(Data, old, gsub("_", "", old), skip_absent=TRUE)

      # setnames(Data, old, Variablecolumns, skip_absent=TRUE)
    }

  }



  # destandardize column names ####

  setDF(Data) # just for this step then we can put back in data.table

  m <- match(gsub("[[:punct:]]| ", "",colnames(Data)), gsub("[[:punct:]]| ", "",names(input)))
  idx_complete <- which(!input[m] %in% "none") # keep standard name when is not asked in the output Profile

  NewNames <- sapply(input[m[idx_complete]], function(x) ifelse(is.null(x), NA, x))
  NewNames[is.na(NewNames)] <-  colnames(Data)[idx_complete][is.na(NewNames)]

  colnames(Data)[idx_complete] <- NewNames


  setDT(Data)
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environment


  # return output ####
  return(Data)

}


