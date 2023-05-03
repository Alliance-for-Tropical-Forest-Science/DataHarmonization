#'RequiredFormat
#'
#'@param Data Forest inventory data set (data.frame or data.table) - already stacked, merged and tidyed
#'
#'@param input A named list, typically the output of function
#'  RequiredFormat_interactive, also called site profile. It has information on
#'  column names correspondence, size units etc...
#'
#'@param x For internal use when function used by Shiny app
#'
#'@param MeasLevel your deepest level of measurements(When function is run outside of Shiny app). Options are one of c("Plot", "Species", "Tree", "Stem")
#'
#'
#'@details This function takes the forest inventory data.frame or data.table as
#'  it is, and converts the column names to the standardized names used in this
#'  package. It also generates missing information, when possible (e.g. Diameter when
#'  only circumference is givent, Genus and Species when only scientifique name
#'  is given etc...). All the decisions are made based on what is provided in
#'  the input argument, which is a named list, as returned by function
#'  RequiredFormat_interactive or Profile.rds file downloaded from shiny app
#'
#'@return Input inventory (data.frame) in the required package format.
#'
#'@export
#'
#'@importFrom data.table copy setDT setDF melt tstrsplit :=
#'@importFrom utils read.csv
#'@importFrom units install_unit remove_unit

#'
#' @examples
#'\dontrun{
#' data(ParacouSubset)
#' data(ParacouProfile)
#' ParacouSubsetFormated <- RequiredFormat(
#'   ParacouSubset,
#'   input = ParacouProfile,
#'   MeasLevel = "Tree")
#'                }
#'

RequiredFormat <- function(
    Data,
    input,
    x = NULL,
    MeasLevel = NULL
){
  # data(ParacouSubset)
  # data(ParacouProfile)
  # Data <- ParacouSubset
  # input <- ParacouProfile

  ThisIsShinyApp =  shiny::isRunning() # this is for internal use when function used by Shiny app

  # prepare a place to hold all warnings so we get only one pop up window
  AllWarnings <- NULL

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
      AllWarnings <- c(AllWarnings, "DataHarmonization package not loaded. Assuming you are in the root of the package instead.")
      x <- read.csv("inst/app/data/interactive_items.csv")
    }

    # keep only what is "active" (the rest is not in used)
    x <- x[x$Activate,]


    # add MeasLevel
    if(!MeasLevel %in% c("Plot", "Species", "Tree", "Stem")) stop("MeasLevel needs to be one of 'Plot, 'Species', 'Tree' or 'Stem'")
    input$MeasLevel <- MeasLevel

  }

  CharacVar <- x$ItemID[x$DataType %in% "character"]
  NumVar <- x$ItemID[x$DataType %in% "numeric"]
  LogicVar <- x$ItemID[x$DataType %in% "logical"]
  FactorVar <- x$ItemID[x$DataType %in% "factor"]

  # standardize column names ####

  setDF(Data) # just for this step then we can put back in data.table

  idx <- match(gsub("[[:punct:]]| ", "", colnames(Data)), gsub("[[:punct:]]| ", "", input[x$ItemID]))
  NewColNames <- names(input[x$ItemID])[idx]

  ## deal with TreeCodes separately
  ## repeat cases where multiple columns match one item (only checked for TreeCodes, need to check what happens for other columns)
  multiplecolumns <- names(which(sapply(input[x$ItemID[!x$Group %in% "second column"]], length)>1))
  if(any(!multiplecolumns %in% "TreeCodes")) stop ("You've selected multiple columns for something other than 'TreeCodes', please contact us at herrmannv@si.edu")

  if(length(multiplecolumns) > 0 & all(multiplecolumns %in%  "TreeCodes")) {
    TreeCodes <- input[multiplecolumns][[1]]
    names(TreeCodes) <- rep(multiplecolumns, length(TreeCodes))
    input[multiplecolumns] <- NULL

    Data[, paste0("Original_", colnames(Data)[colnames(Data) %in% TreeCodes])] <-  Data[, colnames(Data) %in% TreeCodes]

    NewColNames <- c(NewColNames, paste0("Original_", colnames(Data)[colnames(Data) %in%  TreeCodes]))


  } else {
    if(!is.null(input$TreeCodes) && !input$TreeCodes %in% "none" & all(input$TreeCodes %in% names(Data)) ) {
      Data[, paste0("Original_", colnames(Data)[colnames(Data) %in% input$TreeCodes])] <-  Data[, colnames(Data) %in% input$TreeCodes]

      NewColNames <- c(NewColNames, paste0("Original_", colnames(Data)[colnames(Data) %in%  input$TreeCodes]))

    }
  }


  # change columns

  colnames(Data) <- NewColNames

  ## delete columns we don't want (except the ones related to TreeCodes)
  Data[which(is.na(colnames(Data)))] <- NULL

  # save some Original columns
  Data[,  paste0(intersect(names(Data),x$ItemID[x$SaveCopy]), "Original")] <- Data[, intersect(names(Data),x$ItemID[x$SaveCopy])]

  ## add columns missing
  Data[, setdiff(gsub("[[:punct:]]| ", "", x$ItemID[x$RequiredColumn]), gsub("[[:punct:]]| ", "", colnames(Data)))] <- NA

  ## deal with case where one column represents more than one thing
  DoubleFctColumn <- input[names(input) %in% x$ItemID[x$RequiredColumn] & !input %in% "none" & input %in% input[duplicated(input) & names(input) %in% x$ItemID[x$RequiredColumn]]]

  for(j in names(DoubleFctColumn)) {
    if(all(is.na(Data[, j]))) Data[, j] <- Data[, names(DoubleFctColumn[DoubleFctColumn %in% DoubleFctColumn[[j]] & !names(DoubleFctColumn) %in% j])]
    if( !paste0(j, "Original") %in% names(Data)) Data[, paste0(j, "Original")] <- Data[, j]
  }


  setDT(Data)
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environment

  # coerce to data types ####
  ### as.character

  CharacVar <- CharacVar[CharacVar %in% colnames(Data)]

  Data[, (CharacVar) := lapply(.SD, as.character), .SDcols = CharacVar] # (CharacVar) to say that these are existing columns and not new ones to create

  ### as.numeric

  NumVar <- NumVar[NumVar  %in% colnames(Data)]

  Data[, (NumVar) := lapply(.SD, as.character), .SDcols = NumVar] # first as character when the variable is in factor, to preserve information
  suppressWarnings(Data[, (NumVar) := lapply(.SD, as.numeric), .SDcols = NumVar]) # () to say that these are existing columns and not new ones to create

  #### as.factor

  FactorVar <- FactorVar[FactorVar %in% colnames(Data)]

  Data[, (FactorVar) := lapply(.SD, as.factor), .SDcols = FactorVar] # (FactorVar) to say that these are existing columns and not new ones to create

  ### as.logical
  ## Here we have to use user input to know what is TRUE and what is not

  ### Life/Dead status
  if( !is.null(input$LifeStatus)) {
    if(is.null(input$DeadStatus)) {
      stop("Your profile is missing 'DeadStatus'")
    } else {
      if(!input$LifeStatus %in% "none" & !input$DeadStatus %in% "none") {

        # Data[, LifeStatusOriginal := LifeStatus]
        Data[LifeStatusOriginal %in% input$IsLiveMan, LifeStatus := TRUE]
        Data[DeadStatusOriginal %in% input$IsDeadMan, LifeStatus := FALSE]

        Data[, LifeStatus := as.logical(LifeStatus)] # any other thing that "TRUE" and "FALSE" will be converted to NA.
        Data[, DeadStatus := NULL] # delete that as we don't need it.
      }

    }
  }

  ### commercial species
  if( !is.null(input$CommercialSp)) {
    if( !input$CommercialSp %in% "none") {
      # Data[, CommercialSpOriginal := CommercialSp]
      Data[, CommercialSp := ifelse(CommercialSp %in% input$IsCommercial, TRUE, FALSE)]
    }
  }

  # LogicVar <- LogicVar[LogicVar %in% colnames(Data)]
  # Data[, (LogicVar) := lapply(.SD, as.logical), .SDcols = LogicVar] # () to say that these are existing columns and not new ones to create


  # Deal with Date of measurement before anything else ####

  # add Year if given manually
  if(input$Year %in% "none" & !input$YearMan %in% -999) {
    Data[, Year := as.numeric(as.character(input$YearMan))]

    # overwrite input
    input$Year = "Year"
  }

  # concatenate if in 3 different columns
  if(!input$Month %in% "none" & !input$Day %in% "none" & input$Date %in% "none") {
    if(!input$Year %in% "none") {
      Data[, Date := paste(trimws(Year), trimws(Month), trimws(Day), sep = "-")]

      # overwrite input
      input$Date = "Date"
      input$DateFormatMan = "yyyy-mm-dd"
    } else {
      AllWarnings <- c(AllWarnings, "You did not provide a Year so we can't recreate a date using your Month and Day columns.")
    }


  }

  # consider date as June 15th if not Date is given

  if(input$Date %in% "none") {
    if(!input$Year %in% "none") {
      Data[, Date := paste0(Year, "-06-15")]
      AllWarnings <- c(AllWarnings, "You did not provided a Date of measurement but provided a Year. We consider the date as 15th June of the year so as to prevent NA.")
      # overwrite input
      input$Date = "Date"
      input$DateFormatMan = "yyyy-mm-dd"

    } else {
      AllWarnings <- c(AllWarnings, "You did not provide a Year so we can't recreate a date.")
    }

  }


  # put in date format

  if(!input$Date %in% "none"){

    # save the orginal dates
    # Data[, DateOriginal := Date]

    # transform to standard format
    DateFormat <- trimws(input$DateFormatMan)


    if(grepl("num|dec", DateFormat, ignore.case = T)) {

      if(grepl("num", DateFormat, ignore.case = T)) suppressWarnings(Data[, Date := as.Date(as.numeric(trimws(Date)), origin = "1970-01-01")])

      if(grepl("dec", DateFormat, ignore.case = T)) suppressWarnings(Data[, Date := as.Date(lubridate::date_decimal(as.numeric(trimws(Date))))])

    } else {

      DateFormat <- gsub("(?<=^)\\w|(?<=[[:punct:]])\\w", "%", DateFormat, perl = T, ignore.case = T) # replace first letter of each word by '%'
      DateFormat <- gsub("yyy", "Y", DateFormat, ignore.case = T)# if remains 3 `y`, change to upper case Y

      Data[, Date := as.Date(trimws(as.character(Date)), format = DateFormat)]

    }

    # send warning if some dates translated as NA
    if(any(!is.na(Data$DateOriginal) & is.na(Data$Date))) AllWarnings <- c(AllWarnings, "Some dates were translated as NA... Either your data format does not corresponf to the format of your date column, or you do not have a consistent format across all your dates.")

  }





  # make input complete ####

  ## enter all itemID in input as "none" so we can refer to them - make sure this happens after standardizing column names otherwise that won't work...
  input[setdiff(x$ItemID, names(input))] <- x$Default[match(setdiff(x$ItemID, names(input)), x$ItemID)]



  # Fill in info in column missing ####

  ## Year
  if(input$Year %in% "none") {
    if(!input$Date %in% "none") Data[, Year := format(Date, "%Y")] else AllWarnings <- c(AllWarnings, "You did not provide Date nor Year.")

    Data$Year <- as.numeric(as.character(Data$Year))

  }

  ## Month
  if(input$Month %in% "none") {
    if(!input$Date %in% "none") Data[, Month := format(Date, "%m")]
    Data$Month <- as.numeric(as.character(Data$Month))

  }

  ## Day
  if(input$Day %in% "none") {
    if(!input$Date %in% "none") Data[, Day := format(Date, "%d")]
    Data$Day <- as.numeric(as.character(Data$Day))

  }

  ## IdCensus

  ### if Date, use that to order the IdCensus
  if(!input$IdCensus %in% "none") {
    if(!input$Date %in% "none") Data[, IdCensus := factor(IdCensus, levels = unique(IdCensus[order(Date)]), ordered = T)]
  }

  ### if not IdCensus, use Year instead
  if(input$IdCensus %in% "none") {

    AllWarnings <- c(AllWarnings, "You did not provide a Census ID column. We will use year as census ID.")

    Data$IdCensus <- factor(Data$Year, ordered = TRUE)
  }

  ## Site, Plot, subplot
  if (input$Site %in% "none") {
    if(input$SiteMan %in% "")  AllWarnings <- c(AllWarnings, "You did not specify a Site column or name, we will consider you have only one site called 'SiteA'.")

    SiteMan <- ifelse(input$SiteMan %in% "", "SiteA", input$SiteMan)
    Data[, Site :=  SiteMan]

  }
  if (input$Plot %in% "none") {
    if(input$PlotMan %in% "")  AllWarnings <- c(AllWarnings, "You did not specify a Plot column or name, we will consider you have only one plot called 'PlotA'.")

    PlotMan <- ifelse(input$PlotMan %in% "", "PlotA", input$PlotMan)
    Data[, Plot :=  PlotMan]
  }

  if (input$Subplot %in% "none"){
    if(input$SubplotMan %in% "")  AllWarnings <- c(AllWarnings, "You did not specify a subplot column or name, we will consider you have only one subplot called 'SubplotA'.")

    SubplotMan <- ifelse(input$SubplotMan %in% "", "SubplotA", input$SubplotMan)
    Data[, Subplot := SubplotMan]
  }

  ## IdTree (unique along IdCensus) ####

  if ((input$IdTree %in% "none" | any(is.na(Data$IdTree))) & input$MeasLevel %in% c("Tree", "Stem")) {

    # if we also don't have TreeFieldNum, we are just considering that each row within a plot and subplot is one tree
    if(input$IdTree %in% "none") Data$IdTree <- NA

    # if we have TreeFieldNum, we use it

    if (!input$TreeFieldNum %in% "none") {

      AllWarnings <- c(AllWarnings, paste("You are missing treeIDs (either you are missing some tree IDs or you  did not specify a column for tree IDs). But you did specified a column for tree tag, so we are considering that each tree tag within a Site, plot, subplot and census ID", ifelse(input$IdCensus %in% "none", "(taken as your Year, since you did not specify a census ID column)", ""), "refers to one tree, and we are using your tree field tag to construct the tree ID.", ifelse(any(is.na(Data$TreeFieldNum)), "And since some of your  tree field tag are NAs, we will automatically generating those assuming each NA represents one single-stem tree and that the order of those trees is consistent accross censuses.", "")))

      if(any(is.na(Data$TreeFieldNum))) {
        Data[is.na(TreeFieldNum), TreeFieldNum := paste0(seq(1, .N), "_auto") , by = .(Site, Plot, Subplot, IdCensus)]
      }

      Data[is.na(IdTree), IdTree := paste(Site, Plot, Subplot, TreeFieldNum, "auto", sep = "_") , by = .(IdCensus)]

      Data[is.na(IdTree), IdTree := paste(Site, Plot, Subplot, TreeFieldNum, "auto", sep = "_") , by = .(IdCensus)]

    }

    # if we also don't have TreeFieldNum, we are just considering that each row within a plot and subplot is one tree (or we use stemID, which will take care of ForestPlot data where theyonly have idTree for multistem tree)

    if (input$TreeFieldNum %in% "none") {

      if (input$IdStem %in% "none") {
        AllWarnings <- c(AllWarnings, paste("You are missing treeIDs (either you are missing some tree IDs or you did not specify a column for tree IDs). You also did not specify a column for Tree Tags, so we are considering that each row within a Site, plot, subplot and census ID", ifelse(input$IdCensus %in% "none", "(taken as your Year, since you did not specify a census ID column)", ""), "refers to one unique (single-stem) tree. This is assuming the order of your trees is consistent accross censuses."))


        Data[is.na(IdTree), IdTree := paste0(seq(1, .N), "_auto")  , by = .(IdCensus)]
      }

      if (!input$IdStem %in% "none") {
        AllWarnings <- c(AllWarnings, paste("You are missing treeIDs (either you are missing some tree IDs or you did not specify a column for tree IDs). You also did not specify a column for Tree Tags, BUT you did specify a column for Stem tags, so we are using IdStem to replace missing IdTree. WARNING: This was created to deal with ForestPlots data, where only  only multiple stems have an IdTree, so, in that particular case, it is safe to use IdStem as IdTree."))


        Data[is.na(IdTree), IdTree := paste0(IdStem, "_auto")]
      }
    }



  }

  ## IdStem (unique along IdCensus) ####

  if ((input$IdStem %in% "none" | any(is.na(Data$IdStem))) & input$MeasLevel %in% c("Tree", "Stem")) {

    # if we also don't have StemFieldNum, we are just considering that each row within a plot and subplot  and tree is one stem
    if(input$IdStem %in% "none") Data$IdStem <- NA

    if (input$StemFieldNum %in% "none") {

      if (input$MeasLevel %in% "Stem") AllWarnings <- c(AllWarnings, "You are missing stemIDs (either you are missing some stem IDs or you  did not specify a column for stem IDs). You also did not specify a column for stem Tags, so we are considering that each row without a stem ID refers to one unique stem within its tree ID. This is assuming that the order of each stem within a tree is consistent across censuses.")
      Data[is.na(IdStem), IdStem := paste0(.(IdTree), "_", seq(1, .N), "_auto"), by = .(IdCensus, IdTree)]

    }

    # if we have TreeFieldNum, we use it

    if (!input$StemFieldNum %in% "none") {

      if (input$MeasLevel %in% "Stem")  AllWarnings <- c(AllWarnings, "You are missing stemIDs (either you are missing some tree IDs or you  did not specify a column for stem IDs). But you did specify a column for stem tags, so we are considering that each stem field number within a tree refers to a unique stem and are using your stem field number to construct the stem ID.", ifelse(any(is.na(Data$StemFieldNum)), "And since some of your stem field tags are NAs, we will automatically generating those assuming assuming that the order of each stem within a tree is consistent across censuse.", ""))

      if(any(is.na(Data$StemFieldNum))) {
        Data[is.na(StemFieldNum), StemFieldNum := paste0(seq(1, .N), "_auto") , by = .(IdCensus, IdTree)]
      }

      Data[is.na(IdStem), IdStem := paste(IdTree, StemFieldNum, "auto", sep = "_"), by = .(IdCensus)]

    }


  }


  ## Genus, Species, ScientificNameSepMan ####

  if(!input$MeasLevel %in% c("Plot")) {
    ### Genus and species if we have ScientificName and ScientificNameSepMan
    if(!input$ScientificName %in% "none" & !input$ScientificNameSepMan %in% "none") {
      if(input$Genus %in% "none") Data[, Genus := tstrsplit(ScientificName, input$ScientificNameSepMan, fixed = TRUE, keep  = c(1))]
      if(input$Species %in% "none") Data[, Species := tstrsplit(ScientificName, input$ScientificNameSepMan, fixed = TRUE, keep  = c(2))]
      if(input$Subspecies %in% "none" & any(grepl(
        "(.* .*){2,}", Data$ScientificName))) Data[, Subspecies := tstrsplit(ScientificName, input$ScientificNameSepMan , fixed = TRUE, keep  = c(3))]
    }

    ### ScientificName if we have Genus and species

    if(!input$Genus %in% "none" & !input$Species %in% "none" & input$ScientificName %in% "none" ) Data[, ScientificName := paste(Genus, Species)]

  }


  ## Diameter if we have circumference ####
  if(input$MeasLevel %in% c("Tree", "Stem")) {
    if(input$Diameter %in% "none" & input$Circ %in% "none" & input$BD %in% "none" & input$BCirc %in% "none") AllWarnings <- c(AllWarnings, "You did not specify what column represents tree size (Diameter, Circonference, BD or basal circonference) in your data.")

    if(input$Diameter %in% "none" & !input$Circ %in% "none") {
      Data[, Diameter := round(Circ/pi, 2)]
      input$DiameterUnitMan <- input$CircUnitMan
    }
    if(input$BD %in% "none" & !input$BCirc %in% "none") {
      Data[, BD := round(BCirc/pi, 2)]
      input$BDUnitMan <- input$BCircUnitMan

    }
  }

  ## LifeForm if provided manuall
  if(input$LifeForm %in% "none" & length(input$LifeFormMan) > 0) {
    Data[, LifeForm := paste(input$LifeFormMan, collapse = ";")]
    input$LifeForm  = "LifeForm"
  }


  ## MinDBH if we don't have it
  if(input$MinDBH %in% "none") {

    if(!input$MinDBHMan %in% -999) {
      Data[, MinDBH := input$MinDBHMan]
      input$MinDBHUnitMan <- "cm" # if MinDBH given by hand, it should be in cm

    }
    if(input$MinDBHMan %in% -999) {

      if(input$MeasLevel %in% c("Tree", "Stem")) {
        Data[, MinDBH := min(Diameter, na.rm = T)]
        input$MinDBHUnitMan <- grep("[^none]", c(input$DiameterUnitMan, input$CircUnitMan), value = T)[1] # take Diameter in priority, otherwise CircUnit
        AllWarnings <- c(AllWarnings, "MinDBH was calculated.")
      } else {
        AllWarnings <- c(AllWarnings, "You did not specify a MinDBH.")

      }
    }
  }

  ## HOM if we don't have it
  if(input$HOM %in% "none") {

    if(!input$HOMMan %in% -999) {
      Data[, HOM := input$HOMMan]
      input$MinDBHUnitMan <- "m" # if HOM given by hand, it should be in m

    }
    if(input$HOMMan %in% -999) {

      if(input$MeasLevel %in% c("Tree", "Stem")) {
        AllWarnings <- c(AllWarnings, "You did not specify a height of measurement (HOM)")

      }
    }
  }



  # PlotArea (if area is entered manually, it is supposed to be in ha already)
  if(input$PlotArea %in% "none") {
    if(!input$PlotAreaMan %in% -999) {
      Data[, PlotArea := input$PlotAreaMan]
      input$PlotAreaUnitMan <- "ha"
    }

    if(input$PlotAreaMan %in% -999) AllWarnings <- c(AllWarnings, "You did not specify a plot area.")
  }

  # SubplotArea (if area is entered manually, it is supposed to be in ha already)
  if(input$SubplotArea %in% "none") {

    if(!input$SubplotAreaMan %in% -999) {
      Data[, SubplotArea := input$SubplotAreaMan]
      input$SubplotAreaUnitMan <- "ha"
    }

    if(input$SubplotAreaMan %in% -999) AllWarnings <- c(AllWarnings, "You did not specify a subplot area.")

  }

  # convert units to standards ####

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

  StandardUnitTable <- StandardUnitTable[!input[StandardUnitTable$ItemID] %in% "none", ] # keep only the ones we need

  if(any(is.na(StandardUnitTable$StandardUnit))) stop("Some Stanadrd unit have not been defined, contact HerrmannV@si.edu")

  setDF(Data)
  idx <- which(StandardUnitTable$ItemID %in% NewColNames)

  # if(any(input[StandardUnitTable$UnitMan[idx]] %in% "none")) stop(paste0("You did not specify units for ", gsub("UnitMan", "", StandardUnitTable$UnitMan[idx][input[StandardUnitTable$UnitMan[idx]]%in%"none"]), "."))
  #
  # idx <- idx[!input[StandardUnitTable$UnitMan[idx]] %in% "none"]

  for(i in idx) {
    # setting units
    units(Data[, StandardUnitTable$ItemID[i]]) <- input[[StandardUnitTable$UnitMan[i]]]
    # converting units
    units(Data[, StandardUnitTable$ItemID[i]]) <- StandardUnitTable$StandardUnit[i]
    # remove units class
    units(Data[, StandardUnitTable$ItemID[i]]) <- NULL


  }

  setDT(Data)
  Data <- copy(Data)
  # # Units changing ####
  #
  # unitOptions <- c("mm", "cm", "dm", "m") # c("mm", "millimetre", "millimeter", "milimetro", "milimetrica", "cm", "centimetre", "centimeter", "centimetro", "dm", "decimetre", "decimeter", "decimetro", "m", "metre", "meter", "metro")
  #
  # AreaUnitOptions <- c("m2", "ha", "km2")

  # ### Diameter, MinDBH and BD in cm ####
  # # if((!input$Diameter %in% "none" & !input$DiameterUnit %in% "none") | (!input$Circ %in% "none" & !input$CircUnit %in% "none")) stop("We have not coded the case where size units are not constant across your data yet - Please contact us or unify your units first.")
  #
  # if(!input$Diameter %in% "none" | !input$Circ %in% "none") {
  #
  #   SizeUnit <- grep("[^none]", c(input$DiameterUnitMan, input$CircUnitMan), value = T)[1] # take Diameter in priority, otherwise CircUnit (not a big deal since we only care about Diameter and we already converted it from Circ if that was the only size we had)
  #
  #   if(!SizeUnit %in% unitOptions) stop(paste("Your tree size units are not one of:", paste(unitOptions, collapse = ", ")))
  #
  #   if(SizeUnit %in% unitOptions) {
  #
  #   if (SizeUnit == "mm") Data[, Diameter := Diameter/10] # mm -> cm
  #
  #   if (SizeUnit == "dm") Data[, Diameter := Diameter*10] # dm -> cm
  #
  #   if (SizeUnit == "m") Data[, Diameter := Diameter*100] # m -> cm
  #   }
  #
  #   # (re)calculate Circ
  #   Data[, Circ := round(Diameter*pi, 2)]
  # }
  #
  # if(!input$BD %in% "none" | !input$BCirc %in% "none") {
  #
  #   BSizeUnit <- grep("[^none]", c(input$BDUnitMan, input$BCircUnitMan), value = T)[1] # take Diameter in priority, otherwise CircUnit (not a big deal since we only care about Diameter and we already converted it from Circ if that was the only size we had)
  #
  #   if(!BSizeUnit %in% unitOptions) stop(paste("Your basal size units are not one of:", paste(unitOptions, collapse = ", ")))
  #
  #   if(BSizeUnit %in% unitOptions) {
  #     if (BSizeUnit == "mm") Data[, BD := BD/10] # mm -> cm
  #
  #     if (BSizeUnit == "dm") Data[, BD := BD*10] # dm -> cm
  #
  #     if (BSizeUnit == "m") Data[, BD := BD*100] # m -> cm
  #   }
  #
  #   Data[, BCirc := round(BD*pi, 2)]
  # }
  #
  # if(!input$MinDBH %in% "none") {
  #
  #   SizeUnit <- input$MinDBHUnitMan
  #
  #   if(!SizeUnit %in% unitOptions) stop(paste("Your minimum DBH size units are not one of:", paste(unitOptions, collapse = ", ")))
  #
  #   if(SizeUnit %in% unitOptions) {
  #
  #     if (SizeUnit == "mm") Data[, MinDBH := MinDBH/10] # mm -> cm
  #
  #     if (SizeUnit == "dm") Data[, MinDBH := MinDBH*10] # dm -> cm
  #
  #     if (SizeUnit == "m") Data[, MinDBH := MinDBH*100] # m -> cm
  #   }
  #
  # }
  #
  # ### HOM and BHOM in m ####
  # # if(!input$HOM %in% "none" & !input$HOMUnit %in% "none") stop("We have not coded the case where HOM units are not constant across your data yet - Please contact us or unify your units first.")
  #
  # if(!input$HOM %in% "none") {
  #
  #   # if(input$HOMUnitMan %in% "none") stop("we need HOM units")
  #
  #   HOMUnit <- input$HOMUnitMan
  #
  #   if(!HOMUnit %in% unitOptions) stop(paste("Your HOM units are not one of:", paste(unitOptions, collapse = ", ")))
  #
  #   if (HOMUnit %in% unitOptions) {
  #
  #     if (HOMUnit == "mm") Data[, HOM := HOM/1000] # mm -> m
  #
  #     if (HOMUnit == "cm") Data[, HOM := HOM/100] # cm -> m
  #
  #
  #     if (HOMUnit == "dm") Data[, HOM := HOM/10] # dm -> m
  #   }
  # }
  #
  # if(!input$BHOM %in% "none") {
  #
  #   # if(input$BHOMUnitMan %in% "none") stop("we need basal HOm units")
  #
  #
  #   BHOMUnit <- input$BHOMUnitMan
  #
  #   if(!BHOMUnit %in% unitOptions) stop(paste("Your basal HOM units are not one of:", paste(unitOptions, collapse = ", ")))
  #
  #   if (BHOMUnit %in% unitOptions) {
  #
  #     if (BHOMUnit == "mm") Data[, BHOM := BHOM/1000] # mm -> m
  #
  #     if (BHOMUnit == "cm") Data[, BHOM := BHOM/100] # cm -> m
  #
  #     if (BHOMUnit == "dm") Data[, BHOM := BHOM/10] # dm -> m
  #   }
  # }
  #
  #
  # ### TreeHeight in m ####
  # # if(!input$TreeHeight %in% "none" & !input$TreeHeightUnit %in% "none") stop("We have not coded the case where height units are not constant across your data yet - Please contact us or unify your units first.")
  #
  #
  # if(!input$TreeHeight %in% "none") {
  #
  #   # if(input$TreeHeightUnitMan %in% "none") stop("we need tree height units")
  #
  #   TreeHeightUnit <- input$TreeHeightUnitMan
  #
  #   if(!TreeHeightUnit %in% unitOptions) stop(paste("Your height units are not one of:", paste(unitOptions, collapse = ", ")))
  #
  #   if (TreeHeightUnit %in% unitOptions) {
  #
  #     if (TreeHeightUnit == "mm") Data[, TreeHeight := TreeHeight/1000] # mm -> m
  #
  #     if (TreeHeightUnit == "cm") Data[, TreeHeight := TreeHeight/100] # cm -> m
  #
  #     if (TreeHeightUnit == "dm") Data[, TreeHeight := TreeHeight/10] # dm -> m
  #   }
  # }
  #
  #
  #
  #
  ### PlotArea in ha ####

  # if(!input$PlotArea %in% "none") {
  #
  #   # if(input$PlotAreaUnitMan %in% "none") stop("we need Plot Area units")
  #
  #   PlotAreaUnit <- input$PlotAreaUnitMan
  #
  #   if(!PlotAreaUnit %in% AreaUnitOptions) stop(paste("Your plot area units are not one of:", paste(AreaUnitOptions, collapse = ", ")))
  #
  #   if (PlotAreaUnit %in% AreaUnitOptions) {
  #
  #     if (PlotAreaUnit == "m2") Data[, PlotArea := PlotArea/10000] # m2 -> ha
  #
  #     if (PlotAreaUnit == "km2") Data[, PlotArea := PlotArea*100] # km2 -> ha
  #   }
  # }



  # ### SubplotArea in ha ####
  #
  # if(!input$SubplotArea %in% "none") {
  #
  #   SubplotAreaUnitMan <- input$SubplotAreaUnitMan
  #
  #   if(!SubplotAreaUnitMan %in% AreaUnitOptions) stop(paste("Your subplot area units are not one of:", paste(AreaUnitOptions, collapse = ", ")))
  #
  #   if (SubplotAreaUnitMan %in% AreaUnitOptions){
  #
  #     if (SubplotAreaUnitMan == "m2") Data[, SubplotArea := SubplotArea/10000] # m2 -> ha
  #     if (SubplotAreaUnitMan == "km2") Data[, SubplotArea := SubplotArea*100] # km2 -> ha
  #   }
  # }
  #

  #
  # ### XY coordinates in m ####
  #
  #
  # if(!input$XTreeUTM %in% "none") {
  #
  #   TreeUTMUnitMan <- input$TreeUTMUnitMan
  #
  #   if(!TreeUTMUnitMan %in% unitOptions) stop(paste("Your utm units are not one of:", paste(unitOptions, collapse = ", ")))
  #
  #   if (TreeUTMUnitMan %in% unitOptions) {
  #
  #     if (TreeUTMUnitMan == "mm") {
  #       Data[, XTreeUTM := XTreeUTM/1000] # mm -> m
  #       Data[, YTreeUTM := YTreeUTM/1000] # mm -> m
  #     }
  #
  #     if (TreeUTMUnitMan == "cm") {
  #       Data[, XTreeUTM := XTreeUTM/100] # cm -> m
  #       Data[, YTreeUTM := YTreeUTM/100] # cm -> m
  #
  #       }
  #
  #     if (TreeUTMUnitMan == "dm") {
  #       Data[, XTreeUTM := XTreeUTM/10] # dm -> m
  #       Data[, YTreeUTM := YTreeUTM/10] # dm -> m
  #       }
  #
  #   }
  # }
  #
  # if(!input$XTreePlot %in% "none") {
  #
  #   TreePlotUnitMan <- input$TreePlotUnitMan
  #
  #   if(!TreePlotUnitMan %in% unitOptions) stop(paste("Your plot coordinates units are not one of:", paste(unitOptions, collapse = ", ")))
  #
  #   if (TreePlotUnitMan %in% unitOptions) {
  #
  #     if (TreePlotUnitMan == "mm") {
  #       Data[, XTreePlot := XTreePlot/1000] # mm -> m
  #       Data[, YTreePlot := YTreePlot/1000] # mm -> m
  #     }
  #
  #     if (TreePlotUnitMan == "cm") {
  #       Data[, XTreePlot := XTreePlot/100] # cm -> m
  #       Data[, YTreePlot := YTreePlot/100] # cm -> m
  #
  #     }
  #
  #     if (TreePlotUnitMan == "dm") {
  #       Data[, XTreePlot := XTreePlot/10] # dm -> m
  #       Data[, YTreePlot := YTreePlot/10] # dm -> m
  #     }
  #
  #   }
  # }
  #
  # if(!input$XTreeSubplot %in% "none") {
  #
  #   TreeSubplotUnitMan <- input$TreeSubplotUnitMan
  #
  #   if(!TreeSubplotUnitMan %in% unitOptions) stop(paste("Your subplot coordinates units are not one of:", paste(unitOptions, collapse = ", ")))
  #
  #   if (TreeSubplotUnitMan %in% unitOptions) {
  #
  #     if (TreeSubplotUnitMan == "mm") {
  #       Data[, XTreeSubplot := XTreeSubplot/1000] # mm -> m
  #       Data[, YTreeSubplot := YTreeSubplot/1000] # mm -> m
  #     }
  #
  #     if (TreeSubplotUnitMan == "cm") {
  #       Data[, XTreeSubplot := XTreeSubplot/100] # cm -> m
  #       Data[, YTreeSubplot := YTreeSubplot/100] # cm -> m
  #
  #     }
  #
  #     if (TreeSubplotUnitMan == "dm") {
  #       Data[, XTreeSubplot := XTreeSubplott/10] # dm -> m
  #       Data[, YTreeSubplot := YTreeSubplot/10] # dm -> m
  #     }
  #
  #   }
  # }

  # show warnings
  if(!is.null(AllWarnings)) warning(paste(AllWarnings, collapse = "\n"))

  # return output ####
  ColumnsToReturn <- intersect(c(x$ItemID, grep("Original", colnames(Data), value = T)), colnames(Data))
  # ColumnsToReturn <- ColumnsToReturn[unlist(Data[, lapply(.SD, function(x) !all(is.na(x))), .SDcols = ColumnsToReturn] )]
  return(Data[, ..ColumnsToReturn])

}

