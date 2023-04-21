#' General Errors Detection
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @details Detect errors
#'   - Remove **duplicated rows**
#'   - Check **missing value** in
#'      X-YTreeUTM/PlotArea/Plot/Subplot/Year/TreeFieldNum/
#'      IdTree/IdStem/Diameter/POM/HOM/Family/Genus/Species/VernName
#'   - Check **missing value** (NA/0) in the measurement variables: "Diameter",
#'      "HOM", "TreeHeight", "StemHeight"
#'   - Check of the **unique association of the IdTree with plot, subplot**
#'      **and TreeFieldNum** (at the site scale)
#'   - Check **duplicated IdTree/IdStem** in a census (at the site scale)
#'   - Check for trees **outside the subplot** (not implemented yet)
#'   - Check **invariant coordinates per IdTree/IdStem**
#'   - Check **fix Plot and Subplot number** (not implemented yet)
#'
#'
#' @return The input dataset (data.table) with a new *Comment* column with error
#'   type informations.
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data("TestData")
#'
#' Rslt <- GeneralErrorsDetection(TestData)
#'
GeneralErrorsDetection <- function(
    Data
){

  ThisIsShinyApp =  shiny::isRunning() # this is for internal use when function used by Shiny app


  #### Arguments check ####

  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # IdStem or IdTree? ---------------------------------------------------------------------------------------
  # If no IdStem take IdTree
  if((!"IdStem" %in% names(Data) | all(is.na(Data$IdStem))) &
     ("IdTree" %in% names(Data) & any(!is.na(Data$IdTree))) ){
    ID <- "IdTree"
    Data[, IdTree := as.character(IdTree)]

  }else{ ID <- "IdStem"
  Data[, IdStem := as.character(IdStem)]
  }

  if(!any(c("IdStem", "IdTree") %in% names(Data)) | (all(is.na(Data$IdStem)) &  all(is.na(Data$IdTree))) )
    stop("The 'IdStem' or 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------

  Data[, Subplot := as.character(Subplot)]

  #### Function ####

  # In data.table
  setDT(Data)
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environment

  if(!"Comment_DataHarmonization" %in% names(Data)) Data[, Comment_DataHarmonization := ""]


  # Check duplicate rows ------------------------------------------------------------------------------------
  # if there are duplicate rows, delete them

  if(anyDuplicated(Data) != 0)
    Data <- unique(Data)


  # Missing values ----------------------------------------------------------------------------------------------------
  # If the column exists, but has NA values

  # Check bota : Family/Genus/Species/ScientificName/VernName
  # Check size : Diameter, POM(?)
  Vars <- c("Plot", "Subplot", "Year", "TreeFieldNum", "IdTree", "IdStem",
            "Diameter", "POM", "HOM", "TreeHeight", "StemHeight",
            "XTreeUTM", "YTreeUTM", "Family", "Genus", "Species", "VernName")

  for (v in 1:length(Vars)) {

    if (Vars[v] %in% names(Data)){ # If the column exists
      if(!all(is.na(Data[,get(Vars[v])]))){ # if the column is not completely empty

        Data[is.na(get(Vars[v])), Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, paste0("Missing value in ", Vars[v]))]
        # Data <- GenerateComment(Data,
        #                         condition = is.na(Data[,get(Vars[v])]),
        #                         comment = paste0("Missing value in ", Vars[v]))

        # warning(paste0("Missing value in ", Vars[v]))

      } # not empty column
    } # column exists
  } # Vars loop

  # Data[grepl("Missing value", Comment_DataHarmonization)] # to check


  # Measurement variables = 0 -----------------------------------------------------------------------------------------

  Vars <- c("Diameter", "HOM", "TreeHeight", "StemHeight")

  for (v in 1:length(Vars)) {
    if(Vars[v] %in% names(Data)){ # If the column exists

      Data[get(Vars[v]) %in% 0, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization," cannot be 0")]
      # Data <-
      #   GenerateComment(
      #     Data,
      #     condition = (Data[, get(Vars[v])] == 0) &
      #       !is.na(Data[, get(Vars[v])]),
      #     comment = paste0(Vars[v], " cannot be 0")
      #   )

      # warning(paste0(Vars[v]," cannot be 0"))
    }
  }


  # Data[grepl("cannot be 0", Comment)] # to check



  # Check duplicated TreeFieldNum in plot-subplot association ---------------------------------------------------------

  # Create "PlotSubNum" = "Site/Year/Plot/Subplot/TreeFieldNum"
  # Data[, PlotSubNum := paste(Site, Year, Plot, Subplot, TreeFieldNum, sep = "/")]
  #
  # # y = 2017
  # # p=1
  # # c= 3
  # duplicated_num <- num <- vector("character")
  #
  # # if any duplicats in this col
  # if(anyDuplicated(Data$PlotSubNum) != 0) {
  #   # For each site
  #   for (s in unique(na.omit(Data$Site))) {
  #     # For each census
  #     for (y in unique(na.omit(Data$Year))) {
  #       # For each plot
  #       for (p in unique(na.omit(Data$Plot))) {
  #         # For each Subplot in this plot
  #         for (c in unique(na.omit(Data[Data$Plot==p, Subplot]))) {
  #
  #           num <- Data[Data$Site == s & Data$Year == y
  #                       & Data$Plot == p & Data$Subplot == c]$TreeFieldNum # all the TreeFieldNum for each Plot-Subplot combination
  #
  #           # if there are several TreeFieldNum per Plot-Subplot combination
  #           if(anyDuplicated(num) != 0){
  #             duplicated_num <- unique(num[duplicated(num)])
  #
  #             Data <- GenerateComment(Data,
  #                                     condition =
  #                                       Data[,Site] == s & Data[,Year] == y
  #                                     & Data[,Plot] == p & Data[,Subplot] == c
  #                                     & Data[,TreeFieldNum] %in% duplicated_num,
  #                                     comment = "Duplicate TreeFieldNum in the same Plot and Subplot")
  #
  #             num <- vector("character")
  #
  #             warning("Duplicate TreeFieldNum(s) (",duplicated_num,") in the same Plot (",p,") and Subplot (",c,"), in ",y,"")
  #
  #           } else {num <- vector("character")}
  #         } # end subplot loop
  #       } # end plot loop
  #     } # end year loop
  #   } # end site loop
  # }
  #
  # Data[, PlotSubNum := NULL]
  # Data[TreeFieldNum == duplicated_num,.(Year = sort(Year), Plot, Subplot, TreeFieldNum, Comment)] # to check (1 duplicate)


  # Check of the unique association of the IdTree/IdStem with Plot-Subplot-TreeFieldNum, at the site scale -------------------

  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Data$Site))) {

    correspondances <- na.omit(unique(
      Data[Site == s, .(IdTree, Plot, Subplot, TreeFieldNum)]
    ))

    CorresIDs <- correspondances[, IdTree] # .(IdTree) all the Idtree's having a unique P-SubP-TreeFieldNum combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      Data[Site %in% s, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "Non-unique association of the IdTree with Plot, Subplot and TreeFieldNum")]

      # Data <-
      #   GenerateComment(Data,
      #                   condition = (Data[, Site] == s) &
      #                     (Data[, IdTree] %in% duplicated_ID),
      #                   comment = "Non-unique association of the IdTree with Plot, Subplot and TreeFieldNum")

      DuplicatedID <- unique(Data[IdTree %in% duplicated_ID,
                                  .(IdTree, Plot, Subplot, TreeFieldNum)])
      DuplicatedID <- DuplicatedID[order(IdTree)]

      b <- capture.output(DuplicatedID)
      c <- paste(b, "\n", sep = "")

      warning("Non-unique association of IdTree(s) with Plot, Subplot and TreeFieldNum:\n", c, "\n")

    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), Plot, Subplot, TreeFieldNum, Comment)]) # to check


  # Check duplicated IdTree/IdStem in a census ------------------------------------------------------------------------
  DuplicatedID <-
    Data[duplicated(Data[, list(get(ID), Year)]), list(get(ID), Year)]

  if(nrow(DuplicatedID) > 0){

    DuplicatedID[, IDYear := paste(V1, Year, sep = "/")] # code to detect

    Data[, IDYear := paste(get(ID), Year, sep = "/")] # code to detect


    Data[IDYear %in% DuplicatedID[, IDYear], Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, paste0("Duplicated '", ID, "' in the census"))]

    # Data <- GenerateComment(
    #   Data,
    #   condition = Data$IDYear %in% DuplicatedID[, IDYear],
    #   comment = paste0("Duplicated '", ID, "' in the census")
    # )

    a <- Data[
      IDYear %in% DuplicatedID[, IDYear],
      .(Year, Plot, Subplot, TreeFieldNum, get(ID))
    ]
    setnames(a, "V5", ID, skip_absent=TRUE)
    a <- a[order(get(ID), Year)]
    b <- capture.output(a)
    c <- paste(b, "\n", sep = "")

    warning("Duplicated '", ID, "' in the census:\n", c, "\n")
    warning("If these duplicates are normal in your protocol (because there are
            several measurements per year), you can leave your dataset as it is,
            the corrections taking into account one measurement per year will
            consider the data as correct according to the 'KeepMeas' argument.
            If these duplicates are abnormal according to your protocol,
            we advise you to process them before applying the corrections.")

    Data[, IDYear := NULL]
  }

  # Data[grepl("Duplicated", Comment)] # to check


  # TODO Check for trees outside the subplot
  # ---------------------------------------------------------------------
  # Compare PlotArea with the MCP (Minimum Convex Polygon) area of the trees
  # within the plot and return an error if the MCP area > x% plotArea


  # Check invariant coordinates per IdTree/IdStem ------------------------------

  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Data$Site))) {
    CoordIDCombination <- na.omit(unique(Data[Site == s, c(ID, "XTreeUTM", "YTreeUTM"), with = FALSE]))

    CorresIDs <-
      CoordIDCombination[, get(ID)] # .(IdTree) all the Idtree's having a unique X-YTreeUTM) combination

    if (!identical(CorresIDs, unique(CorresIDs))) {
      # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <-
        unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      Data[Site %in% s & get(ID) %in% duplicated_ID, Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, paste0(
        "Different coordinates (XTreeUTM, YTreeUTM) for a same '",
        ID,
        "'"
      ))]


      # Data <- GenerateComment(
      #   Data,
      #   condition =
      #     Data[, Site] == s
      #   & Data[, get(ID)] %in% duplicated_ID,
      #   comment = paste0(
      #     "Different coordinates (XTreeUTM, YTreeUTM) for a same '",
      #     ID,
      #     "'"
      #   )
      # )

      warning(
        paste0(
          "Different coordinates (XTreeUTM, YTreeUTM) for a same '",
          ID,
          "' (",
          duplicated_ID,
          ")"
        )
      )

    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), XTreeUTM, YTreeUTM, Comment)]) # check


  # TODO Check fix Plot and Subplot number -----------------------
  # alert when the number of sub-plots/plots varies from year to year


  return(Data)
}
