#' Plot life status correction result
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdTree` (character)
#'   - `Year` (numeric)
#'   - `LifeStatus` (logical or character)
#'   - `LifeStatus_DataHarmonizationCor` (logical or character)
#'
#' @param OnlyCorrected TRUE: plot only corrected trees, FALSE: plot all trees
#'   (logical)
#'
#' @param SeveralWindows TRUE: return each page in a new window (better
#'   visualisation in Rstudio), FALSE: return each page in the same window
#'   (needed to save all the pages) (logical)
#'
#' @return The plots of the initial diameter values and proposed corrections, by
#'   IdStem/IdTree
#'
#' @importFrom ggplot2 ggplot geom_point geom_line aes theme_minimal
#'    scale_colour_manual labs vars
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom grDevices dev.new
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' data(TestData)
#' Rslt <- StatusCorrection(TestData)
#' StatusCorrectionPlot(Rslt, SeveralWindows = TRUE)
#'}
#'
StatusCorrectionPlot <- function(
  Data,
  OnlyCorrected = TRUE,
  # CorCol = "LifeStatus_DataHarmonizationCor",
  # InitialCol = "LifeStatus",
  # FileName = "StatusCorrectionPlots.pdf"
  SeveralWindows = TRUE
){

  ThisIsShinyApp =  shiny::isRunning() # this is for internal use when function used by Shiny app

  #### Arguments check ####

  # Data
  if(!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # IdStem or IdTree? ---------------------------------------------------------------------------------------
  # If no IdStem take IdTree
  if((!"IdStem" %in% names(Data) | all(is.na(Data$IdStem))) &
     ("IdTree" %in% names(Data) & any(!is.na(Data$IdTree))) ){ ID <- "IdTree"

  }else{ ID <- "IdStem"}

  if(!any(c("IdStem", "IdTree") %in% names(Data)) | (all(is.na(Data$IdStem)) &  all(is.na(Data$IdTree))) )
    stop("The 'IdStem' or 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------


  # Columns
  # IdTree, Year, LifeStatus, LifeStatus_DataHarmonizationCor
  if(!all(c("Year", "LifeStatus", "LifeStatus_DataHarmonizationCor", "ScientificName") %in% names(Data)))
    stop("'Year', 'LifeStatus', 'LifeStatus_DataHarmonizationCor', 'ScientificName' should be columns of Data")

  if(!"ScientificName_DataHarmonizationCor" %in% names(Data)) {
    Data[, ScientificName_DataHarmonizationCor := ScientificName]
  }



  #### Function ####

  Data[,LifeStatus_DataHarmonizationCor := as.character(LifeStatus_DataHarmonizationCor)]
  Data[,LifeStatus := as.character(LifeStatus)]
  Data[is.na(LifeStatus_DataHarmonizationCor), LifeStatus_DataHarmonizationCor := "NA"]
  Data[is.na(LifeStatus), LifeStatus := "NA"]

  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[order(get(ID), Year)]

  if(OnlyCorrected == TRUE){
    # Only corrected stems ----------------------------------------------------------------------------------------------
    IDCor <- Data[LifeStatus != LifeStatus_DataHarmonizationCor, get(ID)] #  corrected stems

    DataCor <- Data[get(ID) %in% IDCor] #  corrected stems

  }else{
    DataCor <- Data
    IDCor <- Data[, get(ID)]
  }


  # Define nrow and ncol for the facet
  n <- length(unique(IDCor))
  if(n<3) { i = 1
  }else{ i = 3}


  # Plot --------------------------------------------------------------------------------------------------------------


 p <- ggplot(DataCor) +
      aes(x = IdCensus, y = LifeStatus) +


    geom_point(aes(color = "Initial"),  shape = "circle", size = 3.9) +
    geom_point(data = DataCor[LifeStatus == LifeStatus_DataHarmonizationCor, ], aes(color = "Conserved"),  shape = "circle", size = 3.9) +


        ggrepel::geom_text_repel(data = DataCor[grepl("Missed stem", Comment_DataHarmonization), ],
                                 label = "Missed stem",
                                 point.size = 3.9, size = 3, direction = "y") +

    geom_point(data = DataCor[grepl("Not processed", StatusCorrectionMeth_DataHarmonization), ], aes(color = "Not processed"),  shape = "circle", size = 3.9) +
    ggrepel::geom_text_repel(data = DataCor[grepl("Not processed", StatusCorrectionMeth_DataHarmonization), ],
                             aes(label = Comment_DataHarmonization),
                             point.size = 3.9, size = 3, direction = "y") +

        # not able to correct

        geom_point(data = DataCor[!is.na(LifeStatus) & is.na(LifeStatus_DataHarmonizationCor), ], aes(y = LifeStatus, color = "Not able to correct"),  shape = "circle", size = 3.9) +
        ggrepel::geom_text_repel(data =  DataCor[!is.na(LifeStatus) & is.na(LifeStatus_DataHarmonizationCor), ],
                                 aes(y = LifeStatus, label = Comment_DataHarmonization),
                                 point.size = 3.9, size = 3, direction = "y") +

      # Corrected
        geom_point(data = DataCor[LifeStatus != LifeStatus_DataHarmonizationCor & !is.na(LifeStatus_DataHarmonizationCor), ], aes(y = LifeStatus_DataHarmonizationCor, color = 'Corrected',  shape = StatusCorrectionMeth_DataHarmonization), size = 3.9) +


      # Colours
      scale_colour_manual(name = "Status", values = c("Conserved" = "black",
                                                      "Initial" = "red",
                                                      "Corrected" = "forestgreen",
                                                      "Not able to correct" = "pruple",
                                                      "Not processed" = "purple")) +
      theme_minimal() +

      # Titles
      labs(
        # title =  paste("ID: ",unique(DataCor[,get(ID)],""),
        x = "Census ID", y = "LifeStatus") +
    theme(legend.position = "bottom") +
    guides(color=guide_legend(nrow=5,byrow=TRUE, title.position = "top"),
           shape=guide_legend(nrow=5,byrow=TRUE, title.position = "top"))


  nPages <- ggforce::n_pages(p+
                               ggforce::facet_wrap_paginate(vars(get(ID), ScientificName_DataHarmonizationCor), scales = "free", ncol = min(n,3), nrow = i, page = 1))

  if(ThisIsShinyApp) {

    p <- lapply(seq_len( nPages), function(k)         p +   ggforce::facet_wrap_paginate(vars(get(ID), ScientificName_DataHarmonizationCor), scales = "free", ncol = min(n,3), nrow = i, page = k))

    return(list(p = p, nPages = nPages, ID = ID, n = n, i = i))

  } else {

    if(SeveralWindows == TRUE)
      dev.new()

    for(k in seq_len( nPages)) {
      print(k)
      print(
        p +   ggforce::facet_wrap_paginate(vars(get(ID), ScientificName_DataHarmonizationCor), scales = "free", ncol = min(n,3), nrow = i, page = k)

      )

      if(SeveralWindows == TRUE & k < nPages)
        dev.new()
    }
  }


  # return(Pl)

}
