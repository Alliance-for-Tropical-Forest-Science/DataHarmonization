#' Plot diameter correction result
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdStem` (character)
#'   - `IdCensus` (ordered factor)
#'   - `Diameter` (numeric)
#'   - `DBHCor` (numeric)
#'   - `HOM` (Height Of Measurement) (numeric)
#'   - `HOMCor` (Corrected Height Of Measurement) (numeric)
#'
#' @param OnlyCorrected TRUE: plot only corrected stems, FALSE: plot all stems
#'   (logical)
#'
#' @param SeveralWindows TRUE: return each page in a new window (better
#'   visualisation in Rstudio), FALSE: return each page in the same window
#'   (needed to save all the pages) (logical)
#'
#' @param CorCol Corrected column name (character)
#'
#' @return The plots of the initial diameter values and proposed corrections, by
#'   IdStem.
#'
#' @importFrom ggplot2 ggplot geom_point geom_line aes theme_minimal guides theme guide_legend
#'   position_nudge scale_colour_manual labs vars ggplot_build
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom ggrepel geom_text_repel
#' @importFrom grDevices dev.new
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # pdf("DiameterCorrectionPlots_TestData.pdf", width = 25, height = 10)
#'
#'  data(TestData)
#'
#' TestData$HOM[1:3] <- c(0.5,1.5,NA)
#' TestData$Diameter[1:3] <- TestData$Diameter[1:3] + c(2,-2,0)
#' TestData$Diameter[21:23] <- c(31,91,14)
#'
#' Rslt <- DiameterCorrection(
#'  TestData,
#'   WhatToCorrect = c("POM change","Abnormal growth"),
#'    CorrectionType = c("phylo"),
#'     MinIndividualNbr = 1)
#' DiameterCorrectionPlot(Rslt, OnlyCorrected = TRUE, SeveralWindows = FALSE)
#'
#' # dev.off()
#'}
#'
DiameterCorrectionPlot <- function(
    Data,
    OnlyCorrected = TRUE,
    CorCol = "Diameter_DataHarmonizationCor",
    # InitialCol = "Diameter"
    SeveralWindows = TRUE
){

  ThisIsShinyApp =  shiny::isRunning() # this is for internal use when function used by Shiny app

  #### Arguments check ####

  # Data ---------------------------------------------------------------------------------------------------------------
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


  # POM or HOM? ----------------------------------------------------------------------------------------------------------
  # If no HOM take POM
  if((!"HOM" %in% names(Data) | all(is.na(Data$HOM))) &
     ("POM" %in% names(Data) & any(!is.na(Data$POM))) ){ POMv <- "POM"

  }else{ POMv <- "HOM"}

  if((!"HOM_DataHarmonizationCor" %in% names(Data) | all(is.na(Data$HOM_DataHarmonizationCor))) &
     ("POM_DataHarmonizationCor" %in% names(Data) & any(!is.na(Data$POM_DataHarmonizationCor))) ){ POMcorv <- "POM_DataHarmonizationCor"

  }else{ POMcorv <- "HOM_DataHarmonizationCor"}


  # Columns --------------------------------------------------------------------------------------------------------------
  # IdStem, IdCensus, Diameter, DBHCor, HOM, HOMCor
  if(!all(c("IdCensus", "Diameter", "ScientificName", CorCol, POMv, POMcorv) %in% names(Data)))
    stop(paste0("'IdCensus', 'Diameter', 'ScientificName', ",CorCol,"', '",POMv,"', ",POMcorv,"' should be columns of Data"))

  if(!"ScientificName_DataHarmonizationCor" %in% names(Data)) {
    Data[, ScientificName_DataHarmonizationCor := ScientificName]
  }


  #### Function ####

  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[order(get(ID), IdCensus)]

  if(OnlyCorrected == TRUE){
    # Only corrected stems ----------------------------------------------------------------------------------------------
    IDCor <- Data[Diameter != get(CorCol) | (is.na(get(CorCol)) & !is.na(Diameter)) | (!is.na(get(CorCol)) & is.na(Diameter)), get(ID)] #  corrected stems

    DataCor <- Data[get(ID) %in% IDCor, ] #  corrected stems

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
    aes(x = IdCensus) +


    geom_point(aes(y = Diameter, color = "Initial"),  shape = "circle", size = 3.9) +
    # geom_line(linetype = "dotted", color = "grey") +
    ggrepel::geom_text_repel(data = DataCor[ !is.na(Diameter) & !is.na(get(POMv)),],
                             aes(y = Diameter, label = get(POMv), colour = paste("initial", POMv)),
                             point.size = 3.9, size = 3, direction = "y") +

    geom_point(data = DataCor[Diameter == get(CorCol), ], aes(y = get(CorCol), color = "Conserved"),  shape = "circle", size = 3.9) +

    ggrepel::geom_text_repel(data = DataCor[grepl("Missed measurement", Comment_DataHarmonization), ],
                             aes(y = get(CorCol)),
                             label = "Missed measurement",
                             point.size = 3.9, size = 3, direction = "y")  +

    ggrepel::geom_text_repel(data = DataCor[grepl("Missed stem", Comment_DataHarmonization), ],
                             aes(y = get(CorCol)),
                             label = "Missed stem",
                             point.size = 3.9, size = 3, direction = "y") +

    geom_point(data = DataCor[grepl("Missed recruit", Comment_DataHarmonization), ], aes(y = get(CorCol), shape = DiameterCorrectionMeth_DataHarmonization), color = "forestgreen", size = 3.9) +


    ggrepel::geom_text_repel(data = DataCor[grepl("Missed recruit", Comment_DataHarmonization), ],
                             aes(y = get(CorCol)),
                             label = "Missed recruit",
                             point.size = 3.9, size = 3, direction = "y") +


    geom_point(data = DataCor[!is.na(Diameter) & is.na(get(CorCol)), ], aes(y = Diameter, color = "Not able to correct"),  shape = "circle", size = 3.9) +
    ggrepel::geom_text_repel(data =  DataCor[!is.na(Diameter) & is.na(get(CorCol)), ],
                             aes(y = Diameter, label = Comment_DataHarmonization),
                             color = "purple",
                             point.size = 3.9, size = 3, direction = "y") +

    geom_point(data = DataCor[(Diameter != get(CorCol)) | (is.na(Diameter) & !is.na(get(CorCol))), ], aes(y = get(CorCol), color = "Corrected", shape = DiameterCorrectionMeth_DataHarmonization), size = 3.9) +
    # geom_line(data = DataCor[!is.na(Diameter) & !is.na(get(CorCol)) & Diameter != get(CorCol), ], aes(y =  get(CorCol), color = "Corrected")) +
    ggrepel::geom_text_repel(data = DataCor[ !is.na(get(CorCol)) & !is.na(get(POMcorv)) & (Diameter != get(CorCol)) | is.na(Diameter), ],
                             aes(y = get(CorCol), label = get(POMcorv), colour = paste("new", POMv)),
                             point.size = 3.9, size = 3, direction = "y") +

    # geom_line(data = DataCor[!is.na(get(CorCol)), ], aes(y =  get(CorCol))) +

    scale_colour_manual(name = "Status", values = c("Conserved" = "black",
                                                    # {if(nrow(subset(DataCor, !is.na(Diameter) & is.na(get(CorCol)))) > 0)
                                                    #   "Duplicated measurement" = "grey" },
                                                    "Initial" = "red",
                                                    "Not able to correct" = "purple",
                                                    "Corrected" = "forestgreen",
                                                    # "Methode" = "purple",
                                                    "initial HOM" = "grey40",
                                                    "initial POM" = "grey40",
                                                    "new HOM" = "darkgreen",
                                                    "new POM" = "darkgreen"))
  # +
  #     scale_x_continuous(labels = levels(DataCor$IdCensus))


  p  <- p + guides(shape= guide_legend(ncol = 2, override.aes = list(color = "forestgreen")),
           colour=guide_legend(ncol=2, override.aes = list(shape = c(16,16,16,NA,NA,16)[1:(ggplot_build(p)$plot)$scales$scales[[1]]$n.breaks.cache], linetype = 0))) +
    theme_minimal() +
    theme(legend.position = "bottom") +

    # Titles
    labs(
      # title =  paste("ID: ",unique(DataCor[, get(ID)]),""),
      x = "Census", y = "Diameter (cm)", shape = "Correction Method")


  nPages <- ggforce::n_pages(p+
    ggforce::facet_wrap_paginate(vars(get(ID), ScientificName_DataHarmonizationCor), scales = "free", ncol = min(n,3), nrow = i, page = 1))

  if(ThisIsShinyApp) {

    p <- lapply(seq_len( nPages), function(k)         p +   ggforce::facet_wrap_paginate(vars(get(ID), ScientificName_DataHarmonizationCor), scales = "free", ncol = min(n,3), nrow = i, page = k))

    return(list(p = p, nPages = nPages, ID = ID, n = n, i = i))

    } else {

      if(SeveralWindows == TRUE)
    dev.new()

  for(k in seq_len( nPages)) {
    print(
      p +   ggforce::facet_wrap_paginate(vars(get(ID), ScientificName_DataHarmonizationCor), scales = "free", ncol = min(n,3), nrow = i, page = k)

    )

    if(SeveralWindows == TRUE & i < nPages)
      dev.new()
  }
  }



  # return(Pl)

}
