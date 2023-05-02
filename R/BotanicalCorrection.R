# Auxiliary functions:

# Function to ensure that the first letter of any string is uppercase
toUpperFirst <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


#' Botanical Correction
#'
#' @param Data Dataset (data.frame or data.table); it must contain Site and IdTree
#' @param Sources Character vector. Taxonomic source(s) to use. Options are c("tropicos", "usda", "wfo", "wcvp") and default is all of them.
#'
#' @return A list with a complete log of the botanical corrections and Data with new columns:
#'   - `Accepted_family_DataHarmonizationCor` (character): corrected family name
#'   - `Accepted_genus_DataHarmonizationCor` (character): corrected genus name
#'   - `Accepted_species_DataHarmonizationCor` (character): corrected species name
#'   - `Accepted_name_DataHarmonizationCor` (character): corrected name (at any taxonomic level)
#'   - `Accepted_name_rank_DataHarmonizationCor` (character): Taxonomic rank of name matched by the TNRS

#'
#'@details
#' - The function uses TNRS package provided by BIEN, https://bien.nceas.ucsb.edu/bien/tools/tnrs/
#' - No special characters (typography)
#' - The suffix "aceae" is restricted to families, and words ending with "aceae" are deleted anywhere else
#' - Correct spelling of botanical names
#' - Update the families according to APG
#' - The log contains a flag if there is more than one name for one Site x IdTree combination
#' - The log contains all details returned by TNRS (authors, etc.). Data just keeps the basic corrected names.
#'
#'@importFrom stats na.omit
#'@importFrom stringr str_squish str_replace_all
#'@importFrom TNRS TNRS
#'
#' @export
#'
#' @examples
#'\dontrun{
#' library(data.table)
#' data(TestData)
#' Rslt <- BotanicalCorrection(TestData)
#'}
#'
#'
BotanicalCorrection <- function(Data,  Sources = c("tropicos", "usda", "wfo", "wcvp")) {

  ThisIsShinyApp =  shiny::isRunning() # this is for internal use when function used by Shiny app


  #### Arguments check ####
  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  #### Function ####

  setDT(Data) # data.frame to data.table
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environment
  Data[, IdTree := as.character(IdTree)]

  # Make sure that we have the columns that we need  ---------------------------------------------------------------------
  # (Mostly to avoid "if" statements and ramifications of the cases)
  if(!"Comment_DataHarmonization" %in% names(Data)) Data[, Comment_DataHarmonization := ""]
  if(!"Family" %in% names(Data)) Data[, Family := NA]
  if(!"ScientificName" %in% names(Data)) Data[, ScientificName := NA]
  if(!"Genus" %in% names(Data)) Data[, Genus := NA]
  if(!"Species" %in% names(Data)) Data[, Species := NA]
  if(!"Subspecies" %in% names(Data)) Data[, Subspecies := NA]
  if(!"Variety" %in% names(Data)) Data[, Variety := NA]

  # Not processed: "VernName", "Voucher", "IdLevel", "Authority"

  if(ThisIsShinyApp) incProgress(1/15)

  # General string manipulations (order matters)  ---------------------------------------------------------------------------------------------------------
  # space after point
  # _ to space
  # remove double spaces
  # strings to NA: "NA", " ", "", "NULL", "unknown", "none", "?"
  # make sure first letter is uppercase in: Family, ScientificName, Genus
  # remove special characters for Genus and Family (in Species we want to keep them)

  Vars <- c("Family", "ScientificName", "Genus", "Species", "Subspecies", "Variety")

  M <- as.matrix(Data[, ..Vars]) # we can use vectorized code on several columns
  original.name.ids <- paste(M[,"Family"],
                             M[,"ScientificName"],
                             M[,"Genus"],
                             M[,"Species"],
                             M[,"Subspecies"],
                             M[,"Variety"])

  rownames(M) <- original.name.ids

  M <- unique(M)

  M <- gsub(".", ". ", M, fixed = TRUE)
  M <- gsub("_", " ", M, fixed = TRUE)
  M <- matrix(stringr::str_squish(M), nrow = nrow(M), dimnames = dimnames(M))
  M[M %in% c("NA", "N/A", "na", " ", "", "NULL", "unknown", "none", "?", "??", "???", "...")] <- NA
  M[,c("Family", "ScientificName", "Genus")] <- toUpperFirst(M[,c("Family", "ScientificName", "Genus")])
  M[,"Family"] <- gsub("[[:punct:]]", "", M[,"Family"]) # !"#$%&’()*+,-./:;<=>?@[]^_`{|}~
  M[,"Genus"] <- gsub("[[:punct:]]", "", M[,"Genus"]) # !"#$%&’()*+,-./:;<=>?@[]^_`{|}~


  if(ThisIsShinyApp) incProgress(1/15)


  # Handle "Family" column and get the best guess of the family ----
  # One thing that could generate many problems is using short codes instead
  # of full family names, e.g. FAB or FABAC. instead of Fabaceae, etc.
  # This is relatively common practice and needs to be checked before moving forward.
  # TNRS is robust to the lack of families, but we do not want to remove content
  # from Family column entirely because it might be the only taxonomic information.
  # (Presence of text inside ScientificName is not enough proof that we know more than family,
  # as we could have things like Family = "Fabaceae" + ScientificName = "Indet. sp. 1";
  # we cannot expect in general that IdLevel column will be populated either).

  # Procedure:
  # first, try to match with the complete names
  # second, look for "aceae" words everywhere: keep elsewhere and remove from columns
  # lastly, remove anything remaining in Family, and use the space to store "aceae" words

  # Note that the behavior before (Vincyane's code) was to swap
  # and include into the Genus column any word without "aceae" in
  # the Family column. In the current version, it is KEY that the
  # user includes the genus into the ScientificName or Genus columns.
  # Redundancy by including genus into Family column won't affect.

  # (1) try to guess the family based on short codes
  txt <- M[,"Family"]
  #txt <- c("FAB.", "MELAS", "AST", "CLU", "PIP.") # for development ***

  cleantxt <- gsub(".", "", txt, fixed = TRUE)
  cleantxt <- gsub(" ", "", cleantxt, fixed = TRUE)

  fams <- taxize::apg_families
  fams <- unique(c(fams$family, fams$synonym, fams$original, fams$accepted_name))
  fams <- unique(unlist(strsplit(fams, "=", fixed = TRUE)))
  fams <- gsub(" ", "", fams, fixed = TRUE)
  fams <- gsub("?", "", fams, fixed = TRUE)
  fams <- unique(fams[grep("aceae", fams)])

  try.to.complete <- which(!grepl("aceae", cleantxt))
  versions <- na.omit(unique(nchar(cleantxt[try.to.complete])))
  guessed.family <- cleantxt
  for(NC in versions)  {
    w <- try.to.complete[which(nchar(cleantxt[try.to.complete]) == NC)]
    short <- substr(fams, 1, NC)
    names(short) <- fams
    unequivocal <- names(which(table(short) == 1))
    names(unequivocal) <- names(short)[match(unequivocal, short)]
    guessed.family[w] <- names(unequivocal)[match(tolower(cleantxt[w]), tolower(unequivocal))]
  }

  M[try.to.complete, "Family"] <- guessed.family[try.to.complete]

  if(ThisIsShinyApp) incProgress(1/15)

  # (2) find "aceae" words anywhere and keep them as the
  # most reliable source of information regarding family.
  # Remove all "aceae" words from their original locations.
  # (If multiple matches, this code does not cross-check; it just picks one)
  ends_in <- function(x, pattern = "aceae") { substr(x, nchar(x) - nchar(pattern) + 1, nchar(x)) == pattern}
  best.guess.family <- rep(NA, nrow(M))
  words <- strsplit(M, split = " ")
  aceae <- matrix(sapply(words, function(x) x[ends_in(x, "aceae")][1]), nrow = nrow(M)) # [1] picks one
  W <- which(!is.na(aceae), arr.ind = TRUE)
  W <- W[!duplicated(W[,"row"]),] # this also picks one
  best.guess.family[W[,"row"]] <- aceae[W]

  delete_aceae <- function(txt) {
    words <- strsplit(txt, split = " ")
    sapply(words, function(x) {
      x <- x[!is.na(x)]
      paste(x[!ends_in(x, "aceae")], collapse = " ")
    })
  }

  M[,"Family"] <- delete_aceae(M[,"Family"]) # will be deleted later anyway
  M[,"ScientificName"] <- delete_aceae(M[,"ScientificName"])
  M[,"Genus"] <- delete_aceae(M[,"Genus"])
  M[,"Species"] <- delete_aceae(M[,"Species"])
  M[,"Subspecies"] <- delete_aceae(M[,"Subspecies"])
  M[,"Variety"] <- delete_aceae(M[,"Variety"])


  if(ThisIsShinyApp) incProgress(1/15)

  # (3) use the "aceae" words found in previous step to
  # populate the Family column, and remove anything else
  # that remains in there.
  M[,"Family"] <- ""
  M[,"Family"] <- best.guess.family
  M[is.na(M[,"Family"]),"Family"] <- ""
  M[,c("Family", "ScientificName", "Genus")] <- toUpperFirst(M[,c("Family", "ScientificName", "Genus")])

  # Solve initials ----
  # E.g. {Family = "Fabaceae", ScientificName = "I. edulis", Genus = "Inga", ...}
  # This problem is a problem because of the order of concatenation.
  # Two possible forms: "A. " or "A " at the first letter. No genus has just 1 letter.
  # Single letters might have some meaning, like morfospecies:
  # {ScientificName = "A Inga", Genus = "Inga} and {ScientificName = "B Inga", Genus = "Inga}
  # but appending them to the end of ScientificName will mess with the subspecies and varieties:
  #TNRS::TNRS("Inga edulis X.")$Accepted_name # this will work
  #TNRS::TNRS("Inga edulis X. var. edulis")$Accepted_name # but this will fail

  # I get rid of initials entirely -- this might be source of some issues later.
  problems2 <- which(substr(M[,"ScientificName"], 1, 2) %in% paste0(LETTERS, " "))
  problems3 <- which(substr(M[,"ScientificName"], 1, 3) %in% paste0(LETTERS, ". "))
  M[problems2,"ScientificName"] <- paste(M[problems2,"Genus"], substr(M[problems2,"ScientificName"], 3, nchar(M[problems2,"ScientificName"])))
  M[problems3,"ScientificName"] <- paste(M[problems3,"Genus"], substr(M[problems3,"ScientificName"], 4, nchar(M[problems3,"ScientificName"])))

  # Besides, remove initials if they are present in the "Species" column:
  problems2 <- which(substr(M[,"Species"], 1, 2) %in% paste0(LETTERS, " "))
  problems3 <- which(substr(M[,"Species"], 1, 3) %in% paste0(LETTERS, ". "))
  M[problems2,"Species"] <- substr(M[problems2,"Species"], 3, nchar(M[problems2,"Species"]))
  M[problems3,"Species"] <- substr(M[problems3,"Species"], 4, nchar(M[problems3,"Species"]))


  if(ThisIsShinyApp) incProgress(1/15)

  # Solve redundancies between ScientificName and Species/Genus columns ----
  # A concatenation can fail if we incorporate the specific epithet twice,
  # as TNRS will try to find subspecies and varieties. It can also fail in
  # rare instances of invalid names where the specific epithet is the same
  # word as in the genus. Solve redundancies in both cases is safer.
  # https://stackoverflow.com/questions/19424709/r-gsub-pattern-vector-and-replacement-vector
  M <- matrix(stringr::str_squish(M), nrow = nrow(M), dimnames = dimnames(M))
  to.remove <- gsub(" ", "|", M[,"ScientificName"]) # any individual word
  replacements <- rep("", length(to.remove))
  names(replacements) <- to.remove # this is needed for the code to run in a vectorized way
  M[,"Species"] <- suppressWarnings(stringr::str_replace_all(M[,"Species"], replacements))
  M[,"Genus"] <- suppressWarnings(stringr::str_replace_all(M[,"Genus"], replacements))

  # Concatenate a single string to pass to TNRS ----
  M <- matrix(stringr::str_squish(M), nrow = nrow(M), dimnames = dimnames(M))
  M[is.na(M)] <- ""
  s <- ifelse(M[,"Subspecies"] == "", "", paste("subsp.", M[,"Subspecies"]))
  v <- ifelse(M[,"Variety"] == "", "", paste("var.", M[,"Variety"]))
  pass.this <- paste(M[,"Family"], M[,"ScientificName"], M[,"Genus"], M[,"Species"], s, v)
  pass.this <- stringr::str_squish(pass.this)


  if(ThisIsShinyApp) incProgress(1/15)

  # Call TNRS ---
  # Use multiple sources for names, but the classification of Tropicos = APG.
  pass.this.unique <- unique(pass.this)

  tnrs <- TNRS::TNRS(pass.this.unique,
                     sources = Sources,
                     classification = "tropicos")
if(length(pass.this.unique) !=  nrow(tnrs)) stop("some species did not pass through") else tnrs$Name_submitted <- pass.this.unique # this necessary when there is special characters

  if(ThisIsShinyApp) incProgress(1/15)

  # pass.this.unique[!pass.this.unique %in% tnrs$Name_submitted] # I do not know why, but sometimes we have this!
  # if(length(leftovers) > 0) {
  #   tnrs2 <- TNRS::TNRS(leftovers,
  #                      sources = Sources,
  #                      classification = "tropicos")
  #   tnrs <- rbind(tnrs, tnrs2)
  # }

  tnrs <- tnrs[match(pass.this, tnrs$Name_submitted), ]

  if(ThisIsShinyApp) incProgress(1/15)

  # Add Accepted_genus to the TNRS output, for easier management later
  g <- sapply(strsplit(tnrs$Accepted_species, split = " "), function(x) x[1])
  g[is.na(g)] <- "" # consistency with TNRS
  tnrs$Accepted_genus <- g

  # Log of botanical corrections + merge back into main dataset ----
  # Put together M + authority, ... + output of TNRS
  # Outside the function we may need to substitute with the user's column names
  colnames(tnrs) <- paste0(colnames(tnrs), "_returned_by_TNRS")

  colnames(M) <- paste0(colnames(M), "_processed_by_app")
  M <- cbind(M, tnrs)
  where.in.M <- match(original.name.ids, rownames(M))
  M <- M[where.in.M,] # same number of rows as Data

  extra.vars1 <- c("IdTree", "Site")
  extra.vars2 <- intersect(names(Data), c("IdStem", "Voucher", "IdLevel", "Authority", "VernName"))
  V <- c(extra.vars1, Vars, extra.vars2)
  M0 <- as.matrix(Data[, ..V]) # original

  LOG <- data.frame(M0, M) # a complete record of the original names, what we did to them, and what TNRS thinks
  setDT(LOG)
  LOG <- LOG[, lapply(.SD, function(x) replace(x, which(x==""), NA))]


  if(ThisIsShinyApp) incProgress(1/15)

  # We do not need to keep everything, just the stuff required for taxonomic homogeneization
  # Note: Unmatched_terms are needed to differentiate morphospecies.
  keep <- c("Accepted_family", "Accepted_genus", "Accepted_species",  "Accepted_name_rank", "Accepted_name")
  keep <- paste0(keep, "_returned_by_TNRS")
  keep <- c("Site", "IdTree", keep)



  # keep the best score for each tree and record issues


  if(ThisIsShinyApp) incProgress(1/15)

  # record issues
  problems <- names(which(table(unique(LOG[, ..keep])[, paste(Site, IdTree)]) > 1))
  Data[paste(Site, IdTree) %in% problems,  Comment_DataHarmonization := GenerateComment(Comment_DataHarmonization, "Incongruent taxonomic information within Site x IdTree combinations.")]

  # get the best score
  bestScores <- LOG[LOG[, .I[which.max(Overall_score_returned_by_TNRS)],by = .(Site, IdTree)]$V1, ]
  bestScores <- bestScores[, ..keep]
  names(bestScores) <- toUpperFirst(gsub("Accepted_", "", names(bestScores)))
  names(bestScores) <- gsub("Name_rank_returned_by_TNRS", "Determination_rank_DataHarmonizationCor", names(bestScores))
  names(bestScores) <- gsub("_returned_by_TNRS", "_DataHarmonizationCor", names(bestScores))

  Data <- merge(Data, bestScores, by = c("Site", "IdTree"), all.x = T) # Merge back into the dataset


  if(ThisIsShinyApp) incProgress(1/15)

  # Species is technically ScientificName_DataHarmonizationCor
  Data[, ScientificName_DataHarmonizationCor := Species_DataHarmonizationCor]

  # make sure species only hold de one-word species name
  Data[, Species_DataHarmonizationCor :=   tstrsplit(Species_DataHarmonizationCor, " ", fixed = TRUE, keep  = c(2))]




  if(ThisIsShinyApp) incProgress(1/15)

  # Return ----
  return(list(Data = Data, log = LOG))
}
