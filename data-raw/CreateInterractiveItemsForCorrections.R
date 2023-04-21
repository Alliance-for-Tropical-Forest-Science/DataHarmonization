library(DataHarmonization)

# get the Rd from the package
db <- tools::Rd_db("DataHarmonization")

# extract the desctiptions of the arguments

outfile <- tempfile()
Fct_args <- NULL


# functio order:
# 1. R/GeneralErrorsDetection.R
# 2. R/BotanicalCorrection.R
# 3. R/StatusCorrection.R
# 4. R/DiameterCorrection.R
# 5. R/RecruitmentCorrection.R

for(rd in c(
  # "GeneralErrorsDetection.Rd",
            "BotanicalCorrection.Rd",
            "StatusCorrection.Rd",
            "DiameterCorrection.Rd"
            # "RecruitmentCorrection.Rd"
            )) {

  tools::Rd2HTML(db[[rd]], outfile)

  Label <- as.character(rvest::html_nodes(xml2::read_html(outfile ), "tr")[-1])

  Fct_args <- rbind(Fct_args, data.frame( Function = gsub("\\.Rd", "", rd),  Label = as.character(rvest::html_nodes(xml2::read_html(outfile ), "tr")[-1])
  ))

}

Fct_args <- Fct_args[!grepl(">Data<",Fct_args$Label), ] # remove Data



# create ItemID
Fct_args$ItemID <- gsub("<code>|</code>", "", regmatches( Fct_args$Label, regexpr("<code>.*?</code>", Fct_args$Label)))


# add the default values
Fct_args$Default <- ""

for(f in unique(Fct_args$Function)) {
  form <- formals(f)
  idx = Fct_args$Function %in% f
  m <- match(Fct_args$ItemID[idx], names(form) )
  Fct_args$Default[idx] <- form[m]
}


# make ItemID unique
Fct_args$ItemID <-paste0(Fct_args$Function, Fct_args$ItemID)

## add more info

df_moreInfo <-
  data.frame(class = c("numeric numeric", "integer integer", "logical logical", "character call","character character", "NULL NULL", "numeric call", "function call"),
             ItemType = c("numericInput", "numericInput","pickerInput", "pickerInput", "textInput", "TBD", "numericInput", "pickerInput"),
             Multiple = c(F, F, F, T, F, "TBD", F, F),
             Options = c(F, F, "list(`live-search` = TRUE)",  "list( `actions-box` = TRUE)", F, "TBD", F, "list( `actions-box` = TRUE)"),
             Argument = c("value", "value", "choices", "choices", "value", "TBD", "value", "choices"),
             Argument2 = c(F, F, "selected", "selected", F, "TBD", F, "selected"),
             argValue = c("OtherOptions", "OtherOptions", "LogicalOptions", "TBD", "OtherOptions", "TBD", "OtherOptions", "Function")) # TBD because some need to be column names, others need to be list of options given by default.


Fct_args <- cbind(Fct_args, df_moreInfo[match(sapply(Fct_args$Default, function(x) paste(class(eval(x)), class(x))), df_moreInfo$class), -1], ReactiveArgValue = T) # by default is a reactive in the app, but when list of option defined in the function, it will be changed to FALSE


## change multiple to FALSE if "1 value" is in description
Fct_args$Multiple[grepl('1 value', Fct_args$Label)] <- FALSE

## get the list of columns
x <- read.csv("inst/app/data/interactive_items.csv")


## fill out the TBD


### ItemType

#### first create a lookup table
ItemType_lookup <- list(
  Pioneers = list(
    ItemType = "pickerInput",
    Multiple = T,
    Options = "list( `actions-box` = TRUE)",
    Argument = "choices",
    Argument2 = "selected",
    argValue = "FormatedScientificNameOptions",
    ReactiveArgValue = TRUE
  ),
  # Sources = list(
  #   ItemType = "pickerInput",
  #   Multiple = T,
  #   Options = "list( `actions-box` = TRUE)",
  #   Argument = "choices",
  #   Argument2 = "selected",
  #   argValue = "BotanicalSourceOptions",
  #   ReactiveArgValue = FALSE
  # ),
  # WFOData = list(
  #   ItemType = "fileInput",
  #   Multiple = F,
  #   Options = F,
  #   Argument = "accept",
  #   Argument2 = F,
  #   argValue = NA,
  #   ReactiveArgValue = FALSE
  # ),
  # OtherCrit = list(
  #   ItemType = "pickerInput",
  #   Multiple = T,
  #   Options = "list( `actions-box` = TRUE)",
  #   Argument = "choices",
  #   Argument2 = "selected",
  #   argValue = "ColumnOptions",
  #   ReactiveArgValue = TRUE
  # ),
  MinDBH = list(
    ItemType = "numericInput",
    Multiple = FALSE,
    Options = FALSE,
    Argument = "value",
    Argument2 = FALSE,
    argValue = "NULL",
    ReactiveArgValue = FALSE
  )
)

#### the fill out

for(i in which(Fct_args$ItemType %in% "TBD")) {

  idx <- which(names(ItemType_lookup) == gsub("<code>|</code>", "", regmatches(Fct_args$Label[i], regexpr("<code>.*?</code>",  Fct_args$Label[i]))))

  if(length(idx) == 1) {
    Fct_args$ItemType[i] <- ItemType_lookup[[idx]]$ItemType
    Fct_args$Multiple[i] <- ItemType_lookup[[idx]]$Multiple
    Fct_args$Options[i] <- ItemType_lookup[[idx]]$Options
    Fct_args$Argument[i] <- ItemType_lookup[[idx]]$Argument
    Fct_args$Argument2[i] <- ItemType_lookup[[idx]]$Argument2
    Fct_args$argValue[i] <- ItemType_lookup[[idx]]$argValue
    Fct_args$ReactiveArgValue[i] <- ItemType_lookup[[idx]]$ReactiveArgValue

  } else { stop("Need to code for this argument")}

  # if(grep(paste(names(ItemType_lookup), collapse = "|"), Fct_args$Label[i])) {
  #   Fct_args$ItemType[i] <- "pickerInput"
  #   Fct_args$Multiple[i] <- T
  #   Fct_args$Options[i] <- "list( `actions-box` = TRUE)"
  #   Fct_args$Argument[i] <- "choices"
  #   Fct_args$Argument2[i] <- "selected"
  #   Fct_args$argValue[i] <- "FormatedScientificNameOptions"
  #   Fct_args$ReactiveArgValue[i] <- TRUE
  #
  # } else { stop("Need to code for this argument")}

}


### argValue

for(i in which(Fct_args$argValue %in% "TBD")) {


  if(all(eval( Fct_args$Default[[i]]) %in% x$ItemID[x$Activate == T])) {
    Fct_args$argValue[i] <- "FormatedColumnOptions"
    Fct_args$ReactiveArgValue[i] <- TRUE
  }

  if(!all(eval( Fct_args$Default[[i]]) %in% x$ItemID[x$Activate == T])){
    Fct_args$argValue[i] <- paste(deparse(Fct_args$Default[[i]]), collapse = "")
    Fct_args$ReactiveArgValue[i] <- FALSE
    if( !Fct_args$ItemID[[i]] %in% c("DiameterCorrectionWhatToCorrect", "BotanicalCorrectionSources")) Fct_args$Default[i] <- paste0('c("', eval(Fct_args$Default[[i]])[1], '")')
  }

}

for(i in which(Fct_args$argValue %in% "TBD")) {


  if(all(eval( Fct_args$Default[[i]]) %in% x$ItemID[x$Activate == T]))  Fct_args$argValue[i] <-"FormatedColumnOptions"

  if(!all(eval( Fct_args$Default[[i]]) %in% x$ItemID[x$Activate == T])) Fct_args$argValue[i] <- deparse(Fct_args$Default[[i]])


  Fct_args$ReactiveArgValue[i] <- ifelse(!all(eval( Fct_args$Default[[i]]) %in% x$ItemID[x$Activate == T]), FALSE, TRUE)

  if(! Fct_args$ReactiveArgValue[i])  Fct_args$Default[i] <- paste0('c("', eval(Fct_args$Default[[i]])[1], '")')

}

for(i in which(Fct_args$argValue %in% "Function")) {
  Fct_args$argValue[i] <- paste0('list("Default Function" = "', paste(deparse(Fct_args$Default[[i]]), collapse = ""), '")')
  Fct_args$Default[i] <- paste0('list("Default Function" = "', paste(deparse(Fct_args$Default[[i]]), collapse = ""), '")')

  Fct_args$ReactiveArgValue[i] <- FALSE
}


# consider default as character now

Fct_args$Default <- as.character(Fct_args$Default)

# save
write.csv(data.frame(Fct_args), "inst/app/data/interactive_items_CorrerctionFunctions.csv", row.names = F)

