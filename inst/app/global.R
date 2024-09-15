suppressPackageStartupMessages({
  #list of packages required
  library(shinydashboard)
  library(bslib)
  library(DT)
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(data.tree)
  library(stringr)
  library(stringdist)
  library(data.table)
  library(DataHarmonization)
  library(shinycssloaders)
  library(htmlTable)
  library(markdown)
  library(lubridate)
  library(ggplot2)
  library(ggforce)
  library(ggrepel)
})

# to log warnings and errors

isDebugging <- function() {
  getOption("DataHarmonization.debug", FALSE)
}

if(isDebugging()) {
  shiny::devmode()

  close_sink_and_quit <- function(){
    sink(file = NULL,type = "message")
    sink(file = NULL,type = "output")
    close(con = flog)
    stopApp()
  }

  # options(error = close_sink_and_quit)
  # options(shiny.error = close_sink_and_quit)

  flog <- file("log.txt", open = "wt")
  sink(file = flog, split = TRUE)
  sink(file = flog, type = "message")
}

# read in csv file that has all we want to ask about the headers
xall <- read.csv("data/interactive_items.csv")
xall <- xall[order(xall$Order),]
x <- xall[xall$Activate, ]
x1 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none == "none", ]
x2 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none == "none", ]
x3 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none != "none", ]
x4 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none != "none", ]
x5 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none != "none" & x$if_X2_isnot_none == "none", ]
x6 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none != "none" & x$if_X2_isnot_none != "none", ]

if(!all(unlist(sapply(list(x1, x2, x3, x4, x5, x6), "[[", "ItemID")) %in% x$ItemID)) stop ("not all interactive items are implemented in the app")

secondColumn <- do.call(rbind,list(x2, x3, x4, x5, x6))

if(!all(!is.na(secondColumn$GroupSecondColumn))) stop ("not all items in second columns have a group")

# correction function interactive items

xCorr <- read.csv("data/interactive_items_CorrerctionFunctions.csv")



## in the Codes tab (tree code pre-defined options)
CodeOptions <-  read.csv("data/CodeOptions.csv")
BotanicalSourceOptions <- c("Tropicos" = "tropicos", "United States Department of Agriculture" = "usda", "World Flora Online" = "wfo", "World Checklist of Vascular Plants" = "wcvp")

# function to make unitque IDs (mostly for inactive buttons)

makeUniqueID <- NS(character(0))

# languages <- c("en" = "English", "es" = "Spanish")
#
# flags <- c(
#   "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg",
#   "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/es.svg"
#
# )


# create a helper for help pannels

tabPanel_helper <- function(i) tabPanel(title = gsub("_", " ", i),
                                        includeMarkdown(paste0("www/", i, "Example.md")),
                                        img(src = paste0(i, ".gif"), width = "100%"))


myTableHeader <- function (data, type = c("head", "foot"), escape = TRUE, ...)
{
  names = colnames(data)
  fstRow = data[1,]

  widths = nchar(fstRow, keepNA = F, allowNA=TRUE)
  widths[is.na(widths)] <- 2
  type = match.arg(type)

  f = tags[[sprintf("t%s", type)]]

  f(tags$tr(mapply(
    function(n, w) {
      if (w > 50)
        tags$th(n, style = paste0("min-width:", w * 2, "px;"))
      else
        tags$th(n)
    },
    n = DT:::escapeColNames(names, escape),
    w = unname(widths),
    SIMPLIFY  = F
  )), ...)
}

myTableFooter <- function (names, escape = TRUE)
{
  myTableHeader(names, "foot", escape)
}


# UI javascript script for asking confirmation when browser window is about to close
# call function anywhere in UI
# example
# fluidPage(
#   askBeforeClose()
# )
askBeforeClose <- function() {
  tags$head(tags$script("
    Shiny.connected=true;
    $(function() {
      $(document).on('shiny:disconnected', function(event) {
        Shiny.connected=false;
      })
    });
    window.addEventListener('beforeunload', function (e) {
      if(Shiny.connected) {
        e.preventDefault();
        e.returnValue = 'unload';
      };
    });"
  ))
}

# SERVER auto close application when session ends
# only if run locally (ie not on a remote server)
# call function for example at start of server function
# example
# function(input, output) {
#   autoCloseApp()
# }
autoCloseApp <- function(session=getDefaultReactiveDomain()) {
  # detect local run (from unexported shiny:::inShinyServer)
  isLocal <- Sys.getenv("SHINY_PORT") == ""
  if(isLocal) {
    session$onSessionEnded(function() {
      stopApp()
    })
  }
}
