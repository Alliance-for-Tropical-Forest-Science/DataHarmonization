# ---- get the VegX data structure ----#

# clear environment ####

rm(list = ls())

# load libraries ####
library(XML)
library(rlist) # list.flatten etc.
library(data.tree) # for tree structures

# Load VegX xml files to get data structure and Description
## VegX: an exchange standard for plot-based vegetation data
## Source of information:
## https://iavs-org.github.io/VegX/
## https://github.com/iavs-org/VegX/tree/master/vegxschema

# Links to the raw files .xsd files.
urls <- c(
  comm = "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-community.xsd",
  misc = "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-misc.xsd",
  org =  "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-organism.xsd",
  plot = "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-plot.xsd",
  obs =  "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-plotobservation.xsd"
  # user = "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-userdefined.xsd",
  #veg =  "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg.xsd" # the super-container
)

pattern = " name=\""
tables <- gsub("https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/|.xsd", "", urls)


tree <- Node$new("VegX")
for(i in 1:length(urls))  {
  x <-tree$AddChild(tables[i])

  # get the XML
  url = urls[i]
  txt <- readLines(url)

  # find named items
  w <- grep(pattern, txt)
  s <- strsplit(txt[w], split = pattern)

  u <- sapply(s, function(x) x[1])
  depth <- sapply(gregexpr(" ", u), length) # 'depth' is a bad name. This is the indentation, number of blank spaces

  terms <- sapply(s, function(x) {
    stop = gregexpr("\"", x[2])[[1]][1] # the name of the VegX term ends here
    substr(x[2], start = 1, stop = stop - 1)
  })

  df <- data.frame(from = tables[i], to = terms, depth)

  # create the network by over-writing the "from" node
  # (this looks for the immediately precedent term closer to the root,
  # which can be interpreted as the parent node to any given node)
  for(j in 2:nrow(df))
  {
    sub <- df[1:j,]
    sub <- sub[sub$depth < sub$depth[j],]
    if(nrow(sub) > 0) df[j, "from"] <- sub[nrow(sub), "to"]
  }

  # Remove certain nodes:
  remove.terminal <- c("id", "system", "scope") # id, system, scope: I do not know what they mean
  w <- which(df$to %in% remove.terminal)
  if(length(w) > 0)
  {
    df <- df[-w,]
  }

  # Split into subtrees:
  breaks <- which(df$from == tables[i])
  subtrees <- lapply(breaks, function(k) {
    if(k == max(breaks)) k2 = nrow(df)
    if(k <  max(breaks)) k2 = breaks[which(breaks == k) + 1] - 1
    if(k == k2) sub <- data.frame(from = df$to[k], to = "NULL") # some hack
    if(k != k2) sub <- df[(k+1):k2, c("from", "to")]

    x$AddChildNode(data.tree::FromDataFrameNetwork(sub))

  })

}


# ToListExplicit(tree)
# str(ToListSimple(tree))
