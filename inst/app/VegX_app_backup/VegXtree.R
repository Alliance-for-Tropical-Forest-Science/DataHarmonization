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


###########################################################
### EXPLORE THE PROBLEM A BIT
###########################################################

# We have to deal with different types of entities (or elements in a broad sense):
entities <- sort(table(unlist(lapply(urls, function(url) {
  #url = "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-community.xsd" # for development
  pattern = " name=\""
  txt <- readLines(url)
  u <- sapply(strsplit(txt[grep(pattern, txt)], split = pattern), function(x) x[1])
  u <- gsub("<xsd:", "", u)
  u <- gsub(" ", "", u)
  u
}))), decreasing = TRUE)

entities # insight: we have to handle just four types of entities

# How many levels of nestedness do we have?
table(unlist(lapply(urls, function(url) {
  pattern = " name=\""
  txt <- readLines(url)
  u <- sapply(strsplit(txt[grep(pattern, txt)], split = pattern), function(x) x[1])
  sapply(gregexpr(" ", u), length) / 4
}))) # insight: manual parsing not feasible


###########################################################
### FUNCTIONS
###########################################################

# We need recursive functionality "f" based on the following reasoning:
# if input is complexType, apply f to each element in sequence
# else: return f(input)

# Our goal is to create two objects:
# 1- a list of list with just names -- for the dropdown menu
# 2- a large reference table where one can learn more about each named item in the dropdown menu
# Everything is linked by VegX name. Still unclear how to intrpret un-named compound elements

# Function to determine whether one node is compound or not
is_complex <- function(node)
{
  children <- xmlChildren(node)
  n = length(children)

  # Exclude "annotation" from the children
  if("annotation" %in% names(children)) n = n - sum(names(children) == "annotation")

  # Is this a complex or compound type?
  n > 0 | xmlName(node) == "complexType"
}

# Function to extract the "annotation"
get_annotation <- function(node)
{
  annotation = ""
  complex = is_complex(node)
  if(!complex) annotation <- xmlValue(node) # contains the annotation
  if(complex)
  {
    children <- xmlChildren(node)
    if(grepl("annotation", names(children)))
    {
      annotation <- xmlValue(children[[grep("annotation", names(children))[1]]]) # contains the annotation
    }
  }
  annotation
}


# Two functions to get extended information (attributes, etc.)
process_simple_nodes <- function(node)
{
  basic <- rbind(c(xmlAttrs(node), # contains the attributes declared along the name
                   'annotation' = get_annotation(node),
                   'xsd:' = xmlName(node))) # the type of entity within xsd nomenclature (just four forms)
  return(basic)
}

recursive_parsing <- function(node) {
  if(!is_complex(node)) return(process_simple_nodes(node))
  if(is_complex(node))
  {
    basic <- rbind(c(xmlAttrs(node), # contains the attributes declared along the name
                     'annotation' = get_annotation(node),
                     'xsd:' = xmlName(node))) # the type of entity within xsd nomenclature (just four forms)

    out <- list(basic, inside = lapply(xmlChildren(node), recursive_parsing))
    return(out)
  }
}


#######################################################################################
### EXTRACT THE HIERARCHY OF NAMED TERMS IN VEGX + ATTRIBUTES, incl. annotations
#######################################################################################

# Loop of loops across the different .xsd files that describe the VegX standard
tables <- gsub("https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/|.xsd", "", urls)

# This is a very brute approach, based on indentations and the name=" pattern. Pure string processing.
# It doesn't care about the type of term (simpleType, choice, element, or whatever).
# Apparently it is robust and recovers everything that is on the information table.

# The information is encoded in from-to form, as in igraph.
# The data.tree package goes from edge (pair of nodes) representation into a tree object.

pattern = " name=\""

trees <- list()
for(i in 1:length(urls))
{
  # get the text
  url = urls[i]
  txt <- readLines(url) # used for indentations --> tree structure

  # problems with tabs (\t)
  # cases: i = 2, text line = 430
  tabequivalent = "        "
  nchar(tabequivalent) # 8 is standard, see https://rdrr.io/cran/fansi/man/tabs_as_spaces.html
  txt <- gsub("\t", tabequivalent, txt)

  # parse to XML
  doc <- xmlParse(txt)  # used for attribute extraction

  # describe level of indentation of each line (useful later)
  depth.txt <- stringr::str_count(txt, "\\G ")

  # identify XML nodes
  # (first order only, to avoid duplication during recursion)
  target <- c("element", "attribute", "simpleType", "complexType")
  nodes <- xmlChildren(xmlChildren(doc)[[1]]) # jump over the xsd:schema root node
  nodes <- nodes[sapply(nodes, xmlName) %in% target] # the basal branches = types

  # find named items
  w <- grep(pattern, txt)
  s <- strsplit(txt[w], split = pattern)

  u <- sapply(s, function(x) x[1])
  depth <- sapply(gregexpr(" ", u), length) # 'depth' is a bad name. This is the indentation, number of blank spaces

  terms <- sapply(s, function(x) {
    stop = gregexpr("\"", x[2])[[1]][1] # the name of the VegX term ends here
    substr(x[2], start = 1, stop = stop - 1)
  })

  df <- data.frame(from = tables[i], to = terms, depth, start = w)

  # create the network by over-writing the "from" node
  # (this looks for the immediately precedent term closer to the root,
  # which can be interpreted as the parent node to any given node)
  for(j in 2:nrow(df))
  {
    sub <- df[1:j,]
    sub <- sub[sub$depth < sub$depth[j],]
    if(nrow(sub) > 0) df[j, "from"] <- sub[nrow(sub), "to"]
  }

  # Find the lines in the text that describe a given XML node
  # Rule: The node closes at the next line with the same indentation
  # except for the one-line nodes.

  #head(df)
  #as.matrix(sapply(txt[4:26], substr, start = 1, stop = 100, USE.NAMES = FALSE))
  df$end <- length(txt) # default value: the end of the document
  for(j in 1:nrow(df))
  {
    start.j = df$start[j]
    depth.j = df$depth[j]
    after.j <- (start.j+1):length(depth.txt)
    blanks.after.j <- depth.txt[after.j]
    end.j = after.j[min(which(blanks.after.j <= depth.j))]
    df$end[j] <- end.j
  }

  # Update one-line nodes:
  oneliners <- which(df$end == df$start + 1)
  if(length(oneliners) > 0)
  {
    df$end[oneliners] <- df$start[oneliners]
  }

  # Remove certain nodes:
  remove.terminal <- c("id", "system", "scope") # id, system, scope: I do not know what they mean
  w <- which(df$to %in% remove.terminal)
  if(length(w) > 0)
  {
    df <- df[-w,]
  }

  # (Hopefully) if we can build proper XML nodes, we can get the info
  # correctly associated to each node, even if the node
  # name is re-used somewhere else.
  # This uses no memoization and does redundant job but it is fast enough, I think.
  all.pieces.of.info <- apply(df[,c("to", "start", "end")], 1, function(tse) {
    #nrowsinfo <- rep(NA, nrow(df))
    #for(j in 1:nrow(df))
    #{
    #tmp.to = as.character(df[j, "to"])
    #tmp.start = as.numeric(df[j, "start"])
    #tmp.end = as.numeric(df[j, "end"])

    tmp.to = tse[1] # the name ('name' attribute in XML sense) of the terminal node
    tmp.start = tse[2]
    tmp.end = tse[3]

    node = xmlChildren(xmlParse(txt[tmp.start:tmp.end]))[[1]] # it works with some warnings
    info <- recursive_parsing(node)
    if(class(info) == "list")
    {
      info <- rlist::list.flatten(info)
      info <- Reduce(function(...) merge(..., all=T), info)
    }
    info <- info[which(info[,"name"] == tmp.to),, drop = FALSE]
    info
  }
  #table(nrowsinfo)
  )

  all(sapply(all.pieces.of.info, nrow) == 1) # must be TRUE
  length(all.pieces.of.info) == nrow(df) # must be TRUE, by definition

  # the Reduce etc. approach doesn't work well, it breaks the 1-to-1 correspondence,
  # so I use a more conservative approach
  cn <- unique(unlist(lapply(all.pieces.of.info, colnames)))
  block.info <- as.data.frame(matrix(NA, nrow = nrow(df), ncol = length(cn), dimnames = list(NULL, cn)))
  for(j in 1:length(all.pieces.of.info))
  {
    piece <- all.pieces.of.info[[j]]
    block.info[j,colnames(piece)] <- piece
  }

  df <- data.frame(df, block.info)

  # Because of the repetitions of terms, a declarative approach
  # based on node names (from --> to), as in igraph or networks,
  # won't work well. It will connect different parts of the tree
  # with each other. That is why we need to run through basal nodes
  # in a more conservative way.
  # Still, some minor problems remain.

  # Split into subtrees:
  breaks <- which(df$from == tables[i]) # same length as number of basal branches or types
  print(paste0("Must be TRUE: ", length(breaks) == length(nodes)))

  subtrees <- lapply(breaks, function(k) {
    # k = breaks[floor(length(breaks)/2)] # for development
    if(k == max(breaks)) k2 = nrow(df)
    if(k <  max(breaks)) k2 = breaks[which(breaks == k) + 1] - 1
    if(k == k2) sub <- data.frame(from = df$to[k], to = "NULL") # some hack
    if(k != k2) sub <- df[(k+1):k2, ]

    # create subtree with attributes at terminal nodes
    subtree <- data.tree::FromDataFrameNetwork(sub)
    #print(subtree, "annotation") # does it work?
    subtree
  })

  # keep
  trees[i] <- list(subtrees)
}

i == length(urls) # if not TRUE, errors above

trees <- rlist::list.flatten(trees)
sapply(trees, class)


# Merge all trees into a single large tree:
tree <- Node$new("VegX")
for(i in 1:length(trees))
  tree$AddChildNode(trees[[i]])

tree
print(tree, "annotation")
print(tree)


saveRDS(tree, "data/VegXtree.rds")
