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
  obs =  "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-plotobservation.xsd",
  # user = "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-userdefined.xsd",
  veg =  "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg.xsd"
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
    if("annotation" %in% names(children))
    {
      annotation <- xmlValue(children$annotation) # contains the annotation
    }
  }
  annotation
}

# A recursive function to create a tree-like structure with VegX names
parse_names <- function(node)
{
  nm = xmlGetAttr(node, "name", default = NULL)
  if(!is_complex(node)) return(nm)
  if(is_complex(node))
  {
    return(setNames(lapply(xmlChildren(node), parse_names), nm))
  }
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

###################################################################
### INFORMATION ABOUT EACH NAMED TERM IN VEGX, INCL. DESCRIPTIONS
###################################################################

# Loop of loops across the different .xsd files that describe the VegX standard
tables <- gsub("https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/|.xsd", "", urls)

info <- vector("list", length(tables))
names(info) <- tables

for(i in 1:length(urls))
{
  # get the content of the XML
  url = urls[i]
  txt <- readLines(url)
  doc <- xmlParse(txt)

  # identify all nodes
  # (first order only, to avoid duplication during recursion)
  target <- c("element", "attribute", "simpleType", "complexType")
  nodes <- xmlChildren(xmlChildren(doc)[[1]]) # jump over the xsd:schema root node
  nodes <- nodes[sapply(nodes, xmlName) %in% target]

  # get attributes, etc. (extended information)
  info[i] <- list(lapply(nodes, recursive_parsing))
}


# Reshape the information as a table

info <- rlist::list.flatten(info)
cn <- unique(unlist(sapply(info, colnames)))
info2 <- matrix(NA, nrow = length(info), ncol = length(cn), dimnames = list(NULL, cn))
for(i in 1:nrow(info2))
{
  info2[i, colnames(info[[i]])] <- info[[i]][1,]
}

info3 <- unique(info2[!is.na(info2[,"name"]),])

dim(info3)
head(info3)


# Most of the terms appear once, but there are repetitions to link tables
# The descriptions vary between those repeated instances

table(table(info3[,"name"]))
info3[info3[,"name"] == "plotObservationID",] # example

repeated <- names(which(table(info3[,"name"]) > 1))
length(repeated) # 39
repeated

i = 18
info3[info3[,"name"] == repeated[i], "annotation"]


###########################################################
### EXTRACT THE HIERARCHY OF NAMED TERMS IN VEGX
###########################################################

# This is a very brute approach, based on indentations and the name=" pattern. Pure string processing.
# It doesn't care about the type of term (simpleType, choice, element, or whatever).
# Apparently it is robust and recovers everything that is on the information table.

# The information is encoded in from-to form, as in igraph.
# The data.tree package goes from edge (pair of nodes) representation into a tree object.

pattern = " name=\""

nets <- list()
for(i in 1:length(urls))
{

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

  # keep
  nets[i] <- list(df)
}



nets <- list()
for(i in 1:length(urls))
{

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

  # keep
  nets[i] <- list(df)
}

# add columns to acutally show the nestedness
V_nets <- lapply(nets, function(net) {

  #remove Id, system, scope
  net <- net[!net$to %in% c("id",
                            "system",
                            "scope"),]

  # get all unique depths
  dephts <- sort(unique(net$depth))

  # create a column for each depth
  net[paste0("level_", dephts)] <- NA


  for(d in rev(dephts)) {
    idx <- net$depth %in% d
    net[idx, paste0("level_", d)] <- net$from[idx]

  }

  for(i in rev(seq(dephts)[-1])) {
    d = dephts[[i]]
    idx <- net$depth %in% d
    net[idx, paste0("level_", d)] <- net$from[idx]

    col <- paste0("level_", dephts[[i]])
    previous_col <- paste0("level_", dephts[[i-1]])

    need_to_fill <- !is.na(net[idx, col]) & is.na(net[idx, previous_col] )

    if(any(need_to_fill)) {
      m <- match( net[idx, col], net$to)
      net[idx, previous_col] <- net$from[m]
    }


  }

  #fill last column with to
  rownames(net) <- seq(nrow(net))
  idx_NA <- apply(net, 1, function(x) which(is.na(x))[1])
  net[cbind(as.numeric(names(idx_NA)), idx_NA)] <- net$to[as.numeric(names(idx_NA))]

  return(net)
})


# Put everything together + the basal branches
net <- do.call(rbind, nets)[,c("from", "to")]

if(FALSE)
{
  # This adds basal branches based on the .xsd files of origin.
  # Not clearly useful.
  roots <- data.frame(from = "VegX", to = tables)
  net <- rbind(roots, net) # this is a network because of the repeated terms (not correct)
}

net$from[net$from %in% tables] <- "VegX" # more general, without basal branches

# Filter the tree, for example excluding certain terminal nodes
remove.terminal <- c("id", "system", "scope") # id, system, scope: I do not know what they mean

w <- which(net$to %in% remove.terminal)
if(length(w) > 0)
{
  net <- net[-w,]
}

# Go from edge representation to tree representation
tree <- data.tree::FromDataFrameNetwork(net) # it ensures tree structure by repeating nodes (correct)
tree

tree2 <- as.data.frame(tree) # to see the whole output and write more easily into a file
tree2

# Is the tree complete?
terms <- unique(unlist(net))
table(terms %in% info3[,"name"])
terms[!terms %in% info3[,"name"]] # few: just the root or the tables

# Is the table complete?
table(info3[,"name"] %in% terms) # mostly complete

###########################################################
### EXPLORE THE TREE
###########################################################

# How many main types do we have?
as.matrix(unique(net[net$from == "VegX", "to"]))

# Decisions to make until arriving to a random terminal node
# This is a metric of how "user friendly" the tree is, from a
# navigation perspective, under the assumption that the user
# knows perfectly the meaning of each term.

avg.path.length.all = mean(sapply(net$to, function(nm) length(FindNode(tree, nm)$path))) # ~number of steps

leaves <- terms[!terms %in% net$from]
avg.path.length.leafs = mean(sapply(leaves, function(nm) length(FindNode(tree, nm)$path))) # ~number of steps

tree$averageBranchingFactor^(avg.path.length.all - 1)   # estimated number of discriminatory decissions (disregard the root
tree$averageBranchingFactor^(avg.path.length.leafs - 1) # the same, but up to leaves

###########################################################
### SAVE OUTPUT
###########################################################

if(FALSE)
{
  path = "/Users/gabriela/Dropbox/PROFESIONAL/PROYECTOS/ITFSA-data-federation/global-standards/"

  # adapt to the app dev directory structure
  saveRDS(tree.of.names, paste0("inst/app/data/VegXtree.rds"), row.names = FALSE)
  write.csv(info3, paste0(path, "VegXinfo.csv"), row.names = FALSE)

  # Store in an external file:
  write.table(tree2, paste0(path, "VegXtree.txt"), row.names = FALSE, quote = FALSE, col.names = FALSE)

}

