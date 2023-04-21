# ---- get the VegX data structure ----#

# clear environment ####

rm(list = ls())

# load libraries ####
library(XML)
library(rlist) # list.flatten etc.


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
### RECURSIVE FUNCTIONS
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
                   'annotation' = xmlValue(node), # contains the annotation
                   'xsd:' = xmlName(node))) # the type of entity within xsd nomenclature (just four forms)
  return(basic)
}

recursive_parsing <- function(node) {
  if(!is_complex(node)) return(process_simple_nodes(node))
  if(is_complex(node))
  {
    basic <- rbind(c(xmlAttrs(node), # contains the attributes declared along the name
                     'annotation' = xmlValue(node), # contains the annotation
                     'xsd:' = xmlName(node))) # the type of entity within xsd nomenclature (just four forms)
    
    out <- list(basic, inside = lapply(xmlChildren(node), recursive_parsing))
    return(out)
  }
}

###########################################################
### EXTRACT THE INFORMATION FROM VEGX XML FILES
###########################################################

# Loop of loops across the different .xsd files that describe the VegX standard
tables <- gsub("https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/|.xsd", "", urls)

tree.of.names <- vector("list", length(tables))
names(tree.of.names) <- tables

info <- vector("list", length(tables))
names(info) <- tables

for(i in 1:length(urls))
{
  # get the XML
  url = urls[i]
  txt <- readLines(url)
  doc = xmlParse(txt)
  
  # identify all nodes
  # (first order only, to avoid duplication during recursion)
  target <- c("element", "attribute", "simpleType", "complexType")
  nodes <- xmlChildren(xmlChildren(doc)[[1]]) # jump over the xsd:schema root node
  nodes <- nodes[sapply(nodes, xmlName) %in% target]
  
  # store the hierarchical structure of names
  tree.of.names[i] <- list(lapply(nodes, parse_names))
  
  # get attributes, etc. (extended information)
  info[i] <- list(lapply(nodes, recursive_parsing))
}


# Have a look at the tree of names:
str(tree.of.names[[1]]) # empty stuff... something to think about ***
str(tree.of.names)

length(unique(unlist(tree.of.names))) # unique terms in VegX standard?

# The extended information can be in a table form:
info <- rlist::list.flatten(info)
length(info)

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


###########################################################
### SAVE OUTPUT
###########################################################

if(FALSE)
{
  # adapt to the app dev directory structure
  saveRDS(tree.of.names, paste0("inst/app/data/VegXtree.rds"), row.names = FALSE)
  write.csv(info3, paste0("inst/app/data/VegXinfo.csv"), row.names = FALSE)
  
}

