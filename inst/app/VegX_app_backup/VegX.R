# ---- get the VegX data structure ----#

# clear environment ####

rm(list = ls())

# load libraries ####
library(XML)
library(dplyr)
require(data.table)

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



VegX <- lapply(urls, function(url) {
print(url)
  txt <- readLines(url)
  doc = xmlRoot(xmlTreeParse(txt, useInternalNodes = T))

  nodes <- "//xsd:complexType|//xsd:complexType//xsd:annotation|//xsd:complexType//xsd:sequence//xsd:element|//xsd:complexType//xsd:attribute|//xsd:complexType//xsd:element|//xsd:complexType//xsd:sequence//xsd:element//xsd:complexType"
  nodes <- paste0(gsub("complexType", "simpleType",nodes),nodes,  collapse = "|")
  els <- getNodeSet(doc, nodes)

  # els <- getNodeSet(doc, "//xsd:complexType|//xsd:annotation|//xsd:sequence|//xsd:element|xsd:attribute")
  a <- sapply(els, function(x) c(xmlAttrs(x), value = xmlValue(x)))

  a <- do.call(bind_rows, a)
  a <- a[!is.na(a$name),]
  a$element1 <- a$name
  a <- a[,c("element1","name", "value",  "type", "minOccurs","maxOccurs", "use", "default")]
  a$value[a$value %in%""] <- NA

  for(i in 1:nrow(a)) {
    idx <- rev(grep(a$value[i], a$value, fixed = T))[2]
    if(!is.na(idx)) a$element1[i] <- a$name[idx]
    if(is.na(idx) & is.na(a$value[i])) a$element1[i] <- a$element1[i-1]# a$element1[i] <- a$name[(which(!is.na(a$value[1:i])))[1]]
  }


  a <- data.table(a)
  a <- split(a, a$element1, sorted = F)
  a <- sapply(a, function(x) split(x, by = "name", sorted = F))


  for(i in length(a):1) {
    x <- a[[i]]
    if(any(sapply(x, nrow) > 1)) stop("There is more nesting")
    if(tail(names(x),1) %in% names(a)) {
      idx <- which(names(a) ==tail(names(x),1))
      if(idx < i ) stop("I have not coded for this case")
      x[tail(names(x), 1)] <-  a[idx]
      a[idx] <- NULL
      }
    a[[i]] <- x
  }

  return(a)

})

View(VegX[[1]])






  sapply(els,xmlValue)
  a <-
    data.frame(element = sapply(els, function(el) xmlGetAttr(el, "name", default = NA)),
               minOccurs = sapply(els, function(el) xmlGetAttr(el, "minOccurs", default = NA)),

               maxOccurs = sapply(els, function(el) xmlGetAttr(el, "maxOccurs", default = NA)),
               type = sapply(els, function(el) xmlGetAttr(el, "type", default = NA)) #,
               # source = sapply(els, function(el) xmlGetAttr(el, "source", default = NA)),
               # default = sapply(els, function(el) xmlGetAttr(el, "default", default = NA)),
               # use = sapply(els, function(el) xmlGetAttr(el, "use", default = NA))
               )

  b <- cbind(table = gsub("https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/|.xsd", "", url),
             element1 = a$element,
             a, xmlValue(els)) #xmlToDataFrame(doc, nodes = els, homogeneous = F, collectNames = T))

  for(i in 1:nrow(b)) {
    idx <- rev(grep(b$annotation[i], b$complexType, fixed = T))[1]
    if(!is.na(idx)) b$element1[i] <- b$element[idx]
  }

  return(b)
})

VegX <- do.call(rbind, VegX)

VegX$complexType <- NULL
names(VegX) <- gsub("annotation" , "description", names(VegX)  )

table(VegX$table)
View(VegX)


# save
write.csv(VegX, paste0("inst/app/data/VegX.csv"), row.names = FALSE)

