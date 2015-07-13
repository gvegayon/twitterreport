rm(list=ls())

library(RCurl)
library(XML)
library(stringr)

################################################################################
# Data base from house.gov
################################################################################
# Key to Room Codes
# CHOB: Cannon House Office Building
# LHOB: Longworth House Office Building
# RHOB: Rayburn House Office Building

# Getting the table
senate <- readHTMLTable(
  "http://www.senate.gov/senators/contact/",stringsAsFactors=FALSE,
  header = FALSE)
senate <- senate[[3]]

tmp <- vector("list",100)
for (i in 1:100) {
  tmp[[i]] <- senate[((i-1)*5+1):((i)*5),]
}
tmp <- lapply(tmp, function(x) {
  data.frame(Name=x[1,1], Addr=x[2,1] ,phone=x[3,1],website=x[4,1],class=x[1,2],
             stringsAsFactors=FALSE)
})
tmp <- do.call(rbind,tmp)
  
senate <- do.call(rbind, senate)
