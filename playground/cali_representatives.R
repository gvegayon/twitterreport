rm(list=ls())

library(RCurl)
library(XML)
library(stringr)

load("data/congress_info.RData")

# More parsing on the rep data
district <- sapply(representatives$District, function(x) {
  list(state=str_extract_all(x,'^([a-zA-Z]+)(?=[0-9 ]+)'),
       number=str_extract_all(x,'^(?<=[a-zA-Z ]+) [0-9]+[a-z]{2}'))
})
