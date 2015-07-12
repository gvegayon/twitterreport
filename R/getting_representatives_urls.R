rm(list=ls())

library(RCurl)
library(XML)
library(stringr)

################################################################################
# Data base from house.gov
################################################################################

# Getting the table
t1 <- readHTMLTable("http://www.house.gov/representatives/",stringsAsFactors=FALSE)
representatives <- do.call(rbind, t1)
row.names(representatives) <- 1:nrow(representatives)

# Getting the info (URLS)
house <- getURLContent("http://www.house.gov/representatives/")

urls <- sapply(representatives$Name, function(x,...) {
  str_extract(house,paste0('https?://[a-zA-Z]+\\.house\\.gov(?=/?">\\s*',x,")"))}
  )

representatives <- cbind(representatives,website=urls)

# Looking for state names
states <- str_extract_all(house,"(?<=state\\_[a-z]{2}.{2})([a-zA-Z]{2,})")[[1]]

# Retrieving info no districts

################################################################################
# Getting twitter accounts
################################################################################
# Function to enter the website and get the twitter account
tw_get_tw_account <- function(uri) {
   web <- getURL(uri)
   try(str_extract_all(web,'https?://(www\\.)?twitter.com/(#!/)?[a-zA-Z0-9_]+(?=">?)'))
}

# Loop
twitterAccounts <- vector("list",nrow(representatives))
n <- length(twitterAccounts)
for (i in 1:n) {
  twitterAccounts[[i]] <- tw_get_tw_account(representatives$website[i])
  if (!(i %% 10)) save.image("data/congress_info.RData")
  message(sprintf("%03d of %03d",i,n)," Congressman ",representatives$Name[i]," done...")
}
