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
   try(str_extract_all(web,'https?://(www\\.)?twitter.com/(#!/)?[a-zA-Z0-9_]+(?=">?)'))[[1]]
}

# Loop
twitterAccounts <- vector("list",nrow(representatives))
n <- length(twitterAccounts)
for (i in 1:n) {
  twitterAccounts[[i]] <- tw_get_tw_account(representatives$website[i])
  if (!(i %% 10)) save.image("data/congress_info.RData")
  message(sprintf("%03d of %03d",i,n)," Congressman ",representatives$Name[i]," done...")
}
twitterAccounts <- sapply(twitterAccounts, unique)
save.image("data/congress_info.RData")

# Normalizing the twitter accounts (note that screen names are not case
# sensitive)
twitterAccounts <- lapply(twitterAccounts, str_extract, "[a-zA-Z0-9_]+$")
twitterAccounts <- lapply(twitterAccounts, tolower)
twitterAccounts <- lapply(twitterAccounts, unique)

# Get info from the twitter homepage
source("R/verify.R")
tw_api_get_usr_profile <- function(x,...) {
  if (is.na(x)) return(NULL)
  else
  tryCatch(GET(paste0(
    "https://api.twitter.com/1.1/users/show.json?screen_name=",x),
    config(token=twitter_token)))
}

tmp <- lapply(twitterAccounts,"[",1)
user_info_raw <- lapply(tmp, tw_api_get_usr_profile)

# Filtering
statuses <- sapply(user_info_raw,is.null)
user_info <- user_info_raw[!statuses]
statuses <- sapply(user_info, status_code)
user_info <- user_info[which(statuses==200)]

user_info <- do.call(rbind, lapply(user_info, content))

