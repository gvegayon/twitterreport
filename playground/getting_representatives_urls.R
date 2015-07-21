rm(list=ls())

library(RCurl)
library(XML)
library(stringr)

source("R/text_mining.R")
source("R/verify.R")

################################################################################
# Data base from house.gov
################################################################################
# Key to Room Codes
# CHOB: Cannon House Office Building
# LHOB: Longworth House Office Building
# RHOB: Rayburn House Office Building

# Getting the table
representatives <- readHTMLTable("http://www.house.gov/representatives/",stringsAsFactors=FALSE)
representatives <- do.call(rbind, representatives)

# Getting the info (URLS)
house <- getURLContent("http://www.house.gov/representatives/")

representatives <- cbind(
  representatives,
  website=sapply(representatives$Name, function(x,...) {
    str_extract(house,paste0('https?://[a-zA-Z]+\\.house\\.gov(?=/?">\\s*',x,")"))}
  ))

# Looking for state names
states <- str_extract_all(house,"(?<=state\\_[a-z]{2}.{2})([a-zA-Z ]{2,})")[[1]]
tmp <- str_detect(representatives$District,paste0(states,collapse="|"))
representatives <- representatives[tmp,]
representatives$state <- str_match(representatives$District,paste0(states,collapse="|"))
representatives$DistrictNum <- as.numeric(str_match(representatives$District,'[0-9]+(?=[a-z]{2} District)'))
rm(tmp)

row.names(representatives) <- 1:nrow(representatives)

################################################################################
# Getting twitter accounts
################################################################################
# Loop
twitter_accounts_house <- vector("list",nrow(representatives))
n <- length(twitter_accounts_house)
for (i in 1:n) {
  twitter_accounts_house[[i]] <- tw_get_tw_account(representatives$website[i])
  if (!(i %% 10)) save.image("data/congress_info.RData")
  message(sprintf("%03d of %03d",i,n)," Congressman ",representatives$Name[i]," done...")
}

# Checking out who I couldn't find (twitter accounts unavailable at the website)
accounts_werent_found <- sapply(twitter_accounts_house,length)==0
representatives$website[which(accounts_werent_found)]

save.image("data/congress_info.RData")
load("data/congress_info.RData")

################################################################################
# Getting account info from API
################################################################################

# Getting the info
tmp <- lapply(twitter_accounts_house,"[",1)
n <- length(twitter_accounts_house)
twitter_profiles_congress <- vector("list",n)
for (s in 1:n) {
  # Getting the info
  twitter_profiles_congress[[s]] <- tw_api_get_usr_profile(tmp[[s]])
  if (!(s %% 10)) save.image("data/congress_info.RData")
  message(sprintf("%03d of %03d",s,n)," Congressman ",representatives$Name[s]," done...")
}
save.image("data/congress_info.RData")
load("data/congress_info.RData")
  
# Checking those that didn't worked out and filling those with empty rows
# so that we can rbind and merge it witht the congress dataset
error <- sapply(twitter_profiles_congress,length)==0
which(error)

emptyvec <- names(twitter_profiles_congress[which(!error)][[1]])
emptyvec <- sapply(emptyvec, function(x) NA)
for (i in which(error)) twitter_profiles_congress[[i]] <- emptyvec

# Filling the data.frame. The warning is for the 'entitites' list
# not relevant for now
representatives_profile <- as.data.frame(do.call(rbind, twitter_profiles_congress),stringsAsFactors = FALSE)
colnames(representatives_profile) <- paste0('tw_',colnames(representatives_profile))

# Removing unrelevant objects
rm(list=ls(pattern = '^tw_'))
rm(twitter_token,error,i,n,s,myapp,tmp,emptyvec)

save.image("data/congress_info.RData")
