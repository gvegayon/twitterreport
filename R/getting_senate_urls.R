rm(list=ls())

library(RCurl)
library(XML)
library(stringr)

source("R/text_mining.R")
source("R/verify.R")

################################################################################
# Data base from senate.gov
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
senators <- lapply(tmp, function(x) {
  data.frame(
    Name=str_extract(x[1,1],'^(.+)(?=\\n)'),
    party=str_extract(x[1,1],'(R|D|I)(?= - )'),
    State=str_extract(x[1,1],'(?<=(R|D|I) - )[A-Z]+'),
    Addr=x[2,1] ,phone=x[3,1],
    website=str_extract(x[4,1],'(?<=\\n).*'),
    class=x[1,2],
             stringsAsFactors=FALSE)
})
senators <- do.call(rbind,senators)

# Cleanning website
senators$website <- str_replace(senators$website,"\\s+","")
senators$website <- str_replace(senators$website,"(?<=public/).*","")
senators$website <- str_replace(senators$website,"(?<=\\.gov/).*","")

################################################################################
# Getting twitter accounts
################################################################################
# Loop
twitter_accounts_senate <- vector("list",nrow(senators))
n <- length(twitter_accounts_senate)
for (i in 1:n) {
  twitter_accounts_senate[[i]] <- tw_get_tw_account(senators$website[i])
  if (!(i %% 10)) save.image("data/senate_info.RData")
  message(sprintf("%03d of %03d",i,n)," Senator ",senators$Name[i]," done...")
}
rm(n,i)

# Checking out who I couldn't find (twitter accounts unavailable at the website)
accounts_werent_found_senate <- sapply(twitter_accounts_senate,length)==0
senators$website[which(accounts_werent_found_senate)]

save.image("data/senate_info.RData")
load("data/senate_info.RData")

################################################################################
# Getting account info from API
################################################################################

# Getting the info
tmp <- lapply(twitter_accounts_senate,"[",1)
n <- length(twitter_accounts_senate)
twitter_profiles_senate <- vector("list",n)
for (s in 1:n) {
  # Getting the info
  twitter_profiles_senate[[s]] <- tw_api_get_usr_profile(tmp[[s]])
  if (!(s %% 10)) save.image("data/senate_info.RData")
  message(sprintf("%03d of %03d",s,n)," Senator ",senators$Name[s]," done...")
}
save.image("data/senate_info.RData")
load("data/senate_info.RData")

# Checking those that didn't worked out and filling those with empty rows
# so that we can rbind and merge it witht the congress dataset
error <- sapply(twitter_profiles_senate,length)==0
which(error)

emptyvec <- names(twitter_profiles_senate[which(!error)][[1]])
emptyvec <- sapply(emptyvec, function(x) NA)
for (i in which(error)) twitter_profiles_senate[[i]] <- emptyvec

# Filling the data.frame. The warning is for the 'entitites' list
# not relevant for now
senators_profile <- as.data.frame(do.call(rbind, twitter_profiles_senate),stringsAsFactors = FALSE)
colnames(senators_profile) <- paste0('tw_',colnames(senators_profile))

# Removing unrelevant objects
rm(list=ls(pattern = '^tw_'))
rm(twitter_token)

save.image("data/senate_info.RData")

