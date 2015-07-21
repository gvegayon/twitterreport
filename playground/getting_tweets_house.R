rm(list=ls())

source("R/text_mining.R")
source("R/verify.R")
load("data/congress_info.RData")

# Getting tweets from the senators
tmp <- sapply(twitter_accounts_house, "[",1)
n <- length(tmp)
congress_tweets <- vector("list",n)
for (s in 1:n) {
  # Getting the info
  congress_tweets[[s]] <- tw_api_get_timeline(tmp[s],count=2000)
  if (!(s %% 10)) save.image("data/house_tweets_example.RData")
  message(sprintf("%03d of %03d",s,n)," Representative ",representatives$Name[s]," done...")
}

congress_tweets <- bind_rows(congress_tweets)
class(congress_tweets) <- 'data.frame'
save(congress_tweets,file="data/house_tweets_example.RData")
