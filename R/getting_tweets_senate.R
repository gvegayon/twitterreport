rm(list=ls())

source("R/text_mining.R")
source("R/verify.R")
load("data/senate_info.RData")

# Getting tweets from the senators
tmp <- sapply(twitter_accounts_senate, "[",1)
n <- length(tmp)
senate_tweets <- vector("list",n)
for (s in 1:n) {
  # Getting the info
  senate_tweets[[s]] <- tw_api_get_timeline(tmp[s],200)
  if (!(s %% 10)) save.image("data/senate_tweets_example.RData")
  message(sprintf("%03d of %03d",s,n)," Senator ",senators$Name[s]," done...")
}

senate_tweets <- bind_rows(senate_tweets)
class(senate_tweets) <- 'data.frame'
save(senate_tweets,file="data/senate_tweets_example.RData")

tweets_components <- tw_extract(senate_tweets$text)
groups <- data.frame(
  name=senators_profile$tw_screen_name,group=as.numeric(factor(senators$party)),
  stringsAsFactors = FALSE)
senate_network <- tw_conversation(
  tolower(senate_tweets$screen_name),
  lapply(lapply(tweets_components,"[[","mention"),unique),onlyFrom = TRUE,
  group=groups, minInteract = 3)

writeLines(tw_write_json_network(senate_network),'data/us_congress.json')
