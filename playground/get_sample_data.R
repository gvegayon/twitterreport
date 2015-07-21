rm(list=ls())
library(twitteR)
library(stringr)

# Setup the apikey
setup_twitter_oauth(
  "JvBaXf5CWYK8gjIGHOxJBiIXO",
  "mJWb9CzG5cFNVxAuLMBrNUdKcr3vPvp1in4Tcuj9ag38Mn3jtr",
  access_token = "73013091-SVASHEYQ7K0dDaq9kRJKIX9X7BCDPT7ZLEpBptk80",
  access_secret = "kCr3ynhjJ7JkEcNPf89pHtYhtKaliz0lp4InFMr8")

# Get my info
user <- getUser("gvegayon")
friends <- user$getFriends()
timeline <- userTimeline(user,n=400)
tl <- twListToDF(timeline)

# Getting which people do he chats with
msj_mentions <- unlist(str_extract_all(tl$text,"[@][A-Za-z0-9_]+"))
user_mentions <- as.data.frame(table(msj_mentions),responseName="n")
user_mentions <- user_mentions[order(-user_mentions$n),]

user_mentions3 <- subset(user_mentions,subset=n>=3)

# Gay marriage
tweets <- searchTwitter("#LoveWins",until ="2015-06-25",n = 500,since="2015-06-20")
tweets <- twListToDF(tweets)


