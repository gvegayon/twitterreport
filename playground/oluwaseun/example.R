################################################################################
# Example created for Oluwaseun
# date: Dec 25 2015
# auth: George G. Vega Yon
################################################################################
rm(list=ls())

library(twitterreport)
library(openxlsx)

tweets <- openxlsx::read.xlsx("playground/oluwaseun/Sample_GNIP_data.xlsx")

# Getting the first name
usernames <- sapply(tweets$screen.name, strsplit, split=" ")
usernames <- sapply(usernames, "[", 1)

# Assigning gender
tw_gender(usernames)

# Common analysis -----------------------------------------------
# Extracting hashtags (and others)
components <- tw_extract(tweets$text)

# Most popular users
tw_table(components, "mention")

# Most popular hashtags
tw_table(components, "hashtag")

# Analyzing text ---------------------------------------
words <- tw_words(tweets$text)

# Word cloud
plot(words)

# Jaccard index (word similarity)
jaccard <- jaccard_coef(words)
jaccard

# what words are most related to "support"
words_closeness("support",jaccard)

# Sentinment analysis
tweets$sentiments <- tw_sentiment(tweets$text)
hist(tweets$sentiments)

# Mapping tweets ---------------------------------------------------------------
tw_leaflet(tweets, lng = "geo-tag1", lat = "geo-tag2", radii=~sqrt(n)*10000)

# Networking ----------------------------------
network <- tw_network(tweets$screen.name, components$mention)
plot(network)

