
library(twitterreport)
data(senate_tweets)
mentions <- tw_extract(senate_tweets$text, obj="mention")$mention

# Preparing data for size
usrs<- tolower(senate_tweets$screen_name)
size <- data.frame(name=unique(usrs),
                   size=exp(runif(length(unique(usrs)))*5))

# Creating the graph
graph <- tw_network(
  usrs, mentions, min.interact = 5, size=size)

plot(graph)
