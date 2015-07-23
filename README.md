# twitterreport
Out-of-the-box tools for analysis and reporting for twitter

## About

This package is an exercise to understad a little bit more how does the Twitter API
works. Several functions allow to use the REST API through the `httr` package.

While there are some (very neat) R packages focused on twitter (specifically the
package `twitteR` and `stramR`), `twitterreport` is centered in analysis and reporting
tools.

## Installation

While the package is still in development, you can always use `devtools` to install
the most recent version.

```r
devtools::install_git('gvegayon/twitterreport')
```

## Example

### Getting tweets from a set of users

```r
# Firts, load the package!
library(twitterreport)

# List of twitter accounts
users <- c('MarsRovers', 'senatormenendez', 'sciencemagazine')

# Getting the twitts (first gen the token)
key <- tw_gen_token('myapp','key', 'secret')
tweets <- lapply(users, tw_api_get_statuses_user_timeline, twitter_token=key)
 
# Processing the data (and taking a look)
tweets <- do.call(rbind, tweets)
head(tweets)
```

### Creating a network of mentions

```r
# First, we need to extract the mentions from the message
contents <- tw_extract(tweets$text)

# Building the network and visualizing it as a D3js graph!
graph <- tw_network(tweets$screen_name, contents$mentions, minInteract=3)
plot(graph)
```

### Creating a wordcloud

```r
plot(tw_words(tweets$text), max.n.words = 20)
```

## Author
George G. Vega Yon

g vegayon at caltech
