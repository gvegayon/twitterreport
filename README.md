twitterreport
=============

Out-of-the-box analysis and reporting tools for twitter

About
-----

While there are some (very neat) R packages focused on twitter (namely `twitteR` and `stramR`), `twitterreport` is centered on providing analysis and reporting tools for twitter data. The package's current version features:

-   Access to twitter API
-   Extracting mentions/hashtags/urls from text (tweets)
-   Gender tagging by matching user names with gender datasets included in the package (**es** and **en**)
-   Creating (mentions) networks and visualizing them using D3js
-   Sentiment analysis (basic, but useful) using lexicons included in the package (again, **es** and **en**)
-   Creating time series charts of hashtags/users/etc. and visualizing them using D3js
-   Create wordclouds (after removing stop words and processing the text)

You can take a look at a live example at <http://www.its.caltech.edu/~gvegayon/twitter/report_example.html>, and at the source code of that example at <https://github.com/gvegayon/twitterreport/blob/master/vignettes/report_example.Rmd>

Some of the functions here were firstly developed in the project *nodoschile.cl* (no longer running). You can visit the project's testimonial website <http://nodos.modularity.cl> and the website (part of nodoschile) that motivated `twitterreports` at <http://modularity.cl/presidenciales>.

Installation
------------

While the package is still in development, you can always use `devtools` to install the most recent version.

``` r
devtools::install_git('gvegayon/twitterreport')
```

Examples
--------

### Getting tweets from a set of users

``` r
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

``` r
# First, we need to extract the mentions from the message
contents <- tw_extract(tweets$text)

# Building the network and visualizing it as a D3js graph!
graph <- tw_network(tweets$screen_name, contents$mentions, minInteract=3)
plot(graph)
```

In the following examples we will use data on US senators extracted from twitter using the REST API (you can find it in the package)

### Creating a wordcloud

The function `tw_words` takes a character vector (of tweets for example) and extracts all the stopwords+symbols. And the `plot` method for its output creates a wordcloud

``` r
data(senate_tweets)
tab <- tw_words(senate_tweets$text)

# What did it do?
senate_tweets$text[1:2];tab[1:2]
```

    ## [1] "“I am saddened by the news that four Marines lost their lives today in the service of our country.” #Chattanooga"         
    ## [2] ".@SenAlexander statement on today’s “tragic and senseless” murder of four Marines in #Chattanooga: http://t.co/H9zWdJPbiE"

    ## [[1]]
    ##  [1] "saddened"    "news"        "four"        "marines"     "lost"       
    ##  [6] "lives"       "today"       "service"     "country"     "chattanooga"
    ## 
    ## [[2]]
    ## [1] "senalexander" "statement"    "todays"       "tragic"      
    ## [5] "senseless"    "murder"       "four"         "marines"     
    ## [9] "chattanooga"

``` r
# Plot
plot(tab, max.n.words = 40)
```

![](README_files/figure-markdown_github/wordcloud-1.png?raw=true)

### Identifying individuals gender

Using english and spanish names, the `tw_gender` function matches the character argument (which can be a vector) with either a male or female name (or unidentified).

``` r
data(senators_profile)

# Getting the names
sen <- tolower(senators_profile$tw_name)
sen <- gsub('\\bsen(ator|\\.)\\s+','',sen)
sen <- gsub('\\s+.+','',sen)

tab <- table(tw_gender(sen))
barplot(tab)
```

![](README_files/figure-markdown_github/gender-1.png?raw=true)

Sentiment analysis
------------------

Here we have an example clasifying senate tweets on the \#irandeal.

``` r
irandeal <- subset(senate_tweets, grepl('irandeal',text, ignore.case = TRUE))
irandeal$sentiment <- tw_sentiment(irandeal$text, normalize = TRUE)

hist(irandeal$sentiment, col = 'lightblue', 
     xlab ='Valence (strength of sentiment)')
```

<img src="README_files/figure-markdown_github/Sentiments-1.png?raw=true" title="" alt="" width="350px" height="350px" />

Author
------

George G. Vega Yon

g vegayon at caltech
