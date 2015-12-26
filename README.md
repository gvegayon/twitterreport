

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
-   Map visualization using the leaflet package
-   Topics identification through the Jaccard coeff (words similarity)

You can take a look at a live example at <http://www.its.caltech.edu/~gvegayon/twitter/report_example.html>, and at the source code of that example at <https://github.com/gvegayon/twitterreport/blob/master/vignettes/report_example.Rmd>

Some of the functions here were firstly developed in the project *nodoschile.cl* (no longer running). You can visit the project's testimonial website <http://nodos.modularity.cl> and the website (part of nodoschile) that motivated `twitterreports` at <http://modularity.cl/presidenciales>.

Installation
------------

While the package is still in development, you can always use `devtools` to install the most recent version.

``` r
devtools::install_github('gvegayon/twitterreport')
```

Please have in mind that the package is still in an early stage (so there it might be a little buggy). Furthermore, function names and classes will change for sure (to more intuitive names of course).

Examples
--------

### Getting tweets from a set of users

    ## Loading required package: Matrix

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
![](README_files/figure-markdown_github/network.png?raw=true)

(This is just for ilustration, the real output is a D3js interactive)


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

![](README_files/figure-markdown_github/Sentiments-1.png?raw=true)

A map using leaflet
-------------------

The function `tw_leaflet` provides a nice wrapper for the function `leaflet` of
the package of the same name. Using D3js, we can visualize the number of tweets grouped up geographically as the following example shows:

``` r
tw_leaflet(senate_tweets,~coordinates, nclusters=3)
```

![](README_files/figure-markdown_github/leaflet_map.png?raw=true)

(This is just for ilustration, the real output is a D3js interactive)

Note that in this case there are 14 tweets with the `coordinates` column non-empty, leading to 4 different senators that have such information. Using the `nclusters` option, the `tw_leaflet` groups the data using the `hclust` function of the stats package. So the user doesn't need to worry about aggregating data.

Words closeness
---------------

An interesting issue to review is how are words related to each other. Using the Jaccard coefficient we are able to estimate a measure of distance between two words. The `jaccard_coef` function implements such algorithm, and it allows us to get a better understanding of topics, as the following example

``` r
# Computing the jaccard coefficient
jaccard <- jaccard_coef(senate_tweets$text,max.size = 1000)

# See what words are related with abortion
words_closeness('veterans',jaccard,.025)
```

    ##        word         coef
    ## 1  veterans 318.00000000
    ## 2        va   0.08982036
    ## 3      care   0.08510638
    ## 4     honor   0.04389313
    ## 5    access   0.04201681
    ## 6   deserve   0.04176334
    ## 7    health   0.04022989
    ## 8  benefits   0.03827751
    ## 9    mental   0.03733333
    ## 10  honored   0.03505155
    ## 11     home   0.03440860
    ## 12  service   0.03266788
    ## 13     july   0.03108808
    ## 14   combat   0.02964960
    ## 15 services   0.02857143
    ## 16   choice   0.02549575
    ## 17    thank   0.02529960

We can also do this using the output from `tw_extract`, this is, by passing a list of character vectors (this is much fasters)

``` r
hashtags <- tw_extract(senate_tweets$text, obj = 'hashtag')$hashtag

# Again, but using a list
jaccard <- jaccard_coef(hashtags,max.size = 15000)
jaccard
```

    ## Jaccard index Matrix (Sparse) of 3283x3283 elements
    ## Contains the following words (access via $freq):
    ##          wrd   n
    ## 1   irandeal 202
    ## 2       iran 179
    ## 3     scotus 141
    ## 4        tpa 132
    ## 5      netde 119
    ## 6 mepolitics 117

``` r
# See what words are related with abortion
words_closeness('veterans',jaccard,.025)
```

    ##          word        coef
    ## 1    veterans 78.00000000
    ## 2 honorflight  0.06382979
    ## 3          va  0.05154639
    ## 4  miasalutes  0.05000000
    ## 5     4profit  0.04166667
    ## 6   choiceact  0.03658537
    ## 7 40mileissue  0.02564103
    ## 8        hepc  0.02531646

Author
------

George G. Vega Yon

g vegayon at caltech
