#' @title Extract info from tweets
#' @aliases 
#' @description Extract email accounts, mentions, hashtags and urls from tweets
#' @param txt Character
#' @param obj List of objects to extract
#' @param normalize bool whether or not to normalize emails, hashtags and mentions (to lower)
#' @return List
#' @examples  
#' head(tw_extract(tweets$text))
tw_extract <- function(txt, obj = c("email", "mention", "hashtag", "url"),
                       normalize=TRUE) {
  # patterns
  p.email <- "([a-zA-Z0-9_]-?\\.?)+@([a-zA-Z0-9_]-?)+\\.[a-zA-Z]+"
  p.hashtag <- "(?<=#)\\w+"
  p.mention <- "(?<=@)[a-zA-Z0-9_]+"
  p.url <- "https?[:]//[[:graph:]]+"
  
  output <- lapply(obj, function(x) {
    str_extract_all(txt, get(paste0('p.',x)))
  })
  
  names(output) <- obj
  
  if (normalize) {
    output$mention <- lapply(output$mention,tolower)
    output$hashtag <- lapply(output$hashtag,tolower)
    output$email <- lapply(output$email,tolower)
  }
  
  return(output)
}

tw_words <- function(txt, stopw=stopwords('en'), cleanfun=NULL) {
  # Spleatting and cleaning
  txt <- str_split(txt, '\\s+')
  
  if (length(cleanfun)>0)
    txt <- lapply(txt, cleanfun)
  else
    txt <- lapply(txt, function(x) {
      # Removing https
      x <- x[which(!grepl('https?[:]//[[:graph:]]+',x))]
      gsub('[[:punct:]]','',x)
    })
  
  txt <- lapply(txt, function(x) x[!(x %in% stopw)])
  
  txt
}
load('data/senate_tweets_example.RData')
words <- tw_words(senate_tweets$text)
head(words)
