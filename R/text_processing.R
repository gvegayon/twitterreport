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

#' @description Process txt extracting relevant words, i.e. rm stop words and
#'  symbols.
#' @param txt A character vector with text to analyze
#' @param Set of stop words
#' @param cleanfun A function to pass to lapply
tw_words <- function(txt, stopw=stopwords('en'), cleanfun=NULL) {
  # Removing URL and punctuation
  txt <- gsub('https?[:]\\/\\/[[:graph:]]+|&amp','',txt)
  txt <- gsub("[^[:alnum:][:space:]']", "", txt)
  
  txt <- tolower(txt)
  txt <- strsplit(txt, '\\s+')
  
  if (length(cleanfun)>0)
    txt <- lapply(txt, cleanfun)
  else
    txt <- lapply(txt, function(x) {
      # Removing RT and monosilabus
      x <- x[!(x %in% c(stopw,'rt',letters))]
    })
  
  txt <- lapply(txt, function(x) x[!grepl('^\\s*$',x)])
  class(txt) <- c('tw_Class_words', class(txt))
  
  txt
}
