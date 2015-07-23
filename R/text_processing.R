#' Extract info from tweets
#' @param txt Object to be passed to the method
#' @param obj List of objects to extract
#' @param normalize bool whether or not to normalize emails, hashtags and mentions (to lower)
#' @param ... Further arguments to be passed to the method
#' @return List
#' @examples  
#' \dontrun{
#' head(tw_extract(tweets$text))
#' }
#' @author George G. Vega Yon
#' @export
tw_extract <- function(txt,...) UseMethod('tw_extract')

#' @describeIn tw_extract Applies for the output of \code{\link{tw_api_get_statuses_user_timeline}}.
#' @export
tw_extract.tw_Class_api_timeline <- function(txt,...) {
  tw_extract(txt$text,...)
}

#' @describeIn tw_extract Applies for a character vector
#' @export
tw_extract.character <- function(txt, obj = c("email", "mention", "hashtag", "url"),
                       normalize=TRUE,...) {
  
  # Checking if the obj is well specified
  original <- c("email", "mention", "hashtag", "url")
  test <- which(!(obj %in% original))
  if (any(test))
      stop('-obj- object list badly specified, should be any of ',
           paste0('\'',original,'\'',collapse=', '),'.')
  
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
  
  class(output) <- c('tw_Class_extract',class(output))
  
  return(output)
}

#' Process txt extracting relevant words, i.e. rm stop words and
#'  symbols.
#' @param txt A character vector with text to analyze
#' @param stopw Set of stop words
#' @param cleanfun A function to pass to lapply
#' @author George G. Vega Yon
#' @export
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
