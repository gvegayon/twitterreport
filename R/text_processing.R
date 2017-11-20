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
#' @family text processors
#' @aliases tw_Class_extract
#' @export
tw_extract <- function(txt,...) UseMethod('tw_extract')

#' @describeIn tw_extract Applies for the output of [tw_api_get_statuses_user_timeline()].
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
#' @family text processors
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

#' Jaccard coefficient
#' 
#' Calculate the Jaccard (similarity) coefficient between words.
#' 
#' @aliases Words similarities
#' @param x Character vector with the phrases (tweets) to be analyzed.
#' @param max.size Max number of words to analyze.
#' @param stopwds Character vector of stopwords.
#' @param ignore.case When true converts all to lower.
#' @param dist When true computes one minus Jaccard coef.
#' @param ... Further arguments to be passed to the method.
#' @details The Jaccard index is used as a measure of similarity between two
#' elements. In particular for a given pair of elements \eqn{x,y} it is calculated as
#' \deqn{J(S,T) = \frac{|S\cap T|}{|S\cup T|}}{J(S,T) = |S intersection T|/|S U T|}
#' Where \eqn{S} is the set of groups where \eqn{x} is present and 
#' \eqn{T} is the set of groups where \eqn{y}. The resulting value is defined
#' between 0 and 1, where 0 corresponds to no similarity at all (the elements
#' don't have a group in common) and 1 represents perfect similarity (both elements
#' are present in the same groups).
#' @export
#' @references 
#' Conover, M., Ratkiewicz, J., & Francisco, M. (2011). "Political polarization
#' on twitter". Icwsm, 133(26), 89–96. http://doi.org/10.1021/ja202932e
#' @return A list including a lower triangular `dgCMatrix` matrix.
jaccard_coef <- function(x,...) UseMethod("jaccard_coef")

#' @describeIn jaccard_coef Method Processes a list of character vectors such as
#' the one obtained from [tw_extract()]
#' @export
jaccard_coef.list <- function(x, max.size=1000, dist=FALSE, ...) {
  
  # Cohersing the list as a data.frame
  x <- cpp_char_list_as_df(x)
  colnames(x) <- c("wrd","id")
  x <- x[which(!grepl('^\\s+$',x$wrd)),]
  
  # We dont want to use all of them
  if (nrow(x)>max.size) {
    y <- group_by_(x,~wrd)
    y <- as.data.frame(dplyr::summarise_(y, "n()"))
    colnames(y) <- c('wrd','n')
    y <- y[order(-y[,c('n')]),]
    y <- y[y[,c('n')]>1,]
    x <- semi_join(x,y[1:max.size,],by="wrd")
  }
  
  # Computing Jaccards index
  x$wrd <- factor(x$wrd, ordered = FALSE)
  jaccard <- with(x,cpp_jaccard_coef(as.numeric(wrd),as.numeric(id), dist))
  
  # Preparing output
  wrd_names <- unique(x$wrd)
  wrd_names <- wrd_names[order(wrd_names)]
  colnames(jaccard) <- wrd_names
  rownames(jaccard) <- colnames(jaccard)
  
  # Freq table to show for the print class
  tab <- data.frame(wrd=attr(jaccard,"Dimnames")[[1]],n=diag(jaccard), 
                    stringsAsFactors = FALSE)
  tab <- tab[order(-tab$n),]
  row.names(tab) <- 1:nrow(tab)
  
  # Wrapping result
  jaccard <- list(mat=jaccard, nwords=attr(jaccard,"Dim")[1], words=attr(jaccard,"Dimnames")[[1]],
                  ntexts=length(unique(x$id)), freq=tab)
  
  class(jaccard) <- c('tw_Class_jaccard', class(jaccard))
  
  return(jaccard)
}

#' @describeIn jaccard_coef Computes the coef from a vector of characters
#' (splits the text)
#' @export
jaccard_coef.character <- function(x,max.size=1000,
                         stopwds=unique(c(tm::stopwords(),letters)), 
                         ignore.case=TRUE, 
                         dist=FALSE, ...) {
  # Parsing text
  if (ignore.case) x <- tolower(x)
  
  # Removing URL and punctuation
  x <- gsub('https?[:]\\/\\/[[:graph:]]+|&amp','',x)
  x <- gsub("[^[:alnum:][:space:]']", "", x)
  
  # Creating ids
  x <- sapply(x, strsplit, split='\\s+')
  
  x <- sapply(x, function(y) {
    y[which(!(y %in% stopwds))]
  })
  
  jaccard_coef.list(x, max.size=1000, dist=dist)
}

#' Retrieves a set of words related to a particular word
#'
#' @param word A character word to analyze
#' @param jaccard A `tw_Class_jaccard` class object
#' @param criter Minimun number in the index to show
#' @param exact When `FALSE` shearch via regular expression
#' @return A data frame containing the list of words that are related
#' to the specified `word`
#' @details After applying the [jaccard_coef()] function, the 
#' resulting object can be analyzed with this function.
#' @export
#' @examples 
#' \dontrun{
#' # Computing the jaccard coefficient
#' jaccard <- jaccard_coef(tweets$text)
#' 
#' # See what words are related with abortion
#' words_closeness('abortion',jaccard,.001)
#' }
words_closeness <- function(word,jaccard,criter=0.01,exact=FALSE) {
  
  # Checking if we have a jaccard object
  if (!inherits(jaccard,"tw_Class_jaccard")) 
    stop('A -tw_Class_jaccard- class object must be provided')
  
  # Tabulating the data  
  tab <- data.frame(freq=diag(jaccard$mat),wrd=jaccard$words, stringsAsFactors = FALSE)
  
  if (exact) index <- which(jaccard$words==word)
  else {
    index <- which(grepl(word,tab$wrd))
    index <- index[order(-tab$freq[index])][1]
  }
  
  if (!length(index)) return(NULL)
  set <- which(jaccard$mat[index,]>=criter)
  set <- data.frame(word=jaccard$words[set],coef=jaccard$mat[index,set],
                    stringsAsFactors = FALSE)
  set <- set[order(-set$coef),]
  rownames(set) <- 1:nrow(set)
  
  set
}

# words_closeness('abortion',jaccard,.001)
