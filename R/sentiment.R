# # # http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#datasets
# rm(list=ls())
# sentiment_lexicon_neg_en <- read.csv(header = FALSE, as.is = TRUE,
#   skip = 35,file="ddata/opinion_lexicon_en/opinion-lexicon-English/negative-words.txt",
#   col.names = 'word')
# 
# sentiment_lexicon_pos_en <- read.csv(
#   header = FALSE,skip = 35, as.is=TRUE, col.names='word',
#   file="ddata/opinion_lexicon_en/opinion-lexicon-English/positive-words.txt")
# 
# # save(sentiment_lexicon_neg_en, file='data/sentiment_lexicon_neg_en.rdata', compress = TRUE)
# # save(sentiment_lexicon_pos_en, file='data/sentiment_lexicon_pos_en.rdata', compress = TRUE)
# write.csv(sentiment_lexicon_neg_en, file='data/sentiment_lexicon_neg_en.csv',
#           quote = FALSE, row.names = FALSE)
# write.csv(sentiment_lexicon_pos_en, file='data/sentiment_lexicon_pos_en.csv',
#           quote = FALSE, row.names = FALSE)

################################################################################
# From Warriner, A.B., Kuperman, V., & Brysbaert, M. (2013). Norms of valence, arousal, and dominance for 13,915 English lemmas. Behavior Research Methods, 45, 1191-1207.
# http://crr.ugent.be/archives/1003
# http://danigayo.info/blog/index.php?entry=entry130117-183114
################################################################################
# warriner_et_al_en <- read.csv("ddata/Warriner_et_al/Ratings_Warriner_et_al.csv",
#                                  as.is=TRUE, dec = '.')
# warriner_et_al_en <- subset(warriner_et_al_en, select=c(Word,V.Mean.Sum))
# colnames(warriner_et_al_en) <- c("word","valence")
# write.table(warriner_et_al_en, file='data/warriner_et_al_en.csv',
#                     quote = FALSE, row.names = FALSE, sep=";")
# system('xz -f data/warriner_et_al_en.csv')
# 
# warriner_et_al_es <- read.csv("ddata/Warriner_et_al/Ratings_Warriner_et_al_Spanish.csv",
#                                  as.is=TRUE, dec='.')
# warriner_et_al_es <- subset(warriner_et_al_es, select=c(Palabra,V.Mean.Sum))
# colnames(warriner_et_al_es) <- c("word","valence")
# write.table(warriner_et_al_es, file='data/warriner_et_al_es.csv',
#           quote = FALSE, row.names = FALSE, sep=";")
# system('xz -f data/warriner_et_al_es.csv')

#' Computes sentiment score
#' 
#' Computes sentiment score by counting the number of positive and negative terms.
#'
#' @param text Character vector with the text to be classified
#' @param pos Positive words lexicon
#' @param neg Negative words lexicon
#' @param pos_s Numeric vector of scores for each word in the pos vec
#' @param neg_s Numeric vector of scores for each word in the neg vec
#' @param lan Languaje of the lexicon (can be either 'en' or 'es')
#' @param normalize Whether or not to normalize the values of each words' score.
#'
#' @details By default uses an english lexicon downloaded from 
#' \url{http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html}. In particular
#' if no list of words is provided, the function loads the dataset 
#' \code{\link{warriner_et_al_en}} or \code{\link{warriner_et_al_es}}, depending
#' on the \code{lang} argument, both of which contains a vector of words with
#' their respective \emph{valence} (pleasantness of the stimulus).
#' 
#' The arguments \code{pos_s} and \code{neg_s} allow to provide with scores
#' (valence) to each vector of words, so instead of adding 1 or -1 for every
#' positive or negative word the function can add some other value specified by
#' these arguments.
#' 
#' By default, when using the default lexicon, the function loads the english
#' lexicon (\code{lan='en'}). Otherwise, if \code{lan} is set to 'es', it
#' will load the spanish lexicon.
#' 
#' When \code{normalize} is set to \code{TRUE} (default), the function will normalize
#' the scores values for each word such as all values are within -1 and 1. Note that
#' this does not implies that the final sentiment score will be in that range as
#' well since it is a result of the addition of the scores.
#' 
#' @seealso \code{\link{sentiment_lexicon_pos_en}} \code{\link{sentiment_lexicon_neg_en}}
#'
#' @return Numeric Vector with scores
#' 
#' @examples 
#'  # Example data
#'  text <- c(
#'    "I'm feeling sad today",
#'    "This is terrible, bad",
#'    "I'm doing ok",
#'    "I'm feeling great",
#'    "I'm feeling nothing",
#'    "This is sick, but good at the same time")
#'  
#'  # Getting the scores
#'  tw_sentiment(text)
#'  tw_sentiment(text, normalize=FALSE)
#' 
#' @export
#'
tw_sentiment <- function(
  text, pos=NULL, neg=NULL, pos_s=NULL, neg_s=NULL, lan='en', normalize=TRUE) {
  
  # Processing text
  x <- sapply(tolower(text), strsplit, split="[^a-zA-Z']")
  x <- lapply(x, gsub, pattern="[^a-zA-Z]", replacement="")
  
  path <- system.file('data/',package='twitterreport')
  
  # Checking language
  if (!(lan %in% c('en','es'))) stop('Invalid language, only \'es\' or \'en\' are supported')
  
  # Checking list of words
  if (!length(pos)) {
    pos<-read.table(sep=';',header = TRUE,file=
    paste0(path,'/warriner_et_al_',lan,'.csv.xz'), as.is=TRUE)
    pos_s <- pos$valence
    pos   <- pos$word
  }
  if (!length(neg)) {
    neg   <- character()
    neg_s <- numeric()
  }
  
  # Checking scores
  if (!length(pos_s)) pos_s <- rep(1, length(pos))
  if (!length(neg_s)) neg_s <- rep(-1, length(neg))
  
  cpp_tw_sentiment(x, pos, neg, pos_s, neg_s, normalize = normalize)
}
