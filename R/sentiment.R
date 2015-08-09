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

# ################################################################################
# # From Warriner, A.B., Kuperman, V., & Brysbaert, M. (2013). Norms of valence, arousal, and dominance for 13,915 English lemmas. Behavior Research Methods, 45, 1191-1207.
# # http://crr.ugent.be/archives/1003
# # http://danigayo.info/blog/index.php?entry=entry130117-183114
# ################################################################################
# warriner_et_al_en <- read.csv("ddata/Warriner_et_al/Ratings_Warriner_et_al.csv",
#                                  as.is=TRUE)
# warriner_et_al_en <- subset(warriner_et_al_en, select=c(Word,V.Mean.Sum))
# colnames(warriner_et_al_en) <- c("word","valence")
# write.csv(warriner_et_al_en, file='data/warriner_et_al_en.csv',
#                     quote = FALSE, row.names = FALSE)
# system('xz data/warriner_et_al_en.csv')
# 
# warriner_et_al_es <- read.csv("ddata/Warriner_et_al/Ratings_Warriner_et_al_Spanish.csv",
#                                  as.is=TRUE)
# warriner_et_al_es <- subset(warriner_et_al_es, select=c(Palabra,V.Mean.Sum))
# colnames(warriner_et_al_es) <- c("word","valence")
# write.csv(warriner_et_al_es, file='data/warriner_et_al_es.csv',
#           quote = FALSE, row.names = FALSE)
# system('xz data/warriner_et_al_es.csv')

#' Computes sentiment score
#' 
#' Computes sentiment score by counting the number of positive and negative terms.
#'
#' @param text Character vector
#' @param pos Positive words
#' @param neg Negative words
#' @param pos_s Numeric vector of scores for each word in the pos vec
#' @param neg_s Numeric vector of scores for each word in the neg vec
#' @param lang Languaje of the lexicon (can be either 'en' or 'es')
#' @param normalize Whether or not to normalize the values of each words' score.
#'
#' @details By default uses an english lexicon downloaded from 
#' \url{http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html}.
#' 
#' @seealso \code{\link{sentiment_lexicon_pos_en}} \code{\link{sentiment_lexicon_neg_en}}
#'
#' @return Numeric Vector with scores
#' @export
#'
tw_sentiment <- function(
  text, pos=NULL, neg=NULL, pos_s=NULL, neg_s=NULL, lang='en', normalize=TRUE) {
  x <- sapply(tolower(text), strsplit, split="[^a-zA-Z']")
  x <- lapply(x, gsub, pattern="[^a-zA-Z]", replacement="")
  
  path <- system.file('data/',package='twitterreport')
  
  # Checking language
  if (!(lang %in% c('en','es'))) stop('Invalid language, only \'es\' or \'en\' are supported')
  
  # Checking list of words
  if (!length(pos)) {
    pos<-read.csv(
    paste0(path,'/warriner_et_al_',lang,'.csv.xz'), as.is=TRUE)
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
