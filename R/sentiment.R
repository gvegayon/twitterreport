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
# 
# warriner_et_al_es <- read.csv("ddata/Warriner_et_al/Ratings_Warriner_et_al_Spanish.csv",
#                                  as.is=TRUE)
# warriner_et_al_es <- subset(warriner_et_al_es, select=c(Palabra,V.Mean.Sum))
# colnames(warriner_et_al_es) <- c("word","valence")

#' Computes sentiment score
#' 
#' Computes sentiment score by counting the number of positive and negative terms.
#'
#' @param text Character vector
#' @param pos Positive words
#' @param neg Negative words
#' @param lang Languaje
#'
#' @details By default uses an english lexicon downloaded from 
#' \url{http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html}.
#' 
#' @seealso \code{\link{sentiment_lexicon_pos_en}} \code{\link{sentiment_lexicon_neg_en}}
#'
#' @return Numeric Vector with scores
#' @export
#'
tw_sentiment <- function(text, pos=NULL, neg=NULL, lang='en') {
  x <- sapply(tolower(text), strsplit, split="[^a-zA-Z']")
  x <- lapply(x, gsub, pattern="[^a-zA-Z]", replacement="")
  
  path <- system.file('data/',package='twitterreport')
  
  if (!length(pos)) pos<-read.csv(
    paste0(path,'/sentiment_lexicon_pos_',lang,'.csv.xz'), as.is=TRUE)
  if (!length(neg)) neg<-read.csv(
    paste0(path,'/sentiment_lexicon_neg_',lang,'.csv.xz'), as.is=TRUE)
  
  cpp_tw_sentiment(x, pos[,1], neg[,1])
}
