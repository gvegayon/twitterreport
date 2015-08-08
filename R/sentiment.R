# http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#datasets
rm(list=ls())
sentiment_lexicon_neg_en <- read.csv(header = FALSE, as.is = TRUE,
  skip = 35,file="ddata/opinion_lexicon_en/opinion-lexicon-English/negative-words.txt")
sentiment_lexicon_neg_en <- unlist(sentiment_lexicon_neg_en)

sentiment_lexicon_pos_en <- read.csv(
  header = FALSE,skip = 35, as.is=TRUE,
  file="ddata/opinion_lexicon_en/opinion-lexicon-English/positive-words.txt")
sentiment_lexicon_pos_en <- unlist(sentiment_lexicon_pos_en)

save(sentiment_lexicon_neg_en, file='data/sentiment_lexicon_neg_en.rdata', compress = TRUE)
save(sentiment_lexicon_pos_en, file='data/sentiment_lexicon_pos_en.rdata', compress = TRUE)

