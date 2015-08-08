#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cpp_tw_sentiment(
    List x, 
    CharacterVector pos, CharacterVector neg,
    NumericVector pos_score = NumericVector::create(),
    NumericVector neg_score = NumericVector::create(),
    CharacterVector neu     = CharacterVector::create(),
    NumericVector neu_score = NumericVector::create())
{
  
  // Number of elements
  int n     = x.size();
  int n_pos = pos.size();
  int n_neg = neg.size();
  int n_neu = neu.size();
  
  NumericVector score(n);
  
  // Checking scores
  NumericVector pos_s(n_pos,1.0);
  NumericVector neg_s(n_neg,-1.0);
  NumericVector neu_s(n_neu,-1.0);
  
  if (pos_score.size()) pos_s = pos_score;
  if (neg_score.size()) neg_s = neg_score;
  if (neu_score.size()) neu_s = neu_score;
  
  for(int i=0;i<n;++i) { // Loop thorugh sentences
    
    // Getting the sentence and number of letters
    CharacterVector wrds = as<CharacterVector>(x[i]);
    int m = wrds.size();
        
    // Loop through words (and adding to the score)
    for(int j=0;j<m;++j) {
      // Loop through POS
      for(int k=0;k<n_pos;++k) {
        if (pos[k]!=wrds[j]) continue;
        score[i] += pos_s[k];
      }
      
      // Loop through NEG
      for(int k=0;k<n_neg;++k) {
        if (neg[k]!=wrds[j]) continue;
        score[i] += neg_s[k];
      }

      // Loop through NEU
      if (!n_neu) continue;
      for(int k=0;k<n_neu;++k) {
        if (neu[k]!=wrds[j]) continue;
        score[i] += neu_s[k];
      }
      
    }
  }
  return score;
}

/*** R
# Example lexicon
pos <- c("good","ok","fine","great")
pos_score <- c(2,1,1,3)

neg <- c("bad","terrible","sick","sad")
neg_score <- c(-1,-4,-2,-3)

# Example data
text <- c(
  "I'm fealing sad today",
  "This is terrible, bad",
  "I'm doing ok",
  "I'm feeling great",
  "I'm feeling nothing",
  "This is sick, but good at the same time")
Text <- sapply(text, strsplit, split="\\s+")
Text <- lapply(Text, gsub, pattern="[^a-zA-Z]", replacement="")

score <- cpp_tw_sentiment(Text, pos, neg, pos_score, neg_score)
data.frame(score,text)

*/
