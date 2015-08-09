/* http://www.lindonslog.com/programming/r/rcpp/
 * https://wbnicholson.wordpress.com/2014/07/10/parallelization-in-rcpp-via-openmp/
 * http://gribblelab.org/CBootcamp/A2_Parallel_Programming_in_C.html
 */

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cpp_tw_sentiment(
    const List x, 
    CharacterVector pos, 
    CharacterVector neg     = CharacterVector::create(),
    const NumericVector pos_score = NumericVector::create(),
    const NumericVector neg_score = NumericVector::create(),
    CharacterVector neu     = CharacterVector::create(),
    const NumericVector neu_score = NumericVector::create(),
    bool normalize          = true)
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
  NumericVector neu_s(n_neu,0.0);
  
  if (pos_score.size()) pos_s = clone(pos_score);
  if (neg_score.size()) neg_s = clone(neg_score);
  if (neu_score.size()) neu_s = clone(neu_score);
  
  // Checking if normalize
  if (normalize) {
    if (n_pos) {
      double maxs = -DBL_MAX, mins = DBL_MAX;
      for (int i=0;i<n_pos;++i) {
        if (pos_s[i]>maxs) maxs = pos_s[i];
        if (pos_s[i]<mins) mins = pos_s[i];
      }
      pos_s = ((pos_s-mins)/(maxs-mins))*2.0-1.0;
    }
    
    if (n_neg) {
      double maxs = -DBL_MAX, mins = DBL_MAX;
      for (int i=0;i<n_neg;++i) {
        if (neg_s[i]>maxs) maxs = neg_s[i];
        if (neg_s[i]<mins) mins = neg_s[i];
      }
      
      neg_s = ((neg_s-mins)/(maxs-mins))*2.0-1.0;
    }
    
    if (n_neu) {
      double maxs = -DBL_MAX, mins = DBL_MAX;
      for (int i=0;i<n_neu;++i) {
        if (neu_s[i]>maxs) maxs = neu_s[i];
        if (neu_s[i]<mins) mins = neu_s[i];
      }
      
      neu_s = ((neu_s-mins)/(maxs-mins))*2.0-1.0;
    }
  }
  
  for(int i=0;i<n;++i) { // Loop thorugh sentences
    
    // Getting the sentence and number of letters
    CharacterVector wrds = as<CharacterVector>(x[i]);
    int m = wrds.size();
        
    // Loop through words (and adding to the score)
    for(int j=0;j<m;++j) {
      bool found = false;
      
      // Loop through POS
      for(int k=0;k<n_pos;++k) {
        if (pos[k]!=wrds[j]) continue;
        score[i] += pos_s[k];
        found = true;
        break;
      }
      
      // Loop through NEG
      if (!n_neg | found) continue;
      for(int k=0;k<n_neg;++k) {
        if (neg[k]!=wrds[j]) continue;
        score[i] += neg_s[k];
        found = true;
        break;
      }

      // Loop through NEU
      if (!n_neu | found) continue;
      for(int k=0;k<n_neu;++k) {
        if (neu[k]!=wrds[j]) continue;
        score[i] += neu_s[k];
        break;
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
  "I'm feeling sad today",
  "This is terrible, bad",
  "I'm doing ok",
  "I'm feeling great",
  "I'm feeling nothing",
  "This is sick, but good at the same time")
Text <- sapply(text, strsplit, split="\\s+")
Text <- lapply(Text, gsub, pattern="[^a-zA-Z]", replacement="")

# Normalized
score <- cpp_tw_sentiment(Text, pos, neg, pos_score, neg_score)
data.frame(score,text)

# Non-normalized
score <- cpp_tw_sentiment(Text, pos, neg, pos_score, neg_score, normalize = FALSE)
data.frame(score,text)

*/
