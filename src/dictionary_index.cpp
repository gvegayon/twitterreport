#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame cpp_dic_indexes(CharacterVector x) {
  
  // Creating the index
  std::vector<std::string> strings(x.size()); 
  for(int i=0;i<x.size();++i)
    strings[i] = std::string(x[i]);
  
  std::vector<std::string> w;
  std::vector<int> ini;
  std::vector<int> end;
  std::vector<int> n;
  
  // If the vector is of length 0
  if (strings.size()<2) {
    stop("It doesn't make any sense to get a dictionary from less than 2 words");
  }
  
  // Initializing values
  w.push_back(strings[0].substr(0,1));
  ini.push_back(0);
  end.push_back(0);
  n.push_back(1);
  
  for(int i=1;i<strings.size();++i) {
    // Current size
    int nw = n.size();
    
    std::string tmps = strings[i].substr(0,1);
    
    if (w[nw-1]!=tmps) {
      // Next index
      w.push_back(tmps);
      ini.push_back(i);
      end.push_back(i);
      n.push_back(1);
      
    }
    else {
      n[nw-1]+=1;
      end[nw-1]+=1;
    }
  }
  
  return DataFrame::create(
    _["word"] = w,
    _["ini"]  = ini,
    _["end"]  = end,
    _["n"]    = n
  ); 
}

// Get the indexes of the word
// [[Rcpp::export]]
NumericVector cpp_get_index(CharacterVector & y, DataFrame & dic) {
  // Defining variables and accesing the columns
  std::string x        = std::string(y[0]);
  std::string letter   = x.substr(0,1);
  NumericVector ini    = dic["ini"];
  NumericVector end    = dic["end"];
  CharacterVector word = dic["word"];
  
  NumericVector index(2,0.0);
  
  for(int i=0;i<word.size();++i) { 
    if (letter==std::string(word[i])) { 
      index[0]=ini[i],index[1]=end[i];
    }
  }
  return index;
}

/***R
library(twitterreport)
x <- c("abc","acd","hola", "zeta","zata","pedro")

cpp_dic_indexes(sort(x))

data(sentiment_lexicon_pos_en)
z <- as.character(sentiment_lexicon_pos_en$x)
z <- sort(iconv(z,to="ASCII//TRANSLIT"))
cpp_dic_indexes(z)

data(warriner_et_al_es)
w <- sort(as.character(tolower(warriner_et_al_es$word)))
q<-cpp_dic_indexes(sort(iconv(w,to="ASCII//TRANSLIT")))
cpp_get_index(c("hola"),q)

*/
