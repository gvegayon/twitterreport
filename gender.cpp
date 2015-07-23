#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

//' Matches names with genders
//' @param x
//' @param male
//' @param female
// [[Rcpp::export]]
IntegerVector tw_gender(CharacterVector x, CharacterVector male, CharacterVector female) {
  int n   = x.size();
  int n_m = male.size();
  int n_f = female.size();
  
  // Creating space for the output
  IntegerVector out(n);
  
  for(int j=0;j<n;++j) {
    bool found=false;
    
    // Looking over male names first
    for(int i=0;i<n_m;++i) {
      if (male[i]!=x[j]) continue;
      out[j]=1;
      found=true;
      break;
    }
    
    // If the gender has already been identified
    if (found) continue;
    
    // Now going for the females
    for(int i=0;i<n_f;++i) {
      if (female[i]!=x[j]) continue;
      out[j]=0;
      break;
    }
  }
  
  return out;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
male   <- c('george','jorge','juan')
female <- c('valentina','camila','gabriela')
tw_gender('george',male,female)
tw_gender('valenta',male,female)
tw_gender(c('george','valenta'),male,female)
*/
