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
//' 
//' Internal use, please refer to \code{\link{tw_gender}} instead.
//' 
//' @param x Character vector with names
//' @param male Character vector with male names
//' @param female Character vector with female names
//' @return Numeric vector with -1, 0, and 1 as 'unidentified', 'female', 'male'
// [[Rcpp::export]]
IntegerVector cpp_tw_gender(CharacterVector x, CharacterVector male, CharacterVector female) {
  int n   = x.size();
  int n_m = male.size();
  int n_f = female.size();
  
  // Creating space for the output
  IntegerVector out(n,-1);
  LogicalVector isna(n);
  isna = is_na(x);
  
  for(int j=0;j<n;++j) {
    bool found=false;

    if (isna[j]) continue;
    
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
# Examples
data("senators_profile")

# Preparing the data
sen <- gsub('\\bsen(ator|\\.)\\s+','',senators_profile$tw_name,ignore.case = TRUE)
sen <- gsub('\\s+.+','',tolower(sen))

ismale <- tw_gender(sen)
View(data.frame(name=sen,male=ismale,senators_profile$tw_name))
barplot(table(ismale))
*/
