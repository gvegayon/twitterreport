#include <RcppArmadillo.h> /* This already includes Rcpp */
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::sp_mat cpp_jaccard_coef(IntegerVector wrds_id, IntegerVector wrds_grp, bool debug=true) {
  
  IntegerVector unique_wrds = unique(wrds_id);
  const int n = unique_wrds.size();
  const int w = wrds_id.size();
  
  arma::sp_mat X = arma::sp_mat(n,n);
  
  /* Counting number of repetitions */
  for(int i=0;i<w;++i)
    for(int j=i;j<w;++j) {
      if (wrds_grp[i]==wrds_grp[j]) {
        if (debug & i==j) Rprintf("i:%i j:%i\n",wrds_id[i],wrds_id[j]);
        /* In order to generate a lower triangular matrix */
        if (wrds_id[i]>wrds_id[j]) X(wrds_id[i]-1,wrds_id[j]-1)+=1;
        else X(wrds_id[j]-1,wrds_id[i]-1)+=1;
      }
    }
  
  /* Computing the index */
//   for(int i=0;i<n;++i)
//     for(int j=0;j<n;++j)
//       X(i,j) = 
  
  return X;
      
}

/*** R
library(Matrix)
x <- c('perro gato pato', 'pato gato pollo', 'pato gato caballo', 'caballo gato')

jaccard_coef <- function(x,stopwds=tm::stopwords()) {
  # Creating ids
  x <- sapply(x, strsplit, split='\\s+')
  names(x) <- 1:length(x)
  
  x <- lapply(names(x), function(i) {
    data.frame(wrds=x[[i]],id=rep(i, length(x[[i]])), stringsAsFactors = FALSE)
  })
  
  x <- dplyr::bind_rows(x)
  x$wrds <- factor(x$wrds, ordered = FALSE) 
  x$wrds_id <- as.numeric(x$wrds)
  
  # Computing Jaccards index
  jaccard <- with(x,cpp_jaccard_coef(as.numeric(wrds),as.numeric(id), debug = FALSE))
  
  # Preparing output
  wrd_names <- unique(x$wrds)
  wrd_names <- wrd_names[order(wrd_names)]
  colnames(jaccard) <- wrd_names
  rownames(jaccard) <- colnames(jaccard)
  
  return(jaccard)
}

jaccard_coef(x)
*/