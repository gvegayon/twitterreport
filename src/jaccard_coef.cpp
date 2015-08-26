#include <RcppArmadillo.h> /* This already includes Rcpp */
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::sp_mat cpp_jaccard_coef(IntegerVector wrds_id, IntegerVector wrds_grp) {
  
  IntegerVector unique_wrds = unique(wrds_id);
  const int n = unique_wrds.size();
  const int w = wrds_id.size();
  
  arma::sp_mat X = arma::sp_mat(n,n);
  Rprintf("w:%i and n:%i\n",w,n);
  /* Counting number of group intersections */
  for(int i=0;i<w;++i)
    for(int j=i;j<w;++j) 
      if (wrds_grp[i]==wrds_grp[j]) {
        /* In order to generate a lower triangular matrix */
        if (wrds_id[i]>wrds_id[j]) X(wrds_id[i]-1,wrds_id[j]-1)+=1;
        else X(wrds_id[j]-1,wrds_id[i]-1)+=1;
      }
  
  /* Computing the index, in particular, each cell represents the cardinality of
    the intersection, while diagonal represents the number of sets in which each
    element is present
    
    |S U T| = |S| + |T| - |S intersect T|
    
    */
  
  for(int i=0;i<n;++i)
    for(int j=0;j<n;++j)
      X(i,j) = X(i,j)/(X(i,i) + X(j,j) - X(i,j));
  
  return X;
      
}

/* / [[Rcpp::export]] */

/*** R
library(Matrix)
library(dplyr)

x <- c('perro gato pato', 'pato gato pollo', 'pato gato caballo', 'caballo gato')

jaccard_coef <- function(x,max.size=1000,stopwds=tm::stopwords(), ignore.case=TRUE) {
  # Creating ids
  if (ignore.case) x <- tolower(x)
  x <- sapply(x, strsplit, split='\\s+')
  names(x) <- 1:length(x)
  
  x <- lapply(names(x), function(i) {
    data.frame(wrds=x[[i]],id=rep(i, length(x[[i]])), stringsAsFactors = FALSE)
  })
  
  x <- dplyr::bind_rows(x)

  # We dont want to use all of them
  if (nrow(x)>max.size) {
    y <- group_by(x, wrds)
    y <- as.data.frame(summarise(y, n=n()))
    y <- y[order(-y$n),]
    y <<- subset(y, n>1)
    x <- semi_join(x,y[1:max.size,],by="wrds")
  }
  
  # Computing Jaccards index
  pre_x <<- x
  x$wrds <- factor(x$wrds, ordered = FALSE)
  jaccard <- with(x,cpp_jaccard_coef(as.numeric(wrds),as.numeric(id)))
  
  # Preparing output
  wrd_names <- unique(x$wrds)
  wrd_names <- wrd_names[order(wrd_names)]
  colnames(jaccard) <- wrd_names
  rownames(jaccard) <- colnames(jaccard)
  
  return(as.matrix(jaccard))
}

z <- as.dist(jaccard_coef(x),FALSE)
z
clust <- hclust(1-z)
cutree(clust,4)
plot(clust)
table(unlist(sapply(x, strsplit, split='\\s+')))

# Test with data from twitter
load('/home/george/Documents/projects/twitterreport/data/senate_tweets.rdata')
tweets <- senate_tweets$text
tweets <- stringr::str_extract_all(tweets,'#[a-zA-Z0-9]+',FALSE)
test <- which(sapply(tweets, length)>0)
tweets <- tweets[test]
tweets <- unlist(lapply(tweets, paste, sep=" "),recursive = FALSE)

x <- jaccard_coef(unique(tweets),max.size = 1000)
z <- as.dist(x,FALSE)
z
clust <- hclust(1-z)
y <- data.frame(wrd=colnames(x),id=cutree(clust,20))
View(y)
*/