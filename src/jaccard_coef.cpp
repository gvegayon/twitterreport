#include <RcppArmadillo.h> /* This already includes Rcpp */
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::sp_mat cpp_jaccard_coef(IntegerVector wrds_id, IntegerVector wrds_grp, bool dist=false) {
  
  IntegerVector unique_wrds = unique(wrds_id);
  const int n = unique_wrds.size();
  const int w = wrds_id.size();
  
  arma::sp_mat X = arma::sp_mat(n,n);
  
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
    for(int j=0;j<i;++j)
      X(i,j) = X(i,j)/(X(i,i) + X(j,j) - X(i,j));
  
  /* If the user requires distance instead */
  if (dist)
    for(int i=0;i<X.n_rows;++i)
      for(int j=0;j<i;++j)  
        if (X(i,j)>0) X(i,j) = 1-X(i,j);
  
  return X;
      
}


/* This function takes a list of characte and coherses it into a dataframe with
two column, -wrd- and -id-. It is twice as fast as applying a apply method */
// [[Rcpp::export]]
DataFrame cpp_char_list_as_df(List & X) {
  int n = X.size();
  std::vector< std::string > wrd;
  std::vector< int > id;
  
  for(int i=0; i<n;++i) {
    CharacterVector tmp = X[i];
    for(int j=0;j<tmp.size();++j)
      wrd.push_back(as<std::string>(tmp[j])), id.push_back(i+1);
  }
  
  return DataFrame::create(
    _["wrd"]=wrd, _["id"]=id
  );
}



/*** R
library(Matrix)
library(dplyr)

x <- c('perro gato pato', 'pato gato pollo', 'pato gato caballo', 'caballo gato')

jaccard_coef <- function(x,max.size=1000,
                         stopwds=unique(c(tm::stopwords(),letters)), 
                         ignore.case=TRUE, 
                         dist=FALSE) {
  # Parsing text
  if (ignore.case) x <- tolower(x)
  
  # Removing URL and punctuation
  x <- gsub('https?[:]\\/\\/[[:graph:]]+|&amp','',x)
  x <- gsub("[^[:alnum:][:space:]']", "", x)
  
  # Creating ids
  x <- sapply(x, strsplit, split='\\s+')

  x <- sapply(x, function(y) {
    y[which(!(y %in% stopwds))]
  })

  x <- cpp_char_list_as_df(x)
  
  # We dont want to use all of them
  if (nrow(x)>max.size) {
    y <- group_by(x, wrd)
    y <- as.data.frame(summarise(y, n=n()))
    y <- y[order(-y$n),]
    y <- subset(y, n>1)
    x <- semi_join(x,y[1:max.size,],by="wrd")
  }
  
  # Computing Jaccards index
  x$wrd <- factor(x$wrd, ordered = FALSE)
  jaccard <- with(x,cpp_jaccard_coef(as.numeric(wrd),as.numeric(id), dist))
  
  # Preparing output
  wrd_names <- unique(x$wrd)
  wrd_names <- wrd_names[order(wrd_names)]
  colnames(jaccard) <- wrd_names
  rownames(jaccard) <- colnames(jaccard)

  # Wrapping result
  jaccard <- list(mat=jaccard, nwords=attr(jaccard,"Dim")[1], words=attr(jaccard,"Dimnames")[[1]],
                  ntexts=length(unique(x$id)))
  
  class(jaccard) <- 'TR_Class_jaccard'
  
  return(jaccard)
}

#' plot method
plot.TR_Class_jaccard <- function(x, ...) {
  n <- x$nwords
  cat("Jaccard index Matrix (Sparse) of ",n,"x",n,"\n",sep="")
  cat("contains the following words:\n")
  print(head(x$words))
  invisible(x)
}


num<-which(colnames(u$mat)=="whitehouse");as.data.frame(u$mat[num,which(u$mat[num,]>0.025)])

z <- jaccard_coef(x,dist = TRUE)
z
k<-which(z==0);z[k] <- rep(1e10,length(k))
clust <- hclust(as.dist(z))
cutree(clust,4)
plot(clust)


# Test with data from twitter
load('/home/george/Documents/projects/twitterreport/data/senate_tweets.rdata')
tweets <- senate_tweets$text

u <- jaccard_coef(unique(tweets),max.size = 1000)

clust <- hclust(1-z)

y <- data.frame(wrd=colnames(x),id=cutree(clust,20))
View(y)
*/