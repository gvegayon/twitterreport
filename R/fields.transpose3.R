fields.transponse3 <- function(data, id, delim = ',') {
  
  if (NROW(data) > 0) {
    x <- lapply(X=as.list(data), function(X) unlist(strsplit(X, split=delim)))
    nreps <- lapply(X=x, function(X) length(X))
    
    nobs <- 1:NROW(data)
    x <- unlist(x)
    
    if ((ncols <- NCOL(id)) > 1) {
      for (i in 1:NCOL(id)) {
        x <- data.frame(x,unlist(
          lapply(X=nobs, function(X) rep(id[X,i], nreps[[X]]))
        ),stringsAsFactors=F)
      }
    }
    else {
      x <- data.frame(x, unlist(lapply(X=nobs, function(X) rep(id[X], nreps[[X]]))))
    }
    if (length(nme1 <- colnames(data)) == 0) nme1 <- paste("val",1:NCOL(data),sep="")
    if (length(nme2 <- colnames(id)) == 0) nme2 <- paste("id",1:NCOL(id),sep="")
    colnames(x) <- c(nme1, nme2)
  }
  else x <- NULL
  return(x)
}

#fields.transponse <- compiler::cmpfun(fields.transponse, )