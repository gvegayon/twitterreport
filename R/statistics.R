 #' @description Creates a time series object with frecuencies 
 #' @param created_at POSIXct class vector
 tw_timeseries <- function(obj,created_at, units=c('secs','mins','hours','days'),span=1) {
   
   # Old stringAsFactors
   oldstasf <- options()$stringsAsFactors
   options(stringsAsFactors = FALSE)
   
   # Processing the data
   n <- length(obj)
   tmp <- as.data.frame(do.call(rbind,lapply(1:n, function(i,...) {
     cbind(created_at=rep(as.character(created_at[i]),length(obj[[i]])),obj=obj[[i]])
   })))
   
   options(stringsAsFactors = oldstasf)
   
   return(tmp)
 }
 
 x <- tw_extract(senate_tweets)
 y <- tw_timeseries(x$mention,senate_tweets$created_at)
 