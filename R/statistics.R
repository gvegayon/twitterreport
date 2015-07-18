 #' @description Creates a time series object with frecuencies 
 #' @param created_at POSIXct class vector
 tw_timeseries <- function(
   obj,created_at, units=c('secs','mins','hours','days'),
   nseries=5, span=1) {
   
   # Old stringAsFactors
   oldstasf <- options()$stringsAsFactors
   options(stringsAsFactors = FALSE)
   
   # Rounding
   if (length(units)>1) units <- 'hours'
   created_at <- round(created_at, units)
   
   # Processing the data
   n <- length(obj)
   tmp <- as.data.frame(do.call(rbind,lapply(1:n, function(i,...) {
     cbind(
       created_at = rep(as.character(created_at[i]),length(obj[[i]])),
       obj        = obj[[i]])
   })))
   
   
   tmp <- group_by(tmp, created_at, obj)
   tmp <- summarise(tmp, n=n())
   tmp <- tmp[order(-tmp$n),]
   
   print(lapply(tmp,class))
   # Getting the top k elements
   series <- unique(tmp$obj)[1:nseries]
   series <- subset(tmp, obj %in% series)
   
   options(stringsAsFactors = oldstasf)
   
   return(tmp)
 }
 
 x <- tw_extract(senate_tweets$text)
 head(tw_timeseries(x$mention,senate_tweets$created_at, units='days'), 10)
 head(y<-tw_timeseries(x$hashtag,senate_tweets$created_at, units='days'), 10)
 head(unique(y$obj))
 subset(y, obj=='irandeal')