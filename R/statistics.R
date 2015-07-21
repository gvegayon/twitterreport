#' @description Creates a time series object with frecuencies 
#' @param obj Any of the members of the output list from tw_extract()
#' @param created_at POSIXct class vector
#' @param span Number of time units to consider
#' @param nseries Number of series to include
#' @param units Time unit
#' @return xts class 
#' @author George G. Vega Yon
#' @export
tw_timeseries <- function(
  obj,created_at, units=c('secs','mins','hours','days'),
  nseries=5, span=30) {
  
  # Old stringAsFactors
  oldstasf <- options()$stringsAsFactors
  options(stringsAsFactors = FALSE)
  
  # Rounding
  if (length(units)>1) units <- 'days'
  created_at <- round(created_at, units)
  
  # Processing the data
  n <- length(obj)
  data <- as.data.frame(do.call(rbind,lapply(1:n, function(i,...) {
    cbind(
      created_at = rep(as.character(created_at[i]),length(obj[[i]])),
      obj        = obj[[i]])
    })))
  
  grp <- group_by(data, created_at, obj)
  tmp <- summarise(grp, n=n())
  tmp <- tmp[order(-tmp$n),]
  
  # Getting the top k elements
  grp <- group_by(tmp, obj)
  who <- summarise(grp, n=sum(n))
  who <- who[order(-who$n),]$obj[1:nseries]
  
  # series <- unique(tmp$obj)[1:nseries]
  series <- as.data.frame(subset(tmp, obj %in% who))
  rm(tmp)
  
  # Creating the timeseries dataframe
  series <- reshape(
    series,timevar = 'obj',idvar = 'created_at', v.names = 'n', direction='wide')
  series <- series[order(series$created_at),]
  
  # Fixing names and filtering
  row.names(series) <- series$created_at
  colnames(series)  <- gsub('^n[.]','',colnames(series))
  series            <- series[(nrow(series)-span):nrow(series),-1]
  
  options(stringsAsFactors = oldstasf)
  
  series[is.na(series)] <- 0
  
  # Class
  class(series) <- c('tw_Class_ts',class(series))
  attributes(series) <- c(attributes(series),units=units)
  return(series)
}

#' Internal use
#' @author George G. Vega Yon
#' @export
.tw_format_unit <- function(obj) {
  units <- attributes(obj)$units
  if (units == 'days') return('%Y-%m-%d')
  if (units %in% c('hours','minutes','secs'))
    return('%a %b %d %T +0000 %Y')
}


# load('data/senate_tweets_example.RData')
# x <- tw_extract(senate_tweets$text)
# 
# Graph
# z <- tw_timeseries(x$mention,created_at = senate_tweets$created_at, units = 'days',span = 60)
# x <- plot(z,rangeSelector=TRUE) 
# library(dygraphs)
# dygraph(z,main = 'Number of daily tweets',width = 600,height = 300)

#' Creates a table
#' @param obj Some text
#' @author George G. Vega Yon
#' @export
tw_table <- function(obj) {
  obj <- unlist(obj,recursive=TRUE)
  obj <- as.data.frame(table(obj),responseName='n', stringsAsFactors=FALSE)
  obj <- obj[order(-obj$n),]
  
  # Setting the class
  class(obj) <- c('tw_Class_table',class(obj))
  
  obj
}
