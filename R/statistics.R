#' Creates a time series object with frecuencies 
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
  
  grp <- group_by_(data, ~created_at, ~obj)
  tmp <- summarise_(grp, .dots=setNames(list(~n()),'n'))
  tmp <- tmp[order(-tmp$n),]
  
  # Getting the top k elements
  grp <- group_by_(tmp, ~obj)
  who <- as.data.frame(summarise_(grp, .dots=setNames(list(~sum(n)),'n')))
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

# Internal use
# @author George G. Vega Yon
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

#' Creates a table of frequencies
#' 
#' See \code{\link{tw_extract}}
#' 
#' @param x An object of class \code{tw_Class_extract}.
#' @param obj A character indicating 
#' @param ... Further arguments to be passed to the method
#' @author George G. Vega Yon
#' @examples 
#' # Loading tweets
#' data(senate_tweets)
#' head(senate_tweets$text)
#' 
#' # Extracting elements and creating tables
#' x <- tw_extract(senate_tweets$text)
#' 
#' head(tw_table(x,'mention'))
#' @export
tw_table <- function(x,...) UseMethod('tw_table')

#' @describeIn tw_table Makes a table out of the output of \code{\link{tw_extract}}
#' @export
tw_table.tw_Class_extract <- function(x, obj=c("email", "mention", "hashtag", "url"),...) {
  
  # Checking if the obj is well specified
  original <- c("email", "mention", "hashtag", "url")
  test <- which(!(obj %in% original))
  if (any(test))
    stop('-obj- object list badly specified, should be any of ',
         paste0('\'',original,'\'',collapse=', '),'.')
  
  if (length(obj)>1) obj <- 'hashtag'
  
  obj <- unlist(x[[obj]],recursive=TRUE)
  obj <- as.data.frame(table(obj),responseName='n', stringsAsFactors=FALSE)
  obj <- obj[order(-obj$n),]
  
  rownames(obj) <- 1:nrow(obj)
  
  # Setting the class
  class(obj) <- c('tw_Class_table',class(obj))
  
  obj
}




