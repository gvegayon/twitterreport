#' @description Plot time series
#' @param x a tw_Class_ts class object
#' @param rangeSelector Wheather or not to include a time range selector
#' @param main See dygraph
#' @param xlab See dygraph
#' @param ylab See dygraph
#' @param group See dygraph
#' @param with See dygraph
#' @param height See dygraph
#' @param ... Further arguments to be passed to dygraph
plot.tw_Class_ts <- function(
  x,rangeSelector=TRUE,main='Number of daily tweets',
  xlab=NULL,ylab=NULL,group=NULL,width = 600,height = 400,...) {
  
  # Plotting graph
  graph<-dygraph(x,main,xlab,ylab,group,width,height,...)
  if (rangeSelector) dyRangeSelector(graph)
  else graph
}