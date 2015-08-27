#' Plot time series
#' @param x a tw_Class_ts class object
#' @param rangeSelector Wheather or not to include a time range selector
#' @param main See dygraph
#' @param xlab See dygraph
#' @param ylab See dygraph
#' @param group See dygraph
#' @param width See dygraph
#' @param height See dygraph
#' @param ... Further arguments to be passed to dygraph
#' @author George G. Vega Yon
#' @export
plot.tw_Class_ts <- function(
  x,rangeSelector=TRUE,main='Number of daily tweets',
  xlab=NULL,ylab=NULL,group=NULL,width = 600,height = 400,...) {
  
  # Plotting graph
  graph<-dygraph(x,main,xlab,ylab,group,width,height,...)
  if (rangeSelector) dyRangeSelector(graph)
  else graph
}

#' Plot a table
#' @param x An object of class \code{tw_Class_table}
#' @param y Ignored
#' @param nentries Number of rows to include in the table
#' @param caption Title of the table
#' @param options Options to be passed to \code{\link{datatable}}
#' @param ... Additional parameters to be passed to \code{\link{datatable}}
#' @export
plot.tw_Class_table <- function(
  x, y=NULL, nentries=20, caption='Most popular hashtags',
  options=list(pageLength=5),...) {
  datatable(x[1:nentries,], options = options,rownames = FALSE,...)
}

# elements <- tw_extract(senate_tweets$text)
# x <- tw_table(elements$mention)
# head(x)
# plot(x)

#' Plot words
#' @param x A \code{tw_Class_words} object
#' @param y Ignored
#' @param scale See \code{\link{wordcloud}}
#' @param min.freq See \code{\link{wordcloud}}
#' @param max.n.words Max number of words to include
#' @param colors See \code{\link{wordcloud}}
#' @param ... Further arguments to be passed to \code{\link{wordcloud}}
#' @export
plot.tw_Class_words <- function(
  x,y=NULL,scale=c(4,.5),min.freq=100,max.n.words=100,
  colors=brewer.pal(min(c(9,max.n.words)),'Blues'),
  ...) {
  
  # Creating data table
  x <- as.data.frame(table(unlist(x,recursive = TRUE)),responseName = 'n',
                     stringsAsFactors = FALSE)
  x <- x[order(-x$n),] 
  
  # Plotting
  max.n.words <- min(c(max.n.words,nrow(x)))
  with(x[1:max.n.words,],
       wordcloud(Var1,n,scale,min.freq, 
                 colors = colors,...)
  ) 
  
  # return data table
  invisible(x)
} 

# load('data/senate_tweets_example.RData')
# words <- tw_words(senate_tweets$text)
# head(words)
# tab <- plot(words)
# head(tab,20) 

#' Plots a network using D3js
#' @param x A tw_Class_graph object (see \code{\link{tw_network}})
#' @param y Ignored
#' @param nodelabel Name of the column that containts the name
#' @param opacity See \code{\link{forceNetwork}}
#' @param opacityNoHover Idem
#' @param legend Whether to include a legend or not
#' @param ... Further arguments to pass to forceNetwork
#' @return A D3js graph
#' @export
plot.tw_Class_graph <- function(
  x,y=NULL,
  nodelabel='name',
  opacity = .9,
  opacityNoHover = .5, legend = TRUE,
  ...) {
  
  forceNetwork(
    Links = x$links,
    Nodes = x$nodes, 
    Source="source", Target="target",
    Value="value",NodeID=nodelabel, Group="group",
    opacity = opacity,
    opacityNoHover = opacityNoHover, legend = legend,...)
}

#' @export
#' @method print tw_Class_jaccard
print.tw_Class_jaccard <- function(x, ...) {
  n <- x$nwords
  cat("Jaccard index Matrix (Sparse) of ",n,"x",n,"\n",sep="")
  cat("contains the following words:\n")
  print(head(x$words))
  invisible(x)
}
