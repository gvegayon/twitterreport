#' @title Creates conversation graph (directed)
#' @param source Vector of screen_name
#' @param target List of vectors of mentions (output from tw_extract)
#' @param onlyFrom Whether to fileter the links to those only where 
#' source and target are in the \code{source} vector
#' @param excludeSelf Whether to exclude selflinks
#' @param minInteract Minimun number of interactions to consider (links
#' below this number will be excluded
#' @param group Data frame with two columns: name & group
#' @param size Character name of the size variable
#' @author George G. Vega Yon
#' @export
tw_network <- function(
  source,target,onlyFrom=FALSE,excludeSelf=TRUE,minInteract=1, group=NULL,
  size=NULL) {
  
  # Old stringAsFactors
  oldstasf <- options()$stringsAsFactors
  options(stringsAsFactors = FALSE)
  n <- length(source)
  
  # Reducing edges list
  if (onlyFrom) {
    original <- unique(source)
    target <- lapply(target, function(x) x[which(x %in% original)])
  }
  
  # Create links
  tmp <- as.data.frame(do.call(rbind,lapply(1:n, function(i,...) {
    cbind(source=rep(source[[i]],length(target[[i]])),target=target[[i]])
  })))
  
  # If excludes self
  if (excludeSelf) tmp <- subset(tmp,source!=target)
  
  # Frequency
  tmp <- group_by(tmp, source, target)
  tmp <- as.data.frame(summarise(tmp,'value'=n()))
  
  # Filtering interactions
  tmp <- subset(tmp,value>=minInteract)
  
  # Encoding links
  ne <- nrow(tmp)
  tmp2 <- as.factor(c(tmp$source,tmp$target))
  links <- data.frame(source=tmp2[1:ne],target=tmp2[(ne+1):(ne*2)],value=tmp[['value']])
  nodes <- unique(unlist(links[,-3]))
  
  nodes <- data.frame(id=as.numeric(nodes)-1,name=as.character(nodes))
  nodes <- nodes[order(nodes$id),]
  
  # If there is grouping
  if (length(group)) {
    suppressMessages(nodes <- left_join(nodes,group))
  }
  else nodes <- cbind(nodes, group='1')
  
  # Returning output
  links$source <- as.numeric(links$source)-1
  links$target <- as.numeric(links$target)-1
  out <- list(nodes=nodes,links=links)
  class(out) <- c('tw_Class_graph',class(out))
  
  options(stringsAsFactors = oldstasf)
  return(out)
}
