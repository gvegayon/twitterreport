#' @title Creates conversation graph (directed)
#' @param source Vector of screen_name
#' @param target List of vectors of mentions (output from tw_extract)
#' @param group Data frame with two columns: name & group
tw_conversation <- function(source,target,onlyFrom=FALSE,excludeSelf=TRUE,minInteract=1,
                            group=NULL) {
  
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
  if (excludeSelf) tmp <- subset(tmp,subset=source!=target)
  
  # Frequency
  tmp <- group_by(tmp, source, target)
  tmp <- as.data.frame(summarise(tmp,value=n()))
  
  # Filtering interactions
  tmp <- subset(tmp,subset=value>=minInteract)
  
  # Encoding links
  ne <- nrow(tmp)
  tmp2 <- as.factor(c(tmp$source,tmp$target))
  links <- data.frame(source=tmp2[1:ne],target=tmp2[(ne+1):(ne*2)],value=tmp$value)
  nodes <- unique(unlist(links[,-3]))
  
  nodes <- data.frame(id=as.numeric(nodes)-1,name=as.character(nodes))
  nodes <- nodes[order(nodes$id),]
  
  # If there is grouping
  if (length(group)) {
    nodes <- merge(nodes,group)
  }
  
  # Returning output
  links$source <- as.numeric(links$source)-1
  links$target <- as.numeric(links$target)-1
  out <- list(nodes=nodes,links=links)
  
  options(stringsAsFactors = oldstasf)
  return(out)
}