#' Creates graph (directed)
#' 
#' Using the output of [tw_extract()] and the author of the message,
#' creates a graph
#' 
#' @param source Vector of *screen_name*
#' @param target List of vectors of *mentions* (output from `tw_extract`)
#' @param only.from Whether to filter the links to those only where 
#' source and target are in the `source` vector
#' @param exclude.self Whether to exclude self-links
#' @param min.interact Minimun number of interactions to consider (links
#' below this number will be excluded)
#' @param group Data frame with two columns: name & group
#' @param size A data frame with two columns: name & size
#' @param ignore.case When `TRUE` converts all of `source` and `target`
#' to lower-case.
#' @author George G. Vega Yon
#' @return A two-element list containing two data.frames, nodes and links of
#' class `tw_Class_graph` (to be used with [plot.tw_Class_graph()].
#' The nodes data.frame includes two columns, `id`, `name` and
#' `group`. The links data.frame includes three columns, `source`,
#' `target` and `value`.
#' 
#' @details The `value` column in the `links` dataframe (see \strong{Value})
#' is computed as the number of connexions between the source and the target.
#' @family network functions
#' @export
#' @examples 
#' \dontrun{
#' # Loading sample data and retrieving mentions
#' data(senate_tweets)
#' mentions <- tw_extract(senate_tweets$text, obj="mention")$mention
#' 
#' # Preparing data for size. Here we are just setting a random size for
#' # each vertex.
#' usrs<- tolower(senate_tweets$screen_name)
#' size <- data.frame(name=unique(usrs),
#'                    size=exp(runif(length(unique(usrs)))*5))
#' 
#' # Creating the graph
#' graph <- tw_network(
#'   usrs, mentions, min.interact = 5, size=size)
#' 
#' # Visualizing the graph
#' plot(graph)
#' }

tw_network <- function(
  source,target,only.from=FALSE,exclude.self=TRUE,min.interact=1, group=NULL,
  size=NULL, ignore.case=TRUE) {
  
  # Analyzing vertex size
#   if (nrow(size) && nrow(size) != length(unique(source)))
#     stop("-source- and -size- differ in lengths.")
  
  # Old stringAsFactors
  oldstasf <- options()$stringsAsFactors
  options(stringsAsFactors = FALSE)
  n <- length(source)
  
  # Checking Case
  if (ignore.case) {
    source <- tolower(source)
    target <- lapply(target, tolower)
  }
  
  # Reducing edges list
  if (only.from) {
    original <- unique(source)
    target <- lapply(target, function(x) x[which(x %in% original)])
  }
  
  # Create links
  tmp <- as.data.frame(do.call(rbind,lapply(1:n, function(i,...) {
    cbind(source=rep(source[[i]],length(target[[i]])),target=target[[i]])
  })))
  
  # If excludes self
  if (exclude.self) tmp <- subset(tmp,source!=target)
  
  # Frequency
  tmp <- group_by_(tmp, ~source, ~target)
  tmp <- as.data.frame(summarise_(tmp,.dots=setNames(list(~n()),'value')))
  
  # Filtering interactions
  tmp <- tmp[tmp$value>=min.interact,]
  
  # Encoding links
  ne <- nrow(tmp)
  tmp2 <- as.factor(c(tmp$source,tmp$target))
  links <- data.frame(source=tmp2[1:ne],target=tmp2[(ne+1):(ne*2)],value=tmp[['value']])
  nodes <- unique(unlist(links[,-3]))
  
  nodes <- data.frame(id=as.numeric(nodes)-1,name=as.character(nodes))
  
  # Merging with vertices sizes
  if (length(size)) {
    suppressMessages(nodes <- left_join(nodes, size))
    test <- is.na(nodes$size)
    if (any(test)) {
      warning("Some vertices where not in the -size- data.frame\n",
              "The minimum size has been inputted.")
      nodes$size[test] <- min(nodes$size, na.rm=TRUE)
    }
  }
  else 
    nodes$size <- 1
    
  nodes <- nodes[order(nodes$id),]
  
  # If there is grouping
  if (length(group)) {
    if (ignore.case) group$name <- tolower(group$name)
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
