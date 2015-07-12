rm(list=ls())
library(stringr)
library(rgexf)
library(dplyr)

load("data/lovewins.RData")
load("data/tweets_congress.RData")

#' @title Extract info from tweets
#' @aliases 
#' @description Extract email accounts, mentions, hashtags and urls from tweets
#' @param txt Character
#' @param obj List of objects to extract
#' @return List
#' @examples  
#' head(tw_extract(tweets$text))
#' #lapply(x,"[[","mention")
tw_extract <- function(txt, obj = c("email", "mention", "hashtag", "url")) {
  if (length(txt)>1) output <- lapply(txt, tw_extracti, obj)
  else output <- tw_extrati(txt,obj)
  return(output)
}

#' @describeIn tw_extract
tw_extracti <- function(txt, obj = c("email", "mention", "hashtag", "url")) {
  # patterns
  p.email <- "([a-zA-Z0-9_]-?\\.?)+@([a-zA-Z0-9_]-?)+\\.[a-zA-Z]+"
  p.hashtag <- "#[[:graph:]]+"
  p.mention <- "@[a-zA-Z0-9_]+"
  p.url <- "https?[:]//[[:graph:]]+"
  
  output <- as.list(obj)
  names(output) <- obj
  for (i in obj) {
    
    # Capturing the object
    pattern <- get(paste0("p.", i, sep=""))
    
    output[[i]] <- str_extract_all(txt, pattern)[[1]]
  }
  if ("mention" %in% obj) 
    output$mention <- str_replace_all(output$mention,"^@","")
  else if ("hashtag" %in% obj)
    output$mention <- str_replace_all(output$hashtag,"^#","")
  
  return(output)
}

#' @title Creates conversation graph (directed)
#' @param from Vector of screen_name
#' @param to List of vectors of mentions (output from tw_extract)
tw_conversation <- function(from,to,onlyFrom=FALSE) {
  oldstasf <- options()$stringsAsFactors
  options(stringsAsFactors = FALSE)
  n <- length(from)

  # Create edges
  tmp <- as.data.frame(do.call(rbind,lapply(1:n, function(i,...) {
    cbind(from=rep(from[[i]],length(to[[i]])),to=to[[i]])
  })))

  # Frequency
  tmp <- group_by(tmp, from, to)
  tmp <- as.data.frame(summarise(tmp,n=n()))

  # Encoding edges
  ne <- nrow(tmp)
  tmp2 <- as.factor(c(tmp$from,tmp$to))
  edges <- data.frame(from=tmp2[1:ne],to=tmp2[(ne+1):(ne*2)],n=tmp$n)
  nodes <- unique(unlist(edges[,-3]))

  original <- unique(as.numeric(edges$from))
  
  # Returning output
  out <- list(
    edges=edges,
    nodes=data.frame(id=as.numeric(nodes),label=as.character(nodes))
    )
  
  # Reducing edges list
  if (onlyFrom) {
    out$nodes <- out$nodes[which(out$nodes$id %in% original),]
    out$edges <- out$edges[which(as.numeric(out$edges$to) %in% original),]
  }
  options(stringsAsFactors = oldstasf)
  return(out)
}
# x <- tw_extract(tweets$text)
# conv <- tw_conversation(tweets$screen_name,lapply(x,"[[","mention"))
# mygraph <- write.gexf(conv2$nodes,conv2$edges[,-3],keepFactors = TRUE)

#' @title Create table (and plot graph bar if needed)
tw_table <- function(txt) {
  # Cleaning text
  txt <- tolower(txt)
  words <- as.data.frame(table(txt))
  words <- words[order(-words$Freq),]
  words
}
x <- tw_extract(tweets_congress$text)
conv <- tw_conversation(tweets_congress$screen_name,lapply(x,"[[","mention"))
conv2 <- tw_conversation(tweets_congress$screen_name,lapply(x,"[[","mention"),onlyFrom = TRUE)
mygraph <- write.gexf(conv$nodes,conv$edges,keepFactors = TRUE)
tw_table(unlist(lapply(x,"[[","hashtag"),recursive = TRUE))
