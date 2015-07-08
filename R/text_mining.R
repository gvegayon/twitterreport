rm(list=ls())
library(stringr)
library(rgexf)
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
tw_conversation <- function(from,to,reduce=TRUE) {
  n <- length(from)

  # Create edges
  tmp <- do.call(rbind,lapply(1:n, function(i,...) {
    cbind(from=rep(from[[i]],length(to[[i]])),to=to[[i]])
  }))
  
  # Encoding edges
  ne <- nrow(tmp)
  tmp <- as.factor(c(tmp[,1],tmp[,2]))
  edges <- data.frame(from=tmp[1:(ne/2)],to=tmp[(ne/2+1):ne])
  nodes <- unique(unlist(edges))
  
  # Returning output
  list(
    edges=edges,
    nodes=data.frame(id=as.numeric(nodes),label=as.character(nodes),
                     stringsAsFactors = FALSE)
    )
}
# x <- tw_extract(tweets$text)
# conv <- tw_conversation(tweets$screen_name,lapply(x,"[[","mention"))
# mygraph <- write.gexf(conv$nodes,conv$edges,keepFactors = TRUE)

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
mygraph <- write.gexf(conv$nodes,conv$edges,keepFactors = TRUE)
tw_table(unlist(lapply(x,"[[","hashtag"),recursive = TRUE))
