rm(list=ls())
library(stringr)
library(rgexf)
library(dplyr)

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
# load("data/lovewins.RData")
# load("data/tweets_congress.RData")
# x <- tw_extract(tweets_congress$text)
# conv <- tw_conversation(tweets_congress$screen_name,lapply(x,"[[","mention"))
# conv2 <- tw_conversation(tweets_congress$screen_name,lapply(x,"[[","mention"),onlyFrom = TRUE)
# mygraph <- write.gexf(conv$nodes,conv$edges,keepFactors = TRUE)
# tw_table(unlist(lapply(x,"[[","hashtag"),recursive = TRUE))

#' @description  Function to enter the website and get the twitter account
tw_get_tw_account <- function(uri, redirect=TRUE) {
  web <- getURL(uri,followlocation=TRUE)
  if (redirect) {
    if (str_detect(web,'meta\\s+http-equiv="refresh"\\s+content')) {
      uri <- str_replace(str_extract(web,'(?<=(url|URL)\\="?).*'),'".*',"")
      message('\tGoing deeper, visiting ',uri)
      return(tw_get_tw_account(uri))
    }
  }
  accounts <- str_extract_all(web,'https?://(www\\.)?twitter.com/(#!/)?[a-zA-Z0-9_]+(?=">?)')[[1]]
  
  # Normalizing the twitter accounts (note that screen names are not case
  # sensitive)
  accounts <- str_extract(accounts, "[a-zA-Z0-9_]+$")
  accounts <- unique(tolower(accounts))
  
  return(accounts)
}

#' @title Wait in minutes to reconnect to the API
tw_api_wait <- function(minutes=1) {
  nbars <- 60
  secs  <- minutes*60/nbars
  
  message('Trying to reconnect in ',minutes,' minutes')
  message(rep('-',nbars))
  for (i in 1:nbars) {
    Sys.sleep(secs)
    message('.',appendLF = FALSE)
  }
  message('done\nTrying again...')
}

#' @title Get user information
tw_api_get_usr_profile <- function(x,...) { 
  if (is.na(x)) return(NULL)
  else { 
    # Query
    query <- paste0(
      "https://api.twitter.com/1.1/users/show.json?screen_name=",x)
    
    status <- 0
    while (status!=200) {
      # Making the call
      req <- GET(query, config(token=twitter_token))
      status <- status_code(req)
      if (status!=200) print(req)
      if (status==429) {
        message('(409) Too Many Requests: Returned in API v1.1 when a request cannot be served due to the application’s rate limit having been exhausted for the resource. See Rate Limiting in API v1.1.
                will try in 15min (current user ',x,')...')
        tw_api_wait()
      }
      else if (status==401) {
        message("(401) Unauthorized: Authentication credentials were missing or incorrect. Also returned in other circumstances, for example all calls to API v1 endpoints now return 401 (use API v1.1 instead).")
        status<-200
      }
      else if (status==404) {
        message("(404) Not found: The URI requested is invalid or the resource requested, such as a user, does not exists. Also returned when the requested format is not supported by the requested method.")
        return(NULL)
      }
      else if (status==502) {
        message('(502) Bad Gateway: Twitter is down or being upgraded.
                will try in 15min (current user ',x,')...')
        tw_api_wait(5)
      }
      else if (status!=200) {
        message('Error, see response ',status)
        return(NULL)
      }
    }
  }
  
  # If it works, then process the data
  message("Success, info of user ",x,' correctly retrieved')
  return(content(req))
}

#' @title Remove common accounts
#'
tw_rm_common_accounts <- function(accounts) {
  
}

