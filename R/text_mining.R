# rm(list=ls())
library(stringr)
library(rgexf)
library(dplyr)
library(RCurl)
library(XML)
source("R/verify.R")

#' @title Extract info from tweets
#' @aliases 
#' @description Extract email accounts, mentions, hashtags and urls from tweets
#' @param txt Character
#' @param obj List of objects to extract
#' @param normalize bool whether or not to normalize emails, hashtags and mentions (to lower)
#' @return List
#' @examples  
#' head(tw_extract(tweets$text))
#' #lapply(x,"[[","mention")
tw_extract <- function(txt, obj = c("email", "mention", "hashtag", "url"),
                       normalize=TRUE) {
  if (length(txt)>1) output <- lapply(txt, tw_extracti, obj, normalize)
  else output <- tw_extracti(txt,obj,normalize)
  return(output)
}

#' @describeIn tw_extract
tw_extracti <- function(txt, obj = c("email", "mention", "hashtag", "url"),
                        normalize=TRUE) {
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
  
  if (normalize) {
    output$mention <- str_to_lower(output$mention)
    output$hashtag <- str_to_lower(output$hashtag)
    output$email <- str_to_lower(output$email)
  }
  
  return(output)
}

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

.tw_api_get <- function(query,minutes,...) {
  status <- 0
  while (status!=200) {
    # Making the call
    req <- GET(query, config(token=twitter_token))
    status <- status_code(req)
    if (status!=200) print(req)
    if (status==429) {
      message('(429) Too Many Requests: Returned in API v1.1 when a request cannot be served due to the application’s rate limit having been exhausted for the resource. See Rate Limiting in API v1.1.')
      tw_api_wait(minutes)
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
      message('(502) Bad Gateway: Twitter is down or being upgraded.')
      tw_api_wait(minutes)
    }
    else if (status!=200) {
      message('Error, see response ',status)
      return(NULL)
    }
  }
  return(req)
}

#' @title Get user information
tw_api_get_usr_profile <- function(usr,...) { 
  if (is.na(usr)) return(NULL)
  else 
    req <- .tw_api_get(
      paste0(
        "https://api.twitter.com/1.1/users/show.json?screen_name=",usr),
      minutes=5)
  
  # Checking if everything went fine
  if (is.null(req)) return(NULL)
  else if (class(req)=='response')
    if (status_code(req)!=200) return(NULL)
  
  # If it works, then process the data
  req <- content(req)

  # Nullable elements
  time_zone   <- req$time_zone
  utc_offset  <- req$utc_offset
  description <- req$description
  location    <- req$location
  
  req <- data.frame(
    stringsAsFactors = FALSE,
    id                      = req$id,
    name                    = req$name,
    screen_name             = req$screen_name,
    contributors_enabled    = req$contributors_enabled,
    created_at              = strptime(req$created_at,'%a %b %d %T +0000 %Y'),
    default_profile         = req$default_profile,
    default_profile_image   = req$default_profile_image,
    description             = ifelse(is.null(description),NA,description),
    favourites_count        = req$favourites_count,
    followers_count         = req$followers_count,
    friends_count           = req$friends_count,
    geo_enabled             = req$geo_enabled,
    is_translator           = req$is_translator,
    lang                    = req$lang,
    listed_count            = req$listed_count,
    location                = ifelse(is.null(location),NA,location),
    profile_image_url       = req$profile_image_url,
    profile_image_url_https = req$profile_image_url_https,
    protected               = req$protected,
    statuses_count          = req$statuses_count,
    time_zone               = ifelse(is.null(time_zone),NA,time_zone),
    utc_offset              = ifelse(is.null(utc_offset),NA,utc_offset),
    verified                = req$verified
    )
  
  # Setting the proper class
  # Source https://dev.twitter.com/overview/api/users
#   var <- c('contributors_enabled','default_profile_image','default_profile',
#                    'geo_enabled','is_translator','notifications','protected','verified')
#   req[,var] <- as.logical(req[,var])
  # req$created_at <- strptime(req$created_at,'%a %b %d %T +0000 %Y')
  
  message('Success, info of user ',usr,' correctly retrieved')
  return(req)
}

#' @description Gets tweets from a user up to 1000 statuses
tw_api_get_timeline <- function(usr,count=100,...) {
  usr <- gsub("^@","",usr)
  
  # API CALL
  req <- .tw_api_get(
    paste0(
    "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=",usr,
    "&count=",count,'&include_entities=false'),
    minutes = 5
    )
  
  # Checking if everything went fine
  if (is.null(req)) return(NULL)
  else if (class(req)=='response')
    if (status_code(req)!=200) return(NULL)
  
  # If it works, then process the data
  req <- content(req)
  
  req <- lapply(req, function(x,...) {
    # Nullable characters
    coords  <- paste0(x$coordinates$coordinates,collapse=":")
    replyto <- x$in_reply_to_screen_name
    nfav    <- x$favorite_count
    isfav   <- x$favorited
    data.frame(
      screen_name             = x$user$screen_name, 
      in_reply_to_screen_name = ifelse(is.null(replyto),NA,replyto),
      user_id                 = x$user$id,
      created_at              = strptime(x$created_at,'%a %b %d %T +0000 %Y'),
      id                      = x$id,
      text                    = x$text, 
      source                  = x$source,
      truncated               = x$truncated,
      retweet_count           = x$retweet_count,
      favorite_count          = ifelse(is.null(nfav),NA,nfav),
      favorited               = ifelse(is.null(isfav),FALSE,isfav),
      retweeted               = x$retweeted,
      coordinates             = ifelse(coords=='',NA,coords),
      stringsAsFactors = FALSE
    )
  })
  message('Success, timeline of user ',usr,' correctly retrieved')
  return(as.data.frame(bind_rows(req)))
}


.tw_df_to_json <- function(d) {
  vnames <- colnames(d)
  for (i in 1:ncol(d)) {
    if (class(d[,i])=="character") d[,i] <- paste0('"',d[,i],'"')
  }
  f <- lapply(1:nrow(d), function(x,...) {
    paste0('\t\t{"',paste(vnames,d[x,],sep='":',collapse=',"'),'}')
  })
  paste0(f,collapse=',\n')
}

#' @description Writes a JSON graph to be used with d3js
tw_write_json_network <- function(graph) {
  nodes <- .tw_df_to_json(graph$nodes)
  links <- .tw_df_to_json(graph$links)
  paste('{\n\t"nodes":[',nodes,'\t\t],\n\t"links":[',links,']\n}',sep="\n")
}

# Designing class
# - Hasthtag table
# - Accounts table
# - Tweets table (most retweeted)
# - statuses update time
# - Most frequent URLs

# load("data/lovewins.RData")
# load("data/tweets_congress.RData")
# x <- tw_extract(tweets_congress$text)
# conv <- tw_conversation(tweets_congress$screen_name,lapply(x,"[[","mention"))
# conv2 <- tw_conversation(tweets_congress$screen_name,lapply(x,"[[","mention"),onlyFrom = TRUE)
# mygraph <- write.gexf(conv$nodes,conv$edges,keepFactors = TRUE)
# tw_table(unlist(lapply(x,"[[","hashtag"),recursive = TRUE))
