#' Generate twitter tokens
#' @param appname See \code{\link{oauth_app}}
#' @param key See \code{\link{oauth_app}}.
#' @param secret See \code{\link{oauth_app}}
#' @return A Token1.0 reference class (RC) object.
#' @examples 
#' \dontrun{
#' tokens <- tw_gen_token('mytwitterapp','xxxxxxxx','yyyyyyyy')
#' tw_api_get_usr_profile('gvegayon',tokens)
#' }
#' @export
tw_gen_token <- function(appname,key,secret=NULL) {
  myapp <- oauth_app(appname,key,secret)
  oauth1.0_token(oauth_endpoints('twitter'),myapp)
}

#' Wait in minutes to reconnect to the API
#' @param minutes Number of minutes to wait
#' @details Internal use only (it's just a timer...)
#' @return Void
#' @export
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

#' @title Make a call to the twitter API
#' @param query URL inlcuding the parameters
#' @param minutes Argument passed to tw_api_wait
#' @param ... Additional arguments passed to GET
#' @return A response class from the httr package (can be parsed with -content-)
#' @export
.tw_api_get <- function(query,twitter_token,minutes,...) {
  status <- 0
  while (status!=200) {
    # Making the call
    req <- GET(query, config(token=twitter_token),...)
    status <- status_code(req)
    if (status!=200) print(req)
    if (status==429) {
      message('(429) Too Many Requests: Returned in API v1.1 when a request cannot be served due to the application\'s rate limit having been exhausted for the resource. See Rate Limiting in API v1.1.')
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

#' Get user information
#' @param usr User screen name
#' @param twitter_token Token
#' @param quietly Whether to show the 'success' message or not
#' @param ... Additional arguments passed to \code{\link{GET}}
#' @details Using the twitter api, get information about a twitter account
#' @return A data.frame with info of the usr.
#' @examples
#' \dontrun{
#' tw_api_get_usr_profile('gvegayon')
#' }
#' @export
tw_api_get_usr_profile <- function(usr,twitter_token,quietly=FALSE,...) { 
  if (is.na(usr)) return(NULL)
  else 
    req <- .tw_api_get(
      paste0(
        "https://api.twitter.com/1.1/users/show.json?screen_name=",usr),
      twitter_token,
      5,...)
  
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
  
  if (!quietly) message('Success, info of user ',usr,' correctly retrieved')
  return(req)
}

#' Gets status updates (tweets) from a given user
#' @param usr screen_name of the user
#' @param twitter_token Token
#' @param count Number of statuses to get 
#' @param quietly Whether or not to show the 'success' message
#' @param ... Additional arguments passed to \code{\link{GET}}
#' @return A data.frame with tweets
#' @export
tw_api_get_timeline <- function(usr,twitter_token,count=100,quietly=FALSE,...) {
  usr <- gsub("^@","",usr)
  
  # API CALL
  req <- .tw_api_get(
    paste0(
      "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=",usr,
      "&count=",count,'&include_entities=false'),
    twitter_token,
    5,...
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
  
  # Processing the data a litthe bit
  req <- as.data.frame(bind_rows(req))
  req$source_name <- str_extract(req$source,'(?<=">).+(?=</a)')
  
  if (!quietly) message('Success, timeline of user ',usr,' correctly retrieved')
  return(req)
}

#' Converts a data.frame into JSON
#' @param d A data frame
#' @return A Char string as JSON format
#' @export
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

#' Writes a JSON graph to be used with d3js
#' @param graph A \code{tw_Class_graph} class object (See \code{\link{tw_network}})
#' @export
tw_write_json_network <- function(graph) {
  
  if (!inherits(graph, 'tw_Class_graph')) 
    stop('The graph must be tw_Class_graph object (output of tw_')
  
  nodes <- .tw_df_to_json(graph$nodes)
  links <- .tw_df_to_json(graph$links)
  paste('{\n\t"nodes":[',nodes,'\t\t],\n\t"links":[',links,']\n}',sep="\n")
}

#' Get codes from the places where trends are availables
#' @param twitter_token Token
#' @param ... Ignored
#' @export
tw_api_trends_available <- function(twitter_token,...) {
  req <- .tw_api_get('https://api.twitter.com/1.1/trends/available.json',
                     twitter_token,minutes = 5,...)
  
  # Checking if everything went fine
  if (is.null(req)) return(NULL)
  else if (class(req)=='response')
    if (status_code(req)!=200) return(NULL)
  
  req <- content(req)
  
  req <- lapply(req, function(x) {
    country     <- x$country
    countryCode <- x$countryCode
    
    data.frame(
      stringsAsFactors=FALSE,
      name           = x$name,
      placeType_code = x$placeType$code,
      placeType_name = x$placeType$name,
      url            = x$url,
      parentid       = x$parentid,
      country        = ifelse(country=="",NA,country),
      woeid          = x$woeid
    )
  })
  
  return(do.call(rbind,req))
}

#' Gets the trends for a given place
#' @param id Id of the place
#' @param twitter_token Token
#' @param exclude Whether to exclude hashtags or not
#' @param ... Ignored
#' @export
tw_api_get_trends_place <- function(id,twitter_token,exclude=FALSE,...) {
  
  # Making the request
  req <- .tw_api_get(
    paste0('https://api.twitter.com/1.1/trends/place.json?id=',id,
           ifelse(exclude,'&exclude=hashtags','')),twitter_token, 5)
  
  if (is.null(req)) return(NULL)
  else if (class(req)=='response')
    if (status_code(req)!=200) return(NULL)
  
  req <- content(req)[[1]]
  
  # Processing the data
  trends <- lapply(req$trends, function(x) {
    prom <- x$promoted_content
    data.frame(
      stringsAsFactors = FALSE,
      name             = x$name,
      query            = x$query,
      url              = x$url,
      promoted_content = ifelse(is.null(prom),NA,prom)
    )
  })
  
  trends <- bind_rows(trends)
  attributes(trends) <- unlist(req[[-1]],recursive = TRUE)
  
  return(trends)
}

#' Gets a sample through the sample API
#' @param twitter_token Token
#' @param Timeout Number of seconds
#' @param ... Additional parameters to be passed to \code{\link{GET}}
#' @export
tw_api_get_statuses_sample <- function(twitter_token,Timeout=60,...) {
  
  # Parameters
  .start_time<-Sys.time()
  tweets <- rawConnection(raw(0), 'r+')
  
  req <- tryCatch(GET('https://stream.twitter.com/1.1/statuses/sample.json',
                      config(token=twitter_token),
                      write_stream(function(x) {
                        tdif <- as.numeric(difftime(Sys.time(),.start_time,units = 'secs'))
                        if (tdif > Timeout) stop()
                        writeBin(x,tweets)
                        length(x)
                      }),...))
  # message('ok')
  con <- tempfile()
  on.exit(close(con))
  writeBin(tweets,con)
  close(tweets)
  return(readLines(con))
}
# x <- tw_api_get_statuses_sample(5)
# con <- rawConnection(raw(0),'r+')
# # GET("https://jeroenooms.github.io/data/diamonds.json",
# GET('https://stream.twitter.com/1.1/statuses/sample.json?delimited=length',
#     config(token=twitter_token),
#     write_stream(function(x) {
#       if (exists('.N',where = .GlobalEnv)) {
#         assign('.N',.N+1,.GlobalEnv)
#         if (.N>10) {
#           rm(.N,envir = .GlobalEnv)
#           stop('Worked!')
#         }
#       }
#       else assign('.N', 1, .GlobalEnv)
#       # tweets <- strsplit(tweets,split="\\s+")
#       writeBin(x,con)
#       length(x)
#     })
# )
# writeBin(
#   rawConnectionValue(con),
#   paste0('data/streaming/',sprintf('%04.0g',1),'.raw'))
# 
# close(con)

# Designing class
# - Hasthtag table
# - Accounts table
# - Tweets table (most retweeted)
# - statuses update time
# - Most frequent URLs

# load("data/lovewins.RData")
# load("data/tweets_congress.RData")
# x <- tw_extract(tweets_congress$text)
# conv <- tw_network(tweets_congress$screen_name,lapply(x,"[[","mention"))
# conv2 <- tw_network(tweets_congress$screen_name,lapply(x,"[[","mention"),onlyFrom = TRUE)
# mygraph <- write.gexf(conv$nodes,conv$edges,keepFactors = TRUE)
# tw_table(unlist(lapply(x,"[[","hashtag"),recursive = TRUE))
