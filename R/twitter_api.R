#' @title Wait in minutes to reconnect to the API
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
#' @return A response class from the httr package (can be parsed with -content-)
#' @export
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

#' Get user information
#' @param usr User screen name
#' @details Using the twitter api, get information about a twitter account
#' @return A data.frame with info of the usr.
#' @examples
#' \dontrun{
#' tw_api_get_usr_profile('gvegayon')
#' }
#' @export
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

#' @description Gets status updates (tweets) from a given user
#' @param usr screen_name of the user
#' @param count Number of statuses to get 
#' @return A data.frame with tweets
#' @export
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
  
  # Processing the data a litthe bit
  req <- as.data.frame(bind_rows(req))
  req$source_name <- str_extract(req$source,'(?<=">).+(?=</a)')
  
  message('Success, timeline of user ',usr,' correctly retrieved')
  return(req)
}

#' @description Converts a data.frame into JSON
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
#' @param graph 
tw_write_json_network <- function(graph) {
  
  if (!inherits(graph, 'tw_Class_graph')) 
    stop('The graph must be tw_Class_graph object (output of tw_')
  
  nodes <- .tw_df_to_json(graph$nodes)
  links <- .tw_df_to_json(graph$links)
  paste('{\n\t"nodes":[',nodes,'\t\t],\n\t"links":[',links,']\n}',sep="\n")
}

#' Get codes from the places where trends are availables
#' @param ... Ignored
#' @export
tw_api_trends_available <- function(...) {
  req <- .tw_api_get('https://api.twitter.com/1.1/trends/available.json',minutes = 5)
  
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

#' @description Gets 
#' @param id Id of the place
#' @export
tw_api_get_trends_place <- function(id,exclude=FALSE,...) {
  
  # Making the request
  req <- .tw_api_get(
    paste0('https://api.twitter.com/1.1/trends/place.json?id=',id,
           ifelse(exclude,'&exclude=hashtags','')), 5)
  
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
#' @param Timeout Number of seconds
#' @export
tw_api_get_statuses_sample <- function(Timeout=60,...) {
  
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
                      })))
  message('ok')
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
