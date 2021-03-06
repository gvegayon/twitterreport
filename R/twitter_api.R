#' Generate twitter tokens
#' 
#' @param appname Character scalar. The name of your twitter app.
#' @param key Character scalar. Your API key provided by twitter.
#' @param secret Character scalar. Your API secret provided by twitter.
#' 
#' @details 
#' This function is a wrapper of [httr::oauth_app()]. To be able to use most of
#' twitter's API service you need to sign-in with your twitter account at
#' https://apps.twitter.com/]
#' 
#' @return A Token1.0 reference class (RC) object.
#' @examples 
#' \dontrun{
#'   tokens <- tw_gen_token('mytwitterapp','xxxxxxxx','yyyyyyyy')
#'   tw_api_get_usr_profile('gvegayon',tokens)
#' }
#' @family API functions
#' @export
tw_gen_token <- function(appname,key,secret=NULL) {
  myapp <- oauth_app(appname,key,secret)
  oauth1.0_token(oauth_endpoints('twitter'), myapp)
}

# Wait in minutes to reconnect to the API
# @param minutes Number of minutes to wait
# @details Internal use only (it's just a timer...)
# @return Void
# @export
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

# @title Make a call to the twitter API
# @param q URL inlcuding the parameters
# @param minutes Argument passed to tw_api_wait
# @param ... Additional arguments passed to GET
# @return A response class from the httr package (can be parsed with -content-)
tw_api_get <- function(q,twitter_token,minutes=15,noisy=FALSE,...) {
  status <- 0
  while (status!=200) {
    # Making the call
    req <- GET(q, config(token=twitter_token),...)
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
#'
#' @param screen_name User screen name
#' @template api
#' @param quietly Whether to show the 'success' message or not
#' @param user_id User id
#' @param include_entities Not used
#' @param ... Additional arguments passed to [GET()]
#'
#' @details Using the twitter api, get information about a twitter account
#' \subsection{From Twitter}{Returns a \href{https://dev.twitter.com/overview/api/users}{variety of information}
#'  about the user specified by the required user_id or screen_name parameter.
#'  The author’s most recent Tweet will be returned inline when possible.}
#' @return A data.frame with info of the usr.
#' \itemize{
#' \item `id`
#' \item `name`
#' \item `screen_name`
#' \item `contributors_enabled`
#' \item `created_at`
#' \item `default_profile`
#' \item `default_profile_image`
#' \item `description`
#' \item `favourites_count`
#' \item `followers_count`
#' \item `friends_count`
#' \item `geo_enabled`
#' \item `is_translator`
#' \item `lang`
#' \item `listed_count`
#' \item `location`
#' \item `profile_image_url`
#' \item `profile_image_url_https`
#' \item `protected`
#' \item `statuses_count`
#' \item `time_zone`
#' \item `utc_offset`
#' \item `verified`
#' }
#' @examples
#' \dontrun{
#' tw_api_get_users_show('gvegayon', mytoken)
#' }
#' @references Twitter REST API (GET users/show)
#' https://dev.twitter.com/rest/reference/get/users/show
#' @export
#' @family API functions
tw_api_get_users_show <- function(screen_name=NULL,twitter_token,quietly=FALSE,
                                  user_id=NULL,include_entities="false",...) {
  
  if (is.null(screen_name) && is.null(user_id)) {
    stop("Must provide either screen_name or user_id.")
  }
  else if (is.na(screen_name) && is.na(user_id)) {
    warning("Both screen_name and user_id are NA. Returning NULL")
    return(NULL)
  } else 
    req <- tw_api_get(
      "https://api.twitter.com/1.1/users/show.json",
      twitter_token, 15,
      query=list(user_id=user_id, screen_name=screen_name,
                 include_entities=include_entities),
    ...
  )
  
  
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
    created_at              = strptime(req$created_at,'%a %b %d %T +0000 %Y',tz = 'UTC'),
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
  
  if (!quietly) message('Success, info of user ',req$screen_name,' correctly retrieved')
  return(req)
}

#' Gets status updates (tweets) from a given user
#' 
#' Using the twitter API, gets the status updates of a given user (up to 3,200)
#' 
#' @param screen_name of the user
#' @template api
#' @param user_id The ID of the user for whom to return results for
#' @param since_id Returns results with an ID greater than (that is, more recent than) the specified ID
#' @param count Number of statuses to get 
#' @param max_id Returns results with an ID less than (that is, older than) or equal to the specified ID
#' @param exclude_replies This parameter will prevent replies from appearing in the returned timeline
#' @param include_rts When set to false, the timeline will strip any native retweets
#' @param quietly Whether or not to show the 'success' message
#' @param ... Additional arguments passed to [GET()]
#' @return A data.frame with tweets (if success), with the following columns: 
#' \itemize{
#' \item `screen_name`
#' \item `in_reply_to_screen_name`
#' \item `user_id`
#' \item `created_at`
#' \item `id`
#' \item `text`
#' \item `source`
#' \item `truncated`
#' \item `retweet_count`
#' \item `favourites_count`
#' \item `favorited`
#' \item `retweeted`
#' \item `coordinates`
#' \item `source_name`
#' }
#' 
#' otherwise returns `NULL`.
#' @details This function is designed to be applied to a large list of twitter
#' accounts, see the example below.
#' \subsection{From twitter}{Returns a collection of the most recent Tweets
#' posted by the user indicated by the screen_name or user_id parameters.
#' }
#' @examples 
#' \dontrun{
#' # List of twitter accounts
#' users <- c('MarsRovers', 'senatormenendez', 'sciencemagazine')
#' 
#' # Getting the twitts (first gen the token)
#' key <- tw_gen_token('myapp','key', 'secret')
#' tweets <- lapply(users, tw_api_get_statuses_user_timeline, twitter_token=key)
#' 
#' # Processing the data (and taking a look)
#' tweets <- do.call(rbind, tweets)
#' head(tweets)
#' }
#' @author George G. Vega Yon
#' @seealso [tw_extract()]
#' @references Twitter REST API (GET statuses/user_timeline)
#' https://dev.twitter.com/rest/reference/get/statuses/user_timeline
#' @export
#' @family API functions
tw_api_get_statuses_user_timeline <- function(
  screen_name=NULL,twitter_token, user_id=NULL, since_id=NULL, count=100,
  max_id=NULL, exclude_replies=NULL, include_rts=NULL,
  quietly=FALSE, ...) {
  
  screen_name <- gsub("^@","",screen_name)
  
  # Checking limits
  if (count > 3200) warning('API can return up to 3,200 tweets.')
  count <- min(c(count,3200))
  
  if (count > 200 & is.null(max_id)) 
    message("We will need to do this in several steps...")
  
  # API CALL
  req <- tw_api_get(
    q="https://api.twitter.com/1.1/statuses/user_timeline.json",
    twitter_token, 15, query=list(
      screen_name=screen_name, user_id=user_id, since_id=since_id, count=count,
      max_id=max_id, exclude_replies=exclude_replies, include_rts=include_rts,
      include_entities='false'), 
    ...
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
    nfav    <- x$favourites_count
    isfav   <- x$favorited
    rpl2sid <- x$in_reply_to_status_id
    lang    <- x$lang
    
    data.frame(
      screen_name             = x$user$screen_name, 
      in_reply_to_screen_name = ifelse(is.null(replyto),NA,replyto),
      in_reply_to_status_id   = ifelse(is.null(rpl2sid),NA,rpl2sid),
      user_id                 = x$user$id,
      created_at              = strptime(x$created_at,'%a %b %d %T +0000 %Y','UTC'),
      id                      = x$id,
      text                    = x$text, 
      source                  = x$source,
      lang                    = ifelse(is.null(lang),NA,lang),
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
  
  # Checking if we have to make a second call
  if (nrow(req)<count & nrow(req)>1) {
    togo <- max(c(0,count - nrow(req)))
    
    minid <- min(req$id)-1
    
    message('Number of statuses got: ', nrow(req), ', to go: ',count, 
            ' (checking id ',minid,')')
    req <- rbind(req, tw_api_get_statuses_user_timeline(
      screen_name,twitter_token, user_id, since_id, togo, max_id=minid,
      exclude_replies, include_rts, quietly=TRUE,...))
  }
  
  class(req) <- c('tw_Class_api_timeline', class(req))
  
  if (!quietly) message('Success, timeline of user ',req$screen_name[1],' correctly retrieved')
  return(req)
}

#' Gets status updates (tweets) via serach
#' 
#' Using the twitter API, gets the status updates via search
#' 
#' @param q Search query
#' @template api
#' @param geocode character with 'latitude,longitude,radius', where radius units must be specified as either 'mi' (miles) or 'km' (kilometers)
#' @param lang Restricts language
#' @param locale Specifies the locale of the query
#' @param result_type Either of the options especified
#' @param count Number of statuses to get 
#' @param until character specifying the limit date (tweets before than) as YYYY-MM-DD
#' @param since_id Returns results with an ID greater than (that is, more recent than) the specified ID
#' @param max_id Returns results with an ID less than (that is, older than) or equal to the specified ID
#' @param quietly Whether or not to show the 'success' message
#' @param ... Additional arguments passed to [GET()]
#' @return A data.frame with tweets (if success), with the following columns: 
#' \itemize{
#' \item `result_type`
#' \item `screen_name`
#' \item `in_reply_to_screen_name`
#' \item `user_id`
#' \item `created_at`
#' \item `id`
#' \item `text`
#' \item `source`
#' \item `truncated`
#' \item `retweet_count`
#' \item `favorite_count`
#' \item `favorited`
#' \item `retweeted`
#' \item `coordinates`
#' \item `source_name`
#' }
#' 
#' otherwise returns `NULL`.
#' @details Keep in mind that the search index has a 7-day limit. In other words,
#' no tweets will be found for a date older than one week.
#' \subsection{From Twitter}{Returns a collection of relevant Tweets matching a 
#' specified query.
#' 
#' Please note that Twitter’s search service and, by extension, the Search API
#' is not meant to be an exhaustive source of Tweets. Not all Tweets will be
#' indexed or made available via the search interface.}
#' @examples 
#' \dontrun{
#' # Getting the twitts (first gen the token)
#' key <- tw_gen_token('myapp','key', 'secret')
#' 
#' # Making a query
#' x <- tw_api_get_search_tweets('lovewins', key)
#' }
#' @author George G. Vega Yon
#' @seealso [tw_extract()]
#' @references Twitter REST API (GET search/tweets)
#' https://dev.twitter.com/rest/reference/get/search/tweets
#' @export
#' @family API functions
tw_api_get_search_tweets <- function(q, twitter_token,
  geocode=NULL, lang=NULL, locale=NULL, result_type=c('mixed','recent','popular'),
  count=100, until='9999-99-99', since_id=NULL, max_id=NULL,quietly=FALSE,...) {
  
  q <- URLencode(q)
  
  # Parsing options
  if (until=='9999-99-99') until <- NULL
  if (length(result_type)>1) result_type<-'mixed'
  
  # API CALL
  req <- tw_api_get(
    "https://api.twitter.com/1.1/search/tweets.json",
    twitter_token, 15,
    query=list(
      q=q,geocode=geocode, lang=lang, locale=locale, result_type=result_type,
      count=count, until=until, since_id=since_id, max_id=max_id,include_entities='false'),
    ...
  )
  
  # Checking if everything went fine
  if (is.null(req)) return(NULL)
  else if (class(req)=='response')
    if (status_code(req)!=200) return(NULL)
  
  # If it works, then process the data
  req <- content(req)
  meta <- req$search_metadata

  req <- lapply(req$statuses, function(x,...) {
    # Nullable characters
    coords  <- paste0(x$coordinates$coordinates,collapse=":")
    replyto <- x$in_reply_to_screen_name
    nfav    <- x$favorite_count
    isfav   <- x$favorited
    data.frame(
      result_type             = x$metadata$result_type,
      screen_name             = x$user$screen_name, 
      in_reply_to_screen_name = ifelse(is.null(replyto),NA,replyto),
      user_id                 = x$user$id,
      created_at              = strptime(x$created_at,'%a %b %d %T +0000 %Y','UTC'),
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
  
  class(req) <- c('tw_Class_api_timeline', class(req))
  attributes(req)$search_metadata <- meta
  
  if (!quietly) message('Success, search ',q,' correctly retrieved')
  return(req)
}

#' GET friends/ids
#'
#' Returns a list of friends of a given user (list of who he/she follows)
#'
#' @param screen_name Screen name
#' @template api
#' @param user_id User ID number
#' @param cursor Cursor page
#' @param count Number of elements to download
#' @param ... Further arguments to be passed to [GET()]
#' @details \subsection{From Twitter}{Returns a cursored collection of user IDs
#' for every user the specified user is following (otherwise known as their
#' ``friends'').}
#' @return A vector
#' @references Twitter REST API (GET search/tweets) 
#' https://dev.twitter.com/rest/reference/get/friends/ids
#' @export
#' @family API functions
tw_api_get_friends_ids <- function(screen_name=NULL, twitter_token, user_id=NULL,
                                   cursor=NULL, count=1000, ...) {
  
  # API CALL
  req <- tw_api_get(
    "https://api.twitter.com/1.1/friends/ids.json",
    twitter_token, 15,
    query=list(
      screen_name=screen_name, user_id=user_id,cursor=cursor, count=count),
    ...
  )
  
  # Checking if everything went fine
  if (is.null(req)) return(NULL)
  else if (class(req)=='response')
    if (status_code(req)!=200) return(NULL)
  
  # If it works, then process the data
  req <- content(req)
  friends <- unlist(req$ids, TRUE)
  
  cursor <- req$next_cursor
  if (cursor & count) {
    count <- count - length(friends)
    message('Retrieving several pages, to go: ', count, ' (next cursor ',cursor,')')
    friends <- c(friends, tw_api_get_friends_ids(screen_name, twitter_token, user_id,
                                             cursor, count, ...))
  }
  
  friends
}

#' GET followers/ids
#' 
#' Returns a list of followers IDs of a given user.
#'
#' @param screen_name User screen name
#' @template api
#' @param user_id User Id number
#' @param cursor Page number (cursor)
#' @param count Number of ids by page
#' @param ... Further arguments to be passed to [GET()]
#' @details \subsection{From twitter}{Returns a cursored collection of user IDs
#' for every user following the specified user.
#' 
#' At this time, results are ordered with the most recent following first -- 
#' however, this ordering is subject to unannounced change and eventual
#' consistency issues.}
#' @return A vector
#' @references Twitter REST API (GET followers/ids)
#' https://dev.twitter.com/rest/reference/get/followers/ids
#' @export
#' @family API functions
tw_api_get_followers_ids <- function(screen_name=NULL, twitter_token, user_id=NULL,
                                   cursor=NULL, count=1000, ...) {
  
  # API CALL
  req <- tw_api_get(
    "https://api.twitter.com/1.1/followers/ids.json",
    twitter_token, 15,
    query=list(
      screen_name=screen_name, user_id=user_id,cursor=cursor, count=count),
    ...
  )
  
  # Checking if everything went fine
  if (is.null(req)) return(NULL)
  else if (class(req)=='response')
    if (status_code(req)!=200) return(NULL)
  
  # If it works, then process the data
  req <- content(req)
  followers <- unlist(req$ids, TRUE)
  
  cursor <- req$next_cursor
  count <- count - length(followers)
  
  if (cursor & count) {
    message('Retrieving several pages, to go: ', count, ' (next cursor ',cursor,')')
    followers <- c(followers, tw_api_get_followers_ids(screen_name, twitter_token, user_id,
                                                 cursor, count, ...))
  }
  
  followers
}

# Converts a data.frame into JSON
# @param d A data frame
# @return A Char string as JSON format
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
#' @param graph A `tw_Class_graph` class object (See [tw_network()])
tw_write_json_network <- function(graph) {
  
  if (!inherits(graph, 'tw_Class_graph')) 
    stop('The graph must be tw_Class_graph object (output of tw_')
  
  nodes <- .tw_df_to_json(graph$nodes)
  links <- .tw_df_to_json(graph$links)
  paste('{\n\t"nodes":[',nodes,'\t\t],\n\t"links":[',links,']\n}',sep="\n")
}

#' Get codes from the places where trends are availables
#' @template api
#' @param ... Ignored
#' @details \subsection{From Twitter}{Returns the locations that Twitter has
#' trending topic information for. 
#' 
#' The response is an array of ``locations'' that encode the location's WOEID and
#' some other human-readable information such as a canonical name and country
#' the location belongs in.
#' 
#' A WOEID is a \href{http://developer.yahoo.com/geo/geoplanet/}{Yahoo! Where On Earth ID}.}
#' @references Twitter REST API (GET trends/available)
#' https://dev.twitter.com/rest/reference/get/trends/available
#' @export
#' @family API functions
tw_api_trends_available <- function(twitter_token,...) {
  req <- tw_api_get('https://api.twitter.com/1.1/trends/available.json',
                     twitter_token,minutes = 15,...)
  
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
#' 
#' Uses the output from [tw_api_trends_available()]
#' 
#' @param id Id of the place
#' @template api
#' @param exclude Whether to exclude hashtags or not
#' @param ... Ignored
#' @details \subsection{From Twitter}{Returns the top 10 trending topics for a
#' specific WOEID, if trending information is available for it.
#' 
#' The response is an array of ``trend'' objects that encode the name of the
#' trending topic, the query parameter that can be used to search for the topic
#' on \href{http://search.twitter.com/}{Twitter Search}, and the Twitter Search
#' URL.
#' 
#' This information is cached for 5 minutes. Requesting more frequently than
#' that will not return any more data, and will count against your rate limit
#' usage.}
#' @references Twitter REST API (GET trends/place)
#' https://dev.twitter.com/rest/reference/get/trends/place
#' @export
#' @family API functions
tw_api_get_trends_place <- function(id,twitter_token,exclude=FALSE,...) {
  
  # Making the request
  req <- tw_api_get(
    paste0('https://api.twitter.com/1.1/trends/place.json?id=',id,
           ifelse(exclude,'&exclude=hashtags','')),twitter_token, 15)
  
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

#' Get Followers list
#'
#' @param screen_name  Screen name
#' @template api
#' @param user_id User id
#' @param cursor (advanced)
#' @param count Number of registries per page (advanced)
#' @param skip_status It self
#' @param include_user_entities (advanced)
#' @param max.n Number of followers to download
#' @param ... Further arguments to be passed to [GET()]
#' @details \subsection{From Twitter}{Returns a cursored collection of user
#' objects for users following the specified user.
#' 
#' At this time, results are ordered with the most recent following first --
#' however, this ordering is subject to unannounced change and eventual
#' consistency issues.}
#' @references Twitter REST API (GET followers/list)
#' https://dev.twitter.com/rest/reference/get/followers/list
#' @return A list
#' @export
#' @family API functions
tw_api_get_followers_list <- function(screen_name=NULL,
  twitter_token, user_id=NULL, cursor=NULL, count=200,
  skip_status=NULL, include_user_entities='false',max.n=NULL,...) {
  
  # Fixing the number to retrieve
  if (length(max.n)) getall <- FALSE
  else getall <- TRUE
  
  if (!getall) count <- min(c(count, max.n))
  
  # API CALL
  req <- tw_api_get(
    "https://api.twitter.com/1.1/followers/list.json",
    twitter_token, 15,
    query=list(user_id=user_id, screen_name=screen_name, cursor=cursor,
               count=count, skip_status=skip_status,
               include_user_entities=include_user_entities),
    ...
  )
  
  # Checking if everything went fine
  if (is.null(req)) return(NULL)
  else if (class(req)=='response')
    if (status_code(req)!=200) return(NULL)
  
  # If it works, then process the data
  req <- content(req)
  
  # Processing the info
  usrs <- do.call('rbind', lapply(req$users, function(x,...) {
    # Nullable elements
    time_zone   <- req$time_zone
    utc_offset  <- req$utc_offset
    description <- req$description
    location    <- req$location
    
    data.frame(
      stringsAsFactors = FALSE,
      id                      = x$id,
      name                    = x$name,
      screen_name             = x$screen_name,
      contributors_enabled    = x$contributors_enabled,
      created_at              = strptime(x$created_at,'%a %b %d %T +0000 %Y','UTC'),
      default_profile         = x$default_profile,
      default_profile_image   = x$default_profile_image,
      description             = ifelse(is.null(description),NA,description),
      favourites_count        = x$favourites_count,
      followers_count         = x$followers_count,
      friends_count           = x$friends_count,
      geo_enabled             = x$geo_enabled,
      is_translator           = x$is_translator,
      lang                    = x$lang,
      listed_count            = x$listed_count,
      location                = ifelse(is.null(location),NA,location),
      profile_image_url       = x$profile_image_url,
      profile_image_url_https = x$profile_image_url_https,
      protected               = x$protected,
      statuses_count          = x$statuses_count,
      time_zone               = ifelse(is.null(time_zone),NA,time_zone),
      utc_offset              = ifelse(is.null(utc_offset),NA,utc_offset),
      verified                = x$verified
    )
  }))
  
  # Checking if we need to continue or not
  if (!getall) {
    max.n  <- max(c(0,max.n - nrow(usrs)))
    if (!max.n) return(usrs)
  }
  
  cursor <- req$next_cursor

  if (cursor) {
    message('Number of records got: ', nrow(usrs), ' (next cursor ',cursor,')')
    
    # Next recursion
    usrs <- rbind(usrs, tw_api_get_followers_list(screen_name,
      twitter_token, user_id,  cursor, count,
      skip_status, include_user_entities, max.n,...
    ))
  }
  
  return(usrs)
}

#' Gets a sample through the sample API
#' 
#' ON DEVELOPMENT
#' 
#' @template api
#' @param Timeout Number of seconds
#' @param ... Additional parameters to be passed to [GET()]
#' @details \subsection{From Twitter}{Returns a small random sample of all
#' public statuses. The Tweets returned by the default access level are the
#' same, so if two different clients connect to this endpoint, they will see the
#' same Tweets.}
#' @references Twitter Streaming API (GET statuses/sample)
#' https://dev.twitter.com/streaming/reference/get/statuses/sample
#' @export
#' @family API functions
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
                      }),...), error=function(e) e)
  
  # message('ok')
  con <- tempfile()
  on.exit(close(con))
  writeBin(tweets,con)
  
  close(tweets)
  return(readLines(con))
}

#' Search users
#' 
#' Search users via approximate string matching
#'
#' @param q Query
#' @template api
#' @param page Page number to retrieve
#' @param count Number of accounts per page
#' @param quietly Whether or not to show the 'success' message
#' @param ... Further parameters to be passed to [GET()] 
#'
#' @details 
#' \subsection{From Twitter}{Provides a simple, relevance-based search interface to public user 
#' accounts on Twitter. Try querying by topical interest, full name, company name,
#' location, or other criteria. Exact match searches are not supported.}
#' @references Twitter REST API (GET users/search) https://dev.twitter.com/rest/reference/get/users/search
#' @return A list of twitter accounts
#' @export
#' @family API functions
tw_api_get_users_search <- function(q, twitter_token, page=NULL, count=20,quietly=TRUE,...) {
  
  # Encoding the query
  q <- URLencode(q)
  
  # API CALL
  req <- tw_api_get(
    "https://api.twitter.com/1.1/users/search.json",
    twitter_token, 15,
    query=list(q=q, page=page,count=count,include_entities="false"),
    ...
  )
  
  # Checking if everything went fine
  if (is.null(req)) return(NULL)
  else if (class(req)=='response')
    if (status_code(req)!=200) return(NULL)
  
  # If it works, then process the data
  req <- content(req)

  req <- lapply(req, function(x,...) {
    # Nullable characters
    nfav <- x$favourites_count
    lang <- x$lang
    loca <- x$location
    tzon <- x$time_zone
    uurl <- x$url
    utco <- x$utc_offset
    
    data.frame(
      screen_name             = x$screen_name,
      name                    = x$name,
      user_id                 = x$id,
      created_at              = strptime(x$created_at,'%a %b %d %T +0000 %Y','UTC'),
      lang                    = ifelse(is.null(lang),NA,lang),
      favourite_count         = ifelse(is.null(nfav),NA,nfav),
      description             = x$description,
      friends_count           = x$friends_count,
      is_translator           = x$is_translator,
      followers_count         = x$followers_count,
      geo_enabled             = x$geo_enabled,
      location                = ifelse(is.null(loca),NA,loca),
      listed_count            = x$listed_count,
      protected               = x$protected,
      time_zone               = ifelse(is.null(tzon),NA,tzon),
      statuses_count          = x$statuses_count,
      url                     = ifelse(is.null(uurl),NA,uurl),
      utc_offset              = ifelse(is.null(utco),NA,utco),
      verified                = x$verified,
      stringsAsFactors = FALSE
    )
  })

  if (!quietly) message("Search for user -",URLdecode(q),"- success.")

  do.call("rbind",req)
}
# x<-tw_api_get_statuses_sample2(key,5)
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
