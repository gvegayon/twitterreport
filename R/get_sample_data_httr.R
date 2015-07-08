library(httr)

# 1. Find OAuth settings for twitter:
#    https://dev.twitter.com/docs/auth/oauth
oauth_endpoints("twitter")

# 2. Register an application at https://apps.twitter.com/
#    Make sure to set callback url to "http://127.0.0.1:1410"
#
#    Replace key and secret below
myapp <- oauth_app("r-twitt",
                   key = "JvBaXf5CWYK8gjIGHOxJBiIXO",
                   secret = "mJWb9CzG5cFNVxAuLMBrNUdKcr3vPvp1in4Tcuj9ag38Mn3jtr"
)

# 3. Get OAuth credentials
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

# 4. Use API
req <- GET("https://api.twitter.com/1.1/search/tweets.json?q=lovewins&result_type=mixed&count=500",
           config(token = twitter_token))
stop_for_status(req)
# content(req)

# > names(content(req)$statuses[[1]]$user)
# [1] "id"                                 "id_str"                            
# [3] "name"                               "screen_name"                       
# [5] "location"                           "description"                       
# [7] "url"                                "entities"                          
# [9] "protected"                          "followers_count"                   
# [11] "friends_count"                      "listed_count"                      
# [13] "created_at"                         "favourites_count"                  
# [15] "utc_offset"                         "time_zone"                         
# [17] "geo_enabled"                        "verified"                          
# [19] "statuses_count"                     "lang"                              
# [21] "contributors_enabled"               "is_translator"                     
# [23] "is_translation_enabled"             "profile_background_color"          
# [25] "profile_background_image_url"       "profile_background_image_url_https"
# [27] "profile_background_tile"            "profile_image_url"                 
# [29] "profile_image_url_https"            "profile_link_color"                
# [31] "profile_sidebar_border_color"       "profile_sidebar_fill_color"        
# [33] "profile_text_color"                 "profile_use_background_image"      
# [35] "has_extended_profile"               "default_profile"                   
# [37] "default_profile_image"              "following"                         
# [39] "follow_request_sent"                "notifications"     
tweets <-as.data.frame(do.call(rbind,lapply(content(req)$statuses, function(x) {
  c(x[c("id","text","retweet_count")],
        screen_name=x$user$screen_name,
        friends_count=x$user$friends_count,
        followers_count=x$user$followers_count,
        favourites_count=x$user$favourites_count,
        verified=x$user$verified,
        location=x$user$location,
        name=x$user$name,
        created_at=x$user$created_at,
        description=x$user$description,
        #url=x$user$url,
        protected=x$user$protected,
        #utc_offset=x$user$utc_offset,
        statuses_count=x$user$statuses_count,
        id_str=x$user$id_str, recursive=TRUE
        #entities=x$user$entities
        )
  })), stringsAsFactors=FALSE
  )

# Fixing data types 
tweets$id <- as.numeric(tweets$id)
tweets$friends_count <- as.numeric(tweets$friends_count)
tweets$followers_count<- as.numeric(tweets$followers_count)
tweets$favourites_count<- as.numeric(tweets$favourites_count)
tweets$statuses_count<- as.numeric(tweets$statuses_count)

save(tweets,file = "data/lovewins.RData")
