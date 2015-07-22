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