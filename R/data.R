#' US senate tweets
#' 
#' This dataset contains tweets from the US senate downloaded using the 'twitter'
#' API tool. It contains the following columns
#' 
#' \itemize{
#'  \item screen_name
#'  \item in_reply_to_screen_name
#'  \item user_id
#'  \item created_at
#'  \item id
#'  \item text
#'  \item source
#'  \item truncated
#'  \item retweet_count
#'  \item favorite_count
#'  \item favorited
#'  \item retweeted
#'  \item coordinates
#'  \item source_name
#' }
#' 
#' @name senate_tweets
#' @format A data.frame with 19,786 rows and 14 variables
#' @source \url{http://dev.twitter.com}
NULL

#' US senators info from senate.gov
#' 
#' Extracted using webcrawling
#' 
#' Name
#' \itemize{
#' \item party
#' \item State
#' \item Addr
#' \item phone
#' \item website
#' \item class
#' }
#' 
#' @name senators
#' @format A data.frame with 100 rows and 7 variables
#' @source \url{http://www.senate.gov/senators/contact/}
NULL

#' US senators twitter profile
#' 
#' Extracted from the web using webcrawling
#' 
#' @name senators_profile
#' @format A data.frame with 100 rows and 23 columns
#' @source \url{https://dev.twitter.com}
NULL