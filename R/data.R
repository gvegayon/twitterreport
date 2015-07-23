################################################################################
# Names datasets
################################################################################
#' Spanish male names
#' @name male_names_es_db
#' @format A character vector with 25,442 elements
#' @source \url{https://github.com/apalancat/spanish-names}
NULL

#' Spanish female names
#' @name female_names_es_db
#' @format A character vector with 24,494 elements
#' @source \url{https://github.com/apalancat/spanish-names}
NULL

#' English male names
#' @name male_names_en_db
#' @format A character vector with 51,212 elements
#' @source OpenGenderTracking project \url{https://github.com/OpenGenderTracking/globalnamedata}
NULL

#' English female names
#' @name female_names_en_db
#' @format A character vector with 80,148 elements
#' @source OpenGenderTracking project \url{https://github.com/OpenGenderTracking/globalnamedata}
NULL


################################################################################
# US politics dataset
################################################################################
#' US senate tweets
#' 
#' This dataset contains tweets from the US senate downloaded using the 'twitter'
#' API tool. It contains the following columns
#' 
#' \itemize{
#'  \item \code{screen_name}
#'  \item \code{in_reply_to_screen_name}
#'  \item \code{user_id}
#'  \item \code{created_at}
#'  \item \code{id}
#'  \item \code{text}
#'  \item \code{source}
#'  \item \code{truncated}
#'  \item \code{retweet_count}
#'  \item \code{favorite_count}
#'  \item \code{favorited}
#'  \item \code{retweeted}
#'  \item \code{coordinates}
#'  \item \code{source_name}
#' }
#' 
#' @name senate_tweets
#' @format A data.frame with 19,786 rows and 14 variables
#' @source \url{http://dev.twitter.com}
NULL

#' US senators info from senate.gov
#' 
#' Extracted using webcrawling, the dataset contains the following columns:
#' 
#' \itemize{
#'  \item \code{Name}
#'  \item \code{party}
#'  \item \code{State}
#'  \item \code{Addr}
#'  \item \code{phone}
#'  \item \code{website}
#'  \item \code{class}
#' }
#' 
#' @name senators
#' @format A data.frame with 100 rows and 7 variables
#' @source \url{http://www.senate.gov/senators/contact/}
NULL

#' US senators twitter profile
#' 
#' Extracted from the web using webcrawling, the dataset contains the following
#' columns:
#' 
#' \itemize{
#'  \item \code{tw_id}
#'  \item \code{tw_name}
#'  \item \code{tw_screen_name}
#'  \item \code{tw_contributors_enabled}
#'  \item \code{tw_created_at}
#'  \item \code{tw_default_profile}
#'  \item \code{tw_default_profile_image}
#'  \item \code{tw_description}
#'  \item \code{tw_favourites_count}
#'  \item \code{tw_followers_count}
#'  \item \code{tw_friends_count}
#'  \item \code{tw_geo_enabled}
#'  \item \code{tw_is_translator}
#'  \item \code{tw_lang}
#'  \item \code{tw_listed_count}
#'  \item \code{tw_location}
#'  \item \code{tw_profile_image_url}
#'  \item \code{tw_profile_image_url_https}
#'  \item \code{tw_protected}
#'  \item \code{tw_statuses_count}
#'  \item \code{tw_time_zone}
#'  \item \code{tw_utc_offset}
#'  \item \code{tw_verified}
#' }
#' 
#' @name senators_profile
#' @format A data.frame with 100 rows and 23 columns
#' @source \url{https://dev.twitter.com}
NULL