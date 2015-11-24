################################################################################
# Lexicon datasets
################################################################################
#' Negative Lexicon (en)
#' @name sentiment_lexicon_neg_en
#' @format A Character vector
#' @family example datasets
#' @family lexicon datasets
#' @source \url{http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#datasets}
NULL

#' Positive Lexicon (en)
#' @name sentiment_lexicon_pos_en
#' @format A Character vector
#' @family example datasets
#' @family lexicon datasets
#' @source \url{http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#datasets}
NULL

#' Warriner et al. Lexicon (en)
#' @name warriner_et_al_en
#' @format A data.frame with scores (Valence)
#' @details Spanish version translated by Daniel Gayo-Avello based on Warriner, et al.
#' @source \url{http://crr.ugent.be/archives/1003}
#' @family lexicon datasets
#' @family example datasets
NULL

#' Warriner et al. Lexicon (es)
#' @name warriner_et_al_es
#' @format A data.frame with scores (Valence)
#' @family example datasets
#' @family lexicon datasets
#' @details Based on \emph{Warriner, A.B., Kuperman, V., & Brysbaert, M. (2013). Norms of valence, arousal, and dominance for 13,915 English lemmas. Behavior Research Methods, 45, 1191-1207.}
#' @source \url{http://danigayo.info/blog/index.php?entry=entry130117-183114}
NULL

################################################################################
# Names datasets
################################################################################
#' Spanish male names
#' @name names_male_es
#' @family example datasets
#' @family names datasets
#' @format A character vector with 25,442 elements
#' @source \url{https://github.com/apalancat/spanish-names}
NULL

#' Spanish female names
#' @name names_female_es
#' @family example datasets
#' @family names datasets
#' @format A character vector with 24,494 elements
#' @source \url{https://github.com/apalancat/spanish-names}
NULL

#' English male names
#' @name names_male_en
#' @family example datasets
#' @family names datasets
#' @format A character vector with 51,212 elements
#' @source OpenGenderTracking project \url{https://github.com/OpenGenderTracking/globalnamedata}
NULL

#' English female names
#' @name names_female_en
#' @family example datasets
#' @family names datasets
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
#' @family example datasets
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
#' @family example datasets
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
#' @family example datasets
#' @format A data.frame with 100 rows and 23 columns
#' @source \url{https://dev.twitter.com}
NULL

