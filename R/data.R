################################################################################
# Lexicon datasets
################################################################################
#' Negative Lexicon (en)
#' @name sentiment_lexicon_neg_en
#' @format A Character vector
#' @family example datasets
#' @family lexicon datasets
#' @source http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#datasets
NULL

#' Positive Lexicon (en)
#' @name sentiment_lexicon_pos_en
#' @format A Character vector
#' @family example datasets
#' @family lexicon datasets
#' @source http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#datasets
NULL

#' Warriner et al. Lexicon (en)
#' @name warriner_et_al_en
#' @format A data.frame with scores (Valence)
#' @details Spanish version translated by Daniel Gayo-Avello based on Warriner, et al.
#' @source http://crr.ugent.be/archives/1003
#' @family lexicon datasets
#' @family example datasets
NULL

#' Warriner et al. Lexicon (es)
#' @name warriner_et_al_es
#' @format A data.frame with scores (Valence)
#' @family example datasets
#' @family lexicon datasets
#' @details Based on *Warriner, A.B., Kuperman, V., & Brysbaert, M. (2013). Norms of valence, arousal, and dominance for 13,915 English lemmas. Behavior Research Methods, 45, 1191-1207.*
#' @source http://danigayo.info/blog/index.php?entry=entry130117-183114
NULL

################################################################################
# Names datasets
################################################################################
#' Spanish male names
#' @name names_male_es
#' @family example datasets
#' @family names datasets
#' @format A character vector with 25,442 elements
#' @source https://github.com/apalancat/spanish-names
NULL

#' Spanish female names
#' @name names_female_es
#' @family example datasets
#' @family names datasets
#' @format A character vector with 24,494 elements
#' @source https://github.com/apalancat/spanish-names
NULL

#' English male names
#' @name names_male_en
#' @family example datasets
#' @family names datasets
#' @format A character vector with 51,212 elements
#' @source OpenGenderTracking project https://github.com/OpenGenderTracking/globalnamedata
NULL

#' English female names
#' @name names_female_en
#' @family example datasets
#' @family names datasets
#' @format A character vector with 80,148 elements
#' @source OpenGenderTracking project https://github.com/OpenGenderTracking/globalnamedata
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
#'  \item `screen_name`
#'  \item `in_reply_to_screen_name`
#'  \item `user_id`
#'  \item `created_at`
#'  \item `id`
#'  \item `text`
#'  \item `source`
#'  \item `truncated`
#'  \item `retweet_count`
#'  \item `favorite_count`
#'  \item `favorited`
#'  \item `retweeted`
#'  \item `coordinates`
#'  \item `source_name`
#' }
#' 
#' @name senate_tweets
#' @family example datasets
#' @format A data.frame with 19,786 rows and 14 variables
#' @source http://dev.twitter.com
NULL

#' US senators info from senate.gov
#' 
#' Extracted using webcrawling, the dataset contains the following columns:
#' 
#' \itemize{
#'  \item `Name`
#'  \item `party`
#'  \item `State`
#'  \item `Addr`
#'  \item `phone`
#'  \item `website`
#'  \item `class`
#' }
#' 
#' @name senators
#' @family example datasets
#' @format A data.frame with 100 rows and 7 variables
#' @source http://www.senate.gov/senators/contact/
NULL

#' US senators twitter profile
#' 
#' Extracted from the web using webcrawling, the dataset contains the following
#' columns:
#' 
#' \itemize{
#'  \item `tw_id`
#'  \item `tw_name`
#'  \item `tw_screen_name`
#'  \item `tw_contributors_enabled`
#'  \item `tw_created_at`
#'  \item `tw_default_profile`
#'  \item `tw_default_profile_image`
#'  \item `tw_description`
#'  \item `tw_favourites_count`
#'  \item `tw_followers_count`
#'  \item `tw_friends_count`
#'  \item `tw_geo_enabled`
#'  \item `tw_is_translator`
#'  \item `tw_lang`
#'  \item `tw_listed_count`
#'  \item `tw_location`
#'  \item `tw_profile_image_url`
#'  \item `tw_profile_image_url_https`
#'  \item `tw_protected`
#'  \item `tw_statuses_count`
#'  \item `tw_time_zone`
#'  \item `tw_utc_offset`
#'  \item `tw_verified`
#' }
#' 
#' @name senators_profile
#' @family example datasets
#' @format A data.frame with 100 rows and 23 columns
#' @source https://dev.twitter.com
NULL

