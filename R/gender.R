# Loading data

# US names
load('ddata/namedata/data/usnames.RData')
load('ddata/namedata/data/uknames.RData')

female_names_us_db <- unique(tolower(subset(usnames, F>M, select=Name, drop=TRUE)))
male_names_us_db <- unique(tolower(subset(usnames, F<M, select=Name, drop=TRUE)))
female_names_uk_db <- unique(tolower(subset(uknames, F>M, select=Name, drop=TRUE)))
male_names_uk_db <- unique(tolower(subset(uknames, F<M, select=Name, drop=TRUE)))

female_names_en_db <- c(female_names_uk_db,female_names_us_db)
male_names_en_db <- c(male_names_uk_db,male_names_us_db)

save(female_names_en_db, file='data/female_names_en_db.RData', compress = TRUE)
save(male_names_en_db, file='data/male_names_en_db.RData', compress = TRUE)

# UK names


save(female_names_us_db, file='data/female_names_us_db.RData', compress = TRUE)
save(male_names_us_db, file='data/male_names_us_db.RData', compress = TRUE)


#' Matches text with male/female names
#' @param x Character vector of names/text to analyze and match
#' @param male Character vector of male names
#' @param female Character vector of female names
#' @param lan Languaje of the names
#' @param rmNoAlpha Whether or not to remove no alpha characters
#' @details The char match is written in C++, which is why it should be fast
#' @section Names datasets
#' 
#' @references For english names OpenGenderTracking project
#' \url{https://github.com/OpenGenderTracking/globalnamedata}
#' 
#' For spanish names
#' \url{https://github.com/apalancat/spanish-names}
#' @export
tw_gender <- function(x, male=NULL, female=NULL, lan=c('es','en'), rmNoAlpha=TRUE) {
  # Analyzing the string, it should be in lowercase and including only
  # letters
  x <- tolower(x)
  
  # Removing no string characters
  if (rmNoAlpha) x <- gsub('^[a-z ]','',x)
  
  print(head(x))
  
  cpp_tw_gender(x,male,female)
}
