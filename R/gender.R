# # Loading data
# 
# # # EN names
# load('ddata/namedata/data/usnames.RData')
# load('ddata/namedata/data/uknames.RData')
# 
# female_names_us_db <- unique(tolower(subset(usnames, F>M, select=Name, drop=TRUE)))
# male_names_us_db <- unique(tolower(subset(usnames, F<M, select=Name, drop=TRUE)))
# female_names_uk_db <- unique(tolower(subset(uknames, F>M, select=Name, drop=TRUE)))
# male_names_uk_db <- unique(tolower(subset(uknames, F<M, select=Name, drop=TRUE)))
# 
# names_female_en <- unique(c(female_names_uk_db,female_names_us_db))
# names_male_en <- unique(c(male_names_uk_db,male_names_us_db))
# 
# # save(female_names_en_db, file='data/female_names_en_db.rda', compress = TRUE)
# # save(male_names_en_db, file='data/male_names_en_db.rda', compress = TRUE)
# 
# write.csv(
#   names_female_en, file='data/names_female_en.csv',quote = FALSE, row.names = FALSE)
# write.csv(
#   names_male_en, file='data/names_male_en.csv',quote = FALSE, row.names = FALSE)
# 
# # ES names
# male_es <- read.csv(file = 'ddata/hombres.csv',header = TRUE,as.is=TRUE)
# female_es <- read.csv(file = 'ddata/mujeres.csv',header = TRUE,as.is=TRUE)
# 
# names_female_es <- unique(tolower(female_es$nombre))
# names_male_es <- unique(tolower(male_es$nombre))
# 
# # save(female_names_es_db, file='data/female_names_es_db.rda', compress = TRUE)
# # save(male_names_es_db, file='data/male_names_es_db.rda', compress = TRUE)
# write.csv(
#   names_female_es, file='data/names_female_es.csv',quote = FALSE, row.names = FALSE)
# write.csv(
#   names_male_es, file='data/names_male_es.csv',quote = FALSE, row.names = FALSE)


#' Matches text with male/female names
#' @param x Character vector of names/text to analyze and match
#' @param male Character vector of male names
#' @param female Character vector of female names
#' @param lan Languaje of the names
#' @param rmNoAlpha Whether or not to remove no alpha characters
#' @details The char match is written in C++, which is why it should be fast
#' @examples 
#' # Some list of names
#' mix <- c('pedro','peter','mariano','maria jose','pablo','paul','jenny')
#' tw_gender(mix)
#' 
#' # Example with the candidates
#' @export
tw_gender <- function(x, male=NULL, female=NULL, lan=c('es','en'), rmNoAlpha=TRUE) {
  # Analyzing the string, it should be in lowercase and including only
  # letters
  x <- tolower(x)
  
  # Checking all is ok
  if (any(!(lan %in% c('es','en')))) stop('Languaje not supported')
  if (all(lan %in% c('es','en'))) lan <- 'en'
  
  # Building male/female list
  if (!length(male))
    for (l in lan)
      male <-c(male,tw_names_db(l,TRUE))
  
  if (!length(female))
    for (l in lan)
      female <-c(female,tw_names_db(l,FALSE))
  
  # Removing no string characters
  if (rmNoAlpha) x <- gsub('[^a-z ]','',x)
  
  factor(cpp_tw_gender(x,male,female),c(-1,0,1),c('unidentified','female','male'))
}

#' Retrieve the database of 
#' @param lan Languaje
#' @param male TRUE loads male names
#' @return A factor vector indicating gender or unidentified
#' @examples 
#' # Retrieving female spanish names
#' fem_es <- tw_names_db('es',FALSE)
#' 
#' # Retrieving male english names
#' mal_en <- tw_names_db('en')
#' @export
tw_names_db <- function(lan,male=TRUE) {
  gender <- ifelse(male,'male','female')
  
  path <- system.file('data/',package='twitterreport')
  read.csv(paste0(path,'/names_',gender,'_',lan,'.csv.xz'),as.is = TRUE)[,1]
}
