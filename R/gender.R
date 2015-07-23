# # Loading data
# 
# # EN names
# load('ddata/namedata/data/usnames.RData')
# load('ddata/namedata/data/uknames.RData')
# 
# female_names_us_db <- unique(tolower(subset(usnames, F>M, select=Name, drop=TRUE)))
# male_names_us_db <- unique(tolower(subset(usnames, F<M, select=Name, drop=TRUE)))
# female_names_uk_db <- unique(tolower(subset(uknames, F>M, select=Name, drop=TRUE)))
# male_names_uk_db <- unique(tolower(subset(uknames, F<M, select=Name, drop=TRUE)))
# 
# female_names_en_db <- c(female_names_uk_db,female_names_us_db)
# male_names_en_db <- c(male_names_uk_db,male_names_us_db)
# 
# save(female_names_en_db, file='data/female_names_en_db.RData', compress = TRUE)
# save(male_names_en_db, file='data/male_names_en_db.RData', compress = TRUE)
# 
# # ES names
# male_es <- read.csv(file = 'ddata/hombres.csv',header = TRUE,as.is=TRUE)
# female_es <- read.csv(file = 'ddata/mujeres.csv',header = TRUE,as.is=TRUE)
# 
# female_names_es_db <- tolower(female_es$nombre)
# male_names_es_db <- tolower(male_es$nombre)
# 
# save(female_names_es_db, file='data/female_names_es_db.RData', compress = TRUE)
# save(male_names_es_db, file='data/male_names_es_db.RData', compress = TRUE)


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
  
  # Getting the dictionaries
  
#   data(list='female_names_es_db',envir=environment())
#   data(list='male_names_es_db',envir=environment())
#   data(list='female_names_en_db',envir=environment())
#   data(list='male_names_en_db',envir=environment())
  lapply(lan,tw_names_db,envir=environment(),loadOnly=TRUE,male=TRUE)
  lapply(lan,tw_names_db,envir=environment(),loadOnly=TRUE,male=FALSE)
  
  # Building male/female list
  if (!length(male)) {
    if ('es' %in% lan) male <- c(male, male_names_es_db)
    if ('en' %in% lan) male <- c(male, male_names_en_db)
  }
  
  if (!length(female)) {
    if ('es' %in% lan) female <- c(female, female_names_es_db)
    if ('en' %in% lan) female <- c(female, female_names_en_db)
  }
  
  # Removing no string characters
  if (rmNoAlpha) x <- gsub('[^a-z ]','',x)
  
  factor(cpp_tw_gender(x,male,female),c(-1,0,1),c('unidentified','female','male'))
}

#' Retrieve the database of 
#' @param lan Languaje
#' @param male TRUE loads male names
#' @param envir Environment to which it will be loaded
#' @param loadOnly TRUE only loads (no assign)
#' @return A factor vector indicating gender or unidentified
#' @examples 
#' # Retrieving female spanish names
#' fem_es <- tw_names_db('es',FALSE)
#' 
#' # Retrieving male english names
#' mal_en <- tw_names_db('en')
#' @export
tw_names_db <- function(lan,male=TRUE,envir=NULL,loadOnly=FALSE) {
  gender <- ifelse(male,'male','female')
  
  if (!length(envir)) envir=environment()
  
  if (!loadOnly) get(data(list=paste0(gender,'_names_',lan,'_db'),envir=envir))
}