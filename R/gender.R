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
#' 
#' Using names+gender dataset, matches a list of \code{Names} with a dictionary
#' and sets the gender of the name.
#' 
#' @param Names Character vector of names/text to analyze and match
#' @param male Character vector of male names
#' @param female Character vector of female names
#' @param lan Languaje of the names
#' @param rm.no.alpha Whether or not to remove no alpha characters
#' @return A factor vector assigning gender to each \code{Name} provided.
#' @details The char match is written in C++, which is why it should be fast.
#' 
#' When no \code{male} or \code{female} names are provided, the function uses
#' by default the names datasets \code{\link{names_male_en}} and  
#' \code{\link{names_female_en}} (if \code{lan='en'}).
#' 
#' If \code{lan='es'} and no list of male or female names is provided, the
#' function will load the \code{\link{names_male_es}} and  
#' \code{\link{names_female_es}} datasets. Note that if \code{lan=c('es','en')}
#' the function will use both.
#' 
#' The argument \code{rm.no.alpha}, by default in \code{TRUE} set whether or not
#' to remove no letter characters before analyzing the data.
#' 
#' @examples 
#' # Some list of names
#' mix <- c('pedro','peter','mariano','maria jose','pablo','paul','jenny')
#' tw_gender(mix)
#' 
#' @export
tw_gender <- function(Names, male=NULL, female=NULL, lan=c('en'), rm.no.alpha=TRUE) {
  # Analyzing the string, it should be in lowercase and including only
  # letters
  Names <- tolower(Names)
  
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
  if (rm.no.alpha) Names <- gsub('[^a-z ]','',Names)
  
  factor(cpp_tw_gender(Names,male,female),c(-1,0,1),c('unidentified','female','male'))
}

tw_names_db <- function(lan,male=TRUE) {
  gender <- ifelse(male,'male','female')
  
  read.csv(
    system.file(paste0('data/names_',gender,'_',lan,'.csv.xz'),package='twitterreport'),
    as.is = TRUE)[,1]
}
