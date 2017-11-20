#' Matches text with male/female names
#' 
#' Using names+gender dataset, matches a list of `Names` with a dictionary
#' and sets the gender of the name.
#' 
#' @param Names Character vector of names/text to analyze and match
#' @param male Character vector of male names
#' @param female Character vector of female names
#' @param lan Languaje of the names
#' @param rm.no.alpha Whether or not to remove no alpha characters
#' @return A factor vector assigning gender to each `Name` provided.
#' @details The char match is written in C++, which is why it should be fast.
#' 
#' When no `male` or `female` names are provided, the function uses
#' by default the names datasets [names_male_en()] and  
#' [names_female_en()] (if `lan='en'`).
#' 
#' If `lan='es'` and no list of male or female names is provided, the
#' function will load the [names_male_es()] and  
#' [names_female_es()] datasets. Note that if `lan=c('es','en')`
#' the function will use both.
#' 
#' The argument `rm.no.alpha`, by default in `TRUE` set whether or not
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
