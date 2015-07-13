rm(list=ls())

library(RCurl)
library(XML)
library(stringr)

################################################################################
# Data base from house.gov
################################################################################
# Key to Room Codes
# CHOB: Cannon House Office Building
# LHOB: Longworth House Office Building
# RHOB: Rayburn House Office Building

# Getting the table
representatives <- readHTMLTable("http://www.house.gov/representatives/",stringsAsFactors=FALSE)
representatives <- do.call(rbind, representatives)

# Getting the info (URLS)
house <- getURLContent("http://www.house.gov/representatives/")

urls <- sapply(representatives$Name, function(x,...) {
  str_extract(house,paste0('https?://[a-zA-Z]+\\.house\\.gov(?=/?">\\s*',x,")"))}
  )

representatives <- cbind(representatives,website=urls)

# Looking for state names
states <- str_extract_all(house,"(?<=state\\_[a-z]{2}.{2})([a-zA-Z ]{2,})")[[1]]
tmp <- str_detect(representatives$District,paste0(states,collapse="|"))
representatives <- representatives[tmp,]
representatives$state <- str_match(representatives$District,paste0(states,collapse="|"))
representatives$DistrictNum <- as.numeric(str_match(representatives$District,'[0-9]+(?=[a-z]{2} District)'))
rm(tmp)

row.names(representatives) <- 1:nrow(representatives)

################################################################################
# Getting twitter accounts
################################################################################
# Function to enter the website and get the twitter account
tw_get_tw_account <- function(uri) {
   web <- getURL(uri)
   try(str_extract_all(web,'https?://(www\\.)?twitter.com/(#!/)?[a-zA-Z0-9_]+(?=">?)'))[[1]]
}

# Loop
twitterAccounts <- vector("list",nrow(representatives))
n <- length(twitterAccounts)
for (i in 1:n) {
  twitterAccounts[[i]] <- tw_get_tw_account(representatives$website[i])
  if (!(i %% 10)) save.image("data/congress_info.RData")
  message(sprintf("%03d of %03d",i,n)," Congressman ",representatives$Name[i]," done...")
}

# Normalizing the twitter accounts (note that screen names are not case
# sensitive)
twitterAccounts <- lapply(twitterAccounts, str_extract, "[a-zA-Z0-9_]+$")
twitterAccounts <- lapply(twitterAccounts, tolower)
twitterAccounts <- lapply(twitterAccounts, unique)

save.image("data/congress_info.RData")
load("data/congress_info.RData")

################################################################################
# Getting account info from API
################################################################################

# Get info from the twitter homepage
source("R/verify.R")

#' @title Get user information
tw_api_get_usr_profile <- function(x,...) { 
  if (is.na(x)) return(NULL)
  else { 
    # Query
    query <- paste0(
      "https://api.twitter.com/1.1/users/show.json?screen_name=",x)
    
    status <- 0
    while (status!=200) {
      # Making the call
      req <- GET(query, config(token=twitter_token))
      status <- status_code(req)
      
      if (status==429) {
        message('Too Many Requests, will try in 15min (current user ',x,')...')
        message(rep('-',31))
        for (i in 1:31) {
          Sys.sleep(30)
          message('.',appendLF = FALSE)
        }
        message('done\nTrying again...')
      }
      else if (status==401) {
        message("Credenciales")
        status<-200
      }
      else if (status!=200) {
        message('Error, see response ',status)
        return(NULL)
      }
    }
  }
  
  # If it works, then process the data
  message("Success, info of user ",x,' correctly obtained, processing...')
  usr_info <- content(req)
}

# Getting the info
tmp <- lapply(twitterAccounts,"[",1)
n <- length(twitterAccounts)
profiles <- vector("list",n)
for (s in 1:n) {
  # Getting the info
  profiles[[s]] <- tw_api_get_usr_profile(tmp[[s]])
  if (!(s %% 10)) save.image("data/congress_info.RData")
  message(sprintf("%03d of %03d",s,n)," Congressman ",representatives$Name[s]," done...")
}
save.image("data/congress_info.RData")
  