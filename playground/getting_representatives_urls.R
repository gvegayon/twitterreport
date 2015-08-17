rm(list=ls())

library(RCurl)
library(XML)
library(stringr)
library(twitterreport)
library(dplyr)

################################################################################
# Data base from house.gov
################################################################################
# Key to Room Codes
# CHOB: Cannon House Office Building
# LHOB: Longworth House Office Building
# RHOB: Rayburn House Office Building

# Getting the table
representatives <- readHTMLTable("http://www.house.gov/representatives/",stringsAsFactors=FALSE)
representatives <- bind_rows(representatives)

# Getting the info (URLS)
house <- getURLContent("http://www.house.gov/representatives/")

representatives <- cbind(
  representatives,
  website=sapply(representatives$Name, function(x,...) {
    str_extract(house,paste0('https?://[a-zA-Z]+\\.house\\.gov(?=/?">\\s*',x,")"))}
  ))

# Looking for state names
states <- str_extract_all(house,"(?<=state\\_[a-z]{2}.{2})([a-zA-Z ]{2,})")[[1]]
tmp <- str_detect(representatives$District,paste0(states,collapse="|"))
representatives <- representatives[tmp,]

representatives$state <- str_extract(representatives$District,paste0(states,collapse="|"))

representatives$DistrictNum <- as.numeric(str_match(representatives$District,'[0-9]+(?=[a-z]{2} District)'))
rm(tmp)

row.names(representatives) <- 1:nrow(representatives)

# Making all column names as lowercase
colnames(representatives) <- tolower(colnames(representatives))

################################################################################
# Getting twitter accounts
################################################################################
# Loop
twitter_accounts_house <- vector("list",nrow(representatives))
n <- length(twitter_accounts_house)
for (i in 1:n) {
  twitter_accounts_house[[i]] <- tw_get_tw_account(representatives$website[i],
                                                   quiet=FALSE)
}

# Modifying manually 
tmp <- twitter_accounts_house
names(tmp) <- paste(1:length(tmp),representatives$name)

# Checking out multiple ones
tmp[which(sapply(tmp, length) > 1)]

# List of the first account ok
ok <- c(5,18,30,33,36,59,65,87,97,120,129,151,152,158,161,163,172,
        184,201,203,205,208,217,222,226,236,238,246,263,270,328,334,
        349,362,380,387,394,398,431)
tmp[ok] <- lapply(tmp[ok], "[[", 1)

# Manually fixing accounts
tmp[[84]]  <- tmp[[84]][5]
tmp[[130]] <- tmp[[130]][4]
tmp[[316]] <- tmp[[316]][3]
tmp[[350]] <- tmp[[350]][2]
tmp[[432]] <- tmp[[432]][3]

################################################################################
# Checking out empty ones (we'll use the API to search for them)
pending_index <- which(sapply(tmp, length) == 0)
pending_names <- gsub("^[0-9]+\\s+","",names(tmp[pending_index]))

pending <- vector("list",length(pending_names))

source("R/credentials.R")
for (i in 1:length(pending)) {
  x <- tw_api_get_users_search(pending_names[i],key)[,c("name","screen_name","verified","followers_count")]
  if (!is.null(x)) tmp[[pending_index[i]]] <- x
}

# The ones that I'm not sure about
head(lapply(tmp[pending_index], head, 1))
notok <- c(66,77,78,177,271,291,309,352,365,409,433)
ok <- pending_index[which(!(pending_index %in% notok))]

tmp[ok] <- lapply(tmp[ok],function(x) {
  if (length(x))x$screen_name[1]
  else NULL
  })

# save.image("data/20150815_working.rdata")

# Doesn't has a twitter account
tmp[[66]] <- NA
tmp[[77]] <- tmp[[77]]$screen_name[1]
tmp[[78]] <- tmp[[78]]$screen_name[3]
tmp[[177]] <- tmp[[177]]$screen_name[4]
tmp[[271]] <- "CongressmanRug"
tmp[[291]] <- "RepRichNugent"
tmp[[309]] <- "reppittenger"
tmp[[352]] <- NA
tmp[[365]] <- tmp[[365]]$screen_name[7]
tmp[[409]] <- "NydiaVelazquez"
tmp[[433]] <- "RepRobWoodall"


representatives$screen_name <- unlist(tmp)
representatives$screen_name <- tolower(representatives$screen_name)

save(representatives,file="data/congress_info.rdata")

################################################################################
# Getting account info from API
################################################################################

# Getting the info
n <- nrow(representatives)
twitter_profiles_congress <- vector("list",n)
for (s in 1:n) {
  account <- representatives$screen_name[s]
  
  if (is.na(account)) next
  
  # Getting the info
  twitter_profiles_congress[[s]] <- tw_api_get_users_show(account,key)
  
  if (!(s %% 10)) save(twitter_profiles_congress,file="data/twitter_profiles_congress.rdata")
}
tmp <- do.call(rbind, twitter_profiles_congress);nrow(tmp)

# Before mergin making sure it is going to merge
tmp$screen_name <- tolower(tmp$screen_name)
tmp <- dplyr::left_join(representatives, tmp, by="screen_name")
twitter_profiles_congress<-tmp

save(twitter_profiles_congress,file="data/twitter_profiles_congress.rdata")
