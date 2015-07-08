library(RCurl)
library(XML)
library(stringr) 

# Getting the info
get_verified <- function(uri="https://twitter.com/verified/lists/us-congress/members?scrolled&set=1") {
  getURLContent(url=uri)
}

# Parsing info
accounts <- getURL(url="https://twitter.com/verified/lists/us-congress/members?scrolled&set=1")
accounts <- htmlParse(accounts)

# Getting the data
congress<- data.frame(name=xpathSApply(accounts,path = '//*[starts-with(@class,"user-actions btn-group")]',xmlGetAttr,'data-name'))
congress$screen_name <- xpathSApply(accounts,path = '//*[starts-with(@class,"username js")]',getChildrenStrings)

save(congress,file="data/congress.RData")
