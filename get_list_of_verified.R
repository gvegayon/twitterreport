library(RCurl)
library(XML)
library(stringr) 

get_verified <- function(uri="https://twitter.com/verified/lists/us-congress/members") {
  getURLContent(url=uri,)
}

accounts <- getURLContent(url="https://twitter.com/verified/lists/us-congress/members")
accounts <- htmlParse(accounts)
xpathSApply(accounts,path = '//*[starts-with(@class,"user-actions btn-group")]',xmlGetAttr,'data-name')
