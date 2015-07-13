rm(list=ls())
load("data/congress_info.RData")
load("data/senate_info.RData")

# Checking out more common accounts found at the website of the senate and
# congress
common_accounts <- as.data.frame(table(c(
  unlist(twitter_accounts_senate,recursive=TRUE),
  unlist(twitter_accounts_house,recursive=TRUE)
  )))

common_accounts <- common_accounts[order(-common_accounts$Freq),]
subset(common_accounts, subset=Freq>=2)
