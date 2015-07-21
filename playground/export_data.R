################################################################################
# Exports data sets
################################################################################
rm(list=ls())
load("data/congress_info.RData")
load("data/senate_info.RData")

# Mergin with twitter data and subset california
senators <- cbind(
  senators,
  senators_profile
)

representatives <- cbind(
  representatives,
  representatives_profile
)

# Removing status column (nasty) and fixing colnames
representatives <- subset(representatives,select=-tw_status)
senators <- subset(senators,select=-tw_status)

# Fixing types
for (i in 1:ncol(representatives)) {
  if (is.list(representatives[,i]))
    representatives[,i] <- as.character(representatives[,i])
}

for (i in 1:ncol(senators)) {
  if (is.list(senators[,i]))
    senators[,i] <- as.character(senators[,i])
}

# Fixing names
colnames(representatives) <- make.names(colnames(representatives))
colnames(senators) <- make.names(colnames(senators))

ca_representatives <- subset(representatives,subset=state=='California')



# Saving datasets
write.table(senators,file = 'data/us_senators.csv',row.names=FALSE)
write.table(representatives,file = 'data/us_representatives.csv',row.names=FALSE)
write.table(ca_representatives,file = 'data/ca_representatives.csv',row.names=FALSE)
