library(plyr)

data <- contentdata
rownames(data)<- data[,1]
data <- data[,-1]

# Content User Import

# Calculating IDF 
DF <- colSums(data)
IDF <- log10(10/DF)

# Scale data
for(i in 1:nrow(data)){
  data[i,] <- data[i,]/(sum(data[i,]))^0.5
}

# User Profiles
data_User <- contentuser
data_User[is.na(data_User)] <- 0

UserProfile <- matrix(data=NA , ncol = ncol(data),nrow= ncol(data_User))

# data --- >  6 Articals , 5 Key words
# data _ User --- > 6 Impressions , 2 Users 

# Algo
for( i in 1:ncol(data_User)){
  for( j in 1:(nrow(data)-1)){
    UserProfile[i,j] <- sum(c(data[,j])*c(data_User[,i]))
  }
}
UserProfile <- as.data.frame(UserProfile)
# Predictions 

UserPredictions <- matrix(data = NA,ncol = ncol(data), nrow=ncol(data_User))

for( i in 1:ncol(data_User)){
  for( j in 1:(nrow(data)-1)){
    UserPredictions[i,j] <- sum((data[j,])*(UserProfile[i,])*IDF)
  }
}

