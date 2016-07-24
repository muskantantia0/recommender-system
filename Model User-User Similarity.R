# Set the Working data

#Load the Libraries 
library(dplyr)
library(plyr)

# Assumptions 
# 1. We are Using Pearson-Correlation to find the Item-Item Similarity.
# 2. We are Using the Similarity to find the results. We have scaled the data with mean before finding the
#   correlation

#Laod the data
ratingsdummydata <- read.csv("ratingsdummydata.csv",sep = ",",header = TRUE)
rownames(ratingsdummydata) <- ratingsdummydata[,1]
ratingsdummydata <- ratingsdummydata[,-1]
str(ratingsdummydata)
ratingsdummydata <- as.data.frame(t(ratingsdummydata))

#Create a dummy martix 
Users <- as.list(colnames(ratingsdummydata))
pearson_correlation <- matrix(data = NA , nrow = ncol(ratingsdummydata), ncol = ncol(ratingsdummydata), dimnames = list(c(Users),c(Users)))

#Creating another data frame by removing users from the previous ratingsdummydata


# Calculate the correlations 
for(i in 1:length(ratingsdummydata)){
  for(j in 1:length(ratingsdummydata)){
    P1 <- ratingsdummydata[,i]
    P2 <- ratingsdummydata[,j]
    data <- cbind(P1,P2)
    data <- data.frame(data)
    data <- na.omit(data)
    if(length(data) == 0){
      pearson_correlation[i,j] <- NA
    }
    else{
      mean1 <- mean(data$P1,na.rm = TRUE)
      mean2 <- mean(data$P2,na.rm = TRUE)
      data$P1 <- data$P1 - mean1
      data$P2 <- data$P2 - mean2
      data <- mutate(data, P1P2 = P1*P2)
      data <- mutate(data, P1sq = P1^2)
      data <- mutate(data, P2sq = P2^2)
      pearson_correlation[i,j] <- sum(data$P1P2)/((sum(data$P1sq)*sum(data$P2sq))^0.5)
      P1 <- NULL
      P2 <- NULL
    }
    
  }
}


# Predictions 
# Created the same data frame with another name so that 

Predictions <- ratingsdummydata

for ( i in 1:nrow(Predictions)){
  for ( j in 1:ncol(Predictions)){
    if(is.na(ratingsdummydata[i,j])== TRUE){
      x <- colnames(ratingsdummydata)
      x <- x[j]
      x <- which(colnames(ratingsdummydata) == x)
      matrix1row <- pearson_correlation[j,]
      matrix1row <- matrix1row[-x]
      matrix1row <- sort(matrix1row,decreasing = TRUE)
      # Finding and removing the columns where NA's are there in correlation matrix and ratingsdummydata.
      m <- ratingsdummydata[i,c(names(matrix1row))]
      z <- which(is.na(ratingsdummydata[i,c(names(matrix1row))]))
      if(length(z) == 0){
        m <- m
      }
      else{
        m <-m[-z]
      }
      m <- pearson_correlation[j,c(names(m))]
      z <- which(is.na(pearson_correlation[j,c(names(m))]))
      if(length(z) == 0){
        m <- m
      }
      else{
        m <-m[-z]
      }
      Predictions[i,j]<-((abs(pearson_correlation[j,names(m)]) %*% t(ratingsdummydata[i,names(m)])))/sum(abs(pearson_correlation[j,names(m)]))
    }
    else{
      Predictions[i,j] <- NA
    }
  }
}

#Final Predictions
# Two DataFrames
# ratingsdummydata - User rated ratings
# Predictions           - User Predicted Ratings
Predictions <- t(Predictions)

# Converting the NaN's into NA values for easy off Predictions.
Predictions <- as.matrix(Predictions)
Predictions[is.nan(Predictions)] <- NA
Predictions <- as.data.frame(Predictions)

# For time Being we will give the number of the Customer Instead of his name.
Username <- function(x){
  UserPredictions <- Predictions[x,1:ncol(Predictions)]
  meanuserrating <- mean(as.matrix(ratingsdummydata[x,1:ncol(ratingsdummydata)]),na.rm = TRUE)
  GreaterthanMeanNum <- which(Predictions[x,1:ncol(Predictions)] <= meanuserrating)
  if(length(GreaterthanMeanNum) == 0){
    UserPredictions <- UserPredictions[colSums(!is.na(UserPredictions)) > 0]
    UserPredictions <- sort(UserPredictions,decreasing = TRUE)
  }else{
    UserPredictions <- UserPredictions[-(GreaterthanMeanNum)]
    UserPredictions <- sort(UserPredictions[-which(is.na(UserPredictions))],decreasing = TRUE)
  }
  if(length(UserPredictions) == 0){
    return("No Predictions")
  }
  else{
    return(names(UserPredictions))
  }
}

# The Code Works fine and handles the NA values but the Question which is left is ?
# 1. How is NaN different from NA in Correlation Matrix and how does it effect our Prediction?
