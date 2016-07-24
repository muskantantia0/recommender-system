# KNN

# Set WD
library(dplyr)

# Load the dummy dataset
ratingsdummydata <- read.csv("ratingsdummydata.csv",header = TRUE,sep = ",")
rownames(ratingsdummydata) <- ratingsdummydata[,1]
ratingsdummydata <- ratingsdummydata[,-1]
str(ratingsdummydata)


## Item - Item Similarity

#Create a dummy martix 
Users <- as.list(colnames(ratingsdummydata))
euclidean_distance <- matrix(data = NA , nrow = ncol(ratingsdummydata), ncol = ncol(ratingsdummydata), dimnames = list(c(Users),c(Users)))



# Calculate the correlations 
for(i in 1:length(ratingsdummydata)){
  for(j in 1:length(ratingsdummydata)){
    P1 <- ratingsdummydata[,i]
    P2 <- ratingsdummydata[,j]
    data <- cbind(P1,P2)
    data <- data.frame(data)
    data <- na.omit(data)
    if(length(data) == 0){
      euclidean_distance[i,j] <- NA
    }
    else{
      data <- mutate(data, distance = (P1-P2)^2)
      euclidean_distance[i,j] <- (sum(data$distance))^0.5
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
      x <- x[2]
      x <- which(colnames(ratingsdummydata) == x)
      matrix1row <- euclidean_distance[j,]
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
      m <- euclidean_distance[j,c(names(m))]
      z <- which(is.na(euclidean_distance[j,c(names(m))]))
      if(length(z) == 0){
        m <- m
      }
      else{
        m <-m[-z]
      }
      if(length(m) >= 3){
        m <- sort(m,decreasing = TRUE)
        m <- m[1:3]
        Predictions[i,j]<- (0.5*ratingsdummydata[i,names(m[1])])+(0.3*ratingsdummydata[i,names(m[2])])+(0.2*ratingsdummydata[i,names(m[3])]) # Giving Higher weightage to the most
        # 
        
      }
      else{
        Predictions[i,j] <- ratingsdummydata[i,names(m[1])]
      }
    }
    else{
      Predictions[i,j] <- NA
    }
  }
}


## User - User Similarity is the same as above, Just transpose the inital matrix and perform the same 
## operations

