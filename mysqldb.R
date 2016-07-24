# Load the libraries 
library(tidyr) # Thanks for Hardley
library(dplyr)
library(RMySQL) # Install any of the packages if they are not their in your R library


# Connecting to mysql. Ask your administrator for password, user, dbname and host.
con <- dbConnect(MySQL(),
                 user="root", password="lokesh.143",
                 dbname="recsys", host="localhost")

# Send the Query to DB
rs <- dbSendQuery(con, "SELECT * FROM recsys.UserRating;")

# Fetch the data
datafrommysqldb <- fetch(rs, n=113)

# Remove the Timestamp as we are not using it here for analysis.
datafrommysqldb <- datafrommysqldb[,-4]

on.exit(dbDisconnect(con)) # Disconnect the DB

# Now the Data is in your R memory. Do the computations.
# Reshaping the data, so that the column names are Products(Movies) and Values are turned into Rating.
ratingsdummydata <- spread(data,Product,Rating)


# Data- Processing to make rownames as username and colnames as Product names.
rownames(ratingsdummydata) <- ratingsdummydata[,1]
ratingsdummydata <- ratingsdummydata[,-1]
str(ratingsdummydata)


#Create a dummy martix 
Users <- as.list(colnames(ratingsdummydata))
pearson_correlation <- matrix(data = NA , nrow = ncol(ratingsdummydata), ncol = ncol(ratingsdummydata), dimnames = list(c(Users),c(Users)))



# Calculate the correlations -- Pearson Correlation
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


# Gather the data, so that we can get rid of NA values.
Users <- rownames(Predictions)
Predictions <- cbind(Users,Predictions)
Predictions_update = gather(Predictions,Product, Ratings,-Users, na.rm = TRUE, convert = FALSE)



#Reconnect to the database and write the data to MySQL.
con <- dbConnect(MySQL(),
                 user="root", password="lokesh.143",
                 dbname="recsys", host="localhost")

#name will create a table and value will update the values.
dbWriteTable(con, name='predictions', value=Predictions_update)

# Computations Done and file uploaded to MySQL Database.

# You can do the samething for User-User Similarity matrix also.

