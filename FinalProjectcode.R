#setwd
setwd("")

# Load the libraries

#Set Colnames
colnames(ratings_Books) <- c("Users","Product","Ratings","TimeStamp")

#Timestamp
data <- ratings_Books
data$TimeStamp<-as.Date(as.POSIXct(data$TimeStamp, origin="1970-01-01"))

# Total Number of Unique Users 
UniqueUsers <- length(unique(data$Users))

# Total Number of Unique Products
UniqueProducts <- length(unique(data$Product))

# Maximum and Minimum time in the dataset
maxtime <- max(data$TimeStamp)
mintime <- min(data$TimeStamp)
Totaltime <- as.integer(maxtime-mintime)

#RF Analysis 
# Recency and Frequency
#Frequency of Users 
FrequencyUser = sqldf("select Users, count(Users) as Frequency from data group by Users")


#Recency of Users
RecencyUser = data %>% group_by(Users) %>% summarise(lastpurchasedate = max(TimeStamp))
maxdate = max(data$TimeStamp)+1
RecencyUser = mutate(RecencyUser, Recency = maxdate - lastpurchasedate)
RecencyUser = RecencyUser[,c(1,3)]
RecencyUser$Recency <- as.numeric(RecencyUser$Recency)


#Frequency of Products
FrequencyProduct = sqldf("select Product, count(Product) as Frequency from data group by Product")


#Recency of Products 
RecencyProduct = data %>% group_by(Product) %>% summarise(lastpurchasedate = max(TimeStamp))
maxdate = max(data$TimeStamp)+1
RecencyProduct = mutate(RecencyProduct, Recency = maxdate - lastpurchasedate)
RecencyProduct = RecencyProduct[,c(1,3)]
RecencyProduct$Recency <- as.numeric(RecencyProduct$Recency)


# Joining Recency and frequency of product 
RF_Product <- sqldf("select FP.Product,Frequency,Recency from FrequencyProduct as FP, RecencyProduct as RP where FP.Product = RP.Product")
RF_Users <- sqldf("select FU.Users,Frequency,Recency from FrequencyUser as FU, RecencyUser as RU where FU.Users = RU.Users")

#Clustering the Users 
RFscore = RF_Users
rownames(RFscore) <- RFscore[,1]
RFscore <- RFscore[,-1]
RFscore <- scale(RFscore)

Kmeans_result <- kmeans(RFscore,centers = 4)
cluster = Kmeans_result$cluster
RF_Users_final <- cbind(RF_Users,cluster)

#Clustering the Products
RFscore = RF_Product
rownames(RFscore) <- RFscore[,1]
RFscore <- RFscore[,-1]
RFscore <- scale(RFscore)

Kmeans_result <- kmeans(RFscore,centers = 4)
cluster = Kmeans_result$cluster
RF_Product_final <- cbind(RF_Product,cluster)
