#Install packagaes
install.packages("stats")
install.packages("dplyr", dependencies = TRUE)
install.packages("ggplot2")
install.packages("ggfortify")

# Load required libraries
library(stats) 
library(dplyr)
library(ggplot2)
library(ggfortify)


# To import a dataset
mydata <- read.csv("vehicles.csv")


#viewing the dataset
head(mydata)
head(mydata,15)
tail(mydata)


#dataset pre-processing and view the structure of data, dimensionality, and attribute names
str(mydata) #the structure of the DS
dim(mydata) #checking dimension of the DS
summary(mydata)#checking the summary of the DS
colnames(mydata) #checking column names of the DS


#converting the labelled attribute to type "factor" and checking the structure of the DS to confirm if the type conversion happened
mydata$type <- as.factor(mydata$type)
str(mydata)


#checking if there exsits any attribute with value = N/A
colSums(is.na(mydata))


#prepare for kmeans clustering and excluding the labeled column (type) and displaying the names of the chosen attributes
data=select(mydata,c(1,2,3,4,5)) # c means combine
names(data)

#creating cluster with K=3
kmeans.result3 <- kmeans(data, 3) 
kmeans.result3

plot(data[c("weight", "fuelcap")], col = kmeans.result3$cluster)
points(kmeans.result3$centers[,c("weight", "fuelcap")], col = 1:3,  pch = 8, cex=2) 

plot(data[c("engine", "weight")], col = kmeans.result3$cluster)
points(kmeans.result3$centers[,c("engine", "weight")], col = 1:3,  pch = 8, cex=2) 

#to plot all variables together without trying different combination
plot(data, col = kmeans.result3$cluster)


#creating cluster with K=4
kmeans.result4 <- kmeans(data, 4)
kmeans.result4

plot(data[c("weight", "fuelcap")], col = kmeans.result4$cluster)
points(kmeans.result4$centers[,c("weight", "fuelcap")], col = 1:2,  pch = 8, cex=2)

plot(data[c("engine", "weight")], col = kmeans.result4$cluster)
points(kmeans.result4$centers[,c("engine", "weight")], col = 1:3,  pch = 8, cex=2) 

#to plot all variables together without trying different combination
plot(data, col = kmeans.result4$cluster)


#creating cluster with K=5
kmeans.result5 <- kmeans(data, 5)
kmeans.result5

plot(data[c("weight", "fuelcap")], col = kmeans.result5$cluster)
points(kmeans.result5$centers[,c("weight", "fuelcap")], col = 1:2,  pch = 8, cex=2)

plot(data[c("engine", "weight")], col = kmeans.result5$cluster)
points(kmeans.result5$centers[,c("engine", "weight")], col = 1:3,  pch = 8, cex=2) 

#to plot all variables together without trying different combination
plot(data, col = kmeans.result5$cluster)


#Finding out the optimum number of clusters (Elbow technique)
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
wssplot(data)




