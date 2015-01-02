# Installs the package for graphs etc
#install.packages("gmodels")
library(gmodels)
# load the "class" library
library(class)

## Functions
# Normalise Function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# FScore Function
FScore <- function(TP, FP, FN) {
  TPS <- 2*TP
  return ((TPS) / ((TPS)+FP+FN))
}

# Setup directory
setwd('E:/CT5018 Data Analytics/Titanic Machine Learning from Disaster/data')
titanicdata <- read.csv("train.csv", stringsAsFactors=FALSE)
str(titanicdata)

# Remove excess features
titanicdata = titanicdata[-c(1,4,7:9,11:12)]
titanicdata = na.omit(titanicdata)

# Sample Data
head(titanicdata)
summary(titanicdata)

# Table of count
table(titanicdata$Survived)

# normalize te 'titanic' data
titanicdata[4] <- as.data.frame(lapply(titanicdata[4], normalize))
titanicdata[5] <- as.data.frame(lapply(titanicdata[5], normalize))

# changes sex column values to useable data entries
titanicdata$Sex <- sub("female", 1, titanicdata$Sex)
titanicdata$Sex <- sub("male", 0, titanicdata$Sex)

# Converts the factors into numerical values
titanicdata$Sex <- as.numeric(titanicdata$Sex)
titanicdata$Pclass <- as.numeric(titanicdata$Pclass)

# summarize the columns
summary(titanicdata)

# Counting the size of the dataset
numberofrows <- nrow(titanicdata)
numberofcolumns <- ncol(titanicdata)

titanicdata_train <- titanicdata[1:(numberofrows*0.75), 2:5]
titanicdata_test <- titanicdata[(numberofrows*0.75):numberofrows, 2:5]

# Create labels for training and test data
titanicdata_train_labels <- titanicdata[1:(numberofrows*0.75), 1]
titanicdata_test_labels <- titanicdata[(numberofrows*0.75):numberofrows, 1]

# Variable k
nValue <- 27

# knn model creation
titanicdata_test_pred <- knn(train = titanicdata_train, test = titanicdata_test, cl = titanicdata_train_labels, k=nValue)

# Displays the results of the knn model in a cross table
ctable <- CrossTable(titanicdata_test_labels, titanicdata_test_pred, prop.chisq=FALSE)

fscore <- FScore(ctable$t[1,1],ctable$t[1,2],ctable$t[2,1])
print(fscore)  