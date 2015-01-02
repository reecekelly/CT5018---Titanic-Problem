## Decision tree packages and libary
install.packages("C50") 
library(C50)

## Installs the package for graphs etc
install.packages("gmodels")
library(gmodels)
library(class)

## Functions
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

## removes entries with n/a
titanicdata = na.omit(titanicdata)

titanicdata_mod <- titanicdata[,c(2:5,1)]

# Changes a column into factor
titanicdata_mod$Survived <- as.factor(titanicdata_mod$Survived)

table(titanicdata_mod$Survived)

# Counting the size of the dataset
numberofrows <- nrow(titanicdata)
numberofcolumns <- ncol(titanicdata)

# create a random sample for training and test data
# use set.seed to use the same random number sequence
set.seed(12345)
titanic_rand <- titanicdata_mod[order(runif(numberofrows)), ]

# Checking to make sure data sets are still identical
summary(titanicdata_mod)
summary(titanic_rand)

# Checking to make sure data sets are ordered differently
head(titanicdata_mod)
head(titanic_rand)

# Splitting the data set 90% modeling - 10% testing
titanic_train <- titanic_rand[1:numberofrows*0.9, ]
titanic_test <- titanic_rand[numberofrows*0.9:714, ]

# Checking for equal split (around 30%)
prop.table(table(titanic_train$Survived))
prop.table(table(titanic_test$Survived))

# Decision Tree model creation
titanic_model <- C5.0(titanic_train[-5], 
                      titanic_train$Survived, 
                      trials = 100)

# Creating useable data for cross table
titanic_pred <- predict(titanic_model, titanic_test)

# Cross table to plot results from Decision Tree model
ctable <- CrossTable(titanic_test$Survived, titanic_pred, 
                     prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                     dnn = c('actual default', 'predicted default'))

fscore <- FScore(ctable$t[1,1],ctable$t[1,2],ctable$t[2,1])
print(fscore) 

## Error scaler code
# Creating a error scaler
error_survived <- matrix(c(0, 1, 4, 0), nrow = 2)

# apply the cost matrix to the tree
titanic_survived <- C5.0(titanic_train[-5], titanic_train$Survived,
                    costs = error_survived)

titanic_cost_pred <- predict(titanic_survived, titanic_test)

update <- CrossTable(titanic_test$Survived, titanic_cost_pred,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))

fscore <- FScore(update$t[1,1],update$t[1,2],update$t[2,1])
print(fscore) 