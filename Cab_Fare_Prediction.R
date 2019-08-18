#remove all the objects stored
rm(list=ls())
library(tidyverse)
library(lubridate)
library(caret)
library(geosphere)



#set current working directory
setwd("D:/Edvisor/Project/Cab Prediction")


#Current working directory
getwd()

#Install multiple packages at a time
install.packages(c("dplyr","plyr","reshape","ggplot2","data.table"))


##Load data in R
data_train = read.csv("train_cab.CSV", header = T) 

data_test = read.csv("test.CSV", header = T) 


#Getting the column names of the data_train
colnames(data_train)
colnames(data_test)



#Getting the structure of the data_train
str(data_train)
str(data_test)



#Getting the number of variables and obervation in the data_trains
dim(data_train)
dim(data_test)
data_train$fare_amount<-as.numeric(paste(data_train$fare_amount))

#Getting first 5 rows of the data_train

head(data_train, 5)
head(data_test, 5)

#Getting the last 5 rows of the data_train
tail(data_train, 5)

#data type
class(data_train)
class(data_test)

#Summary of a varaible 
summary(data_train)

data_train <- mutate(data_train,
                     pickup_datetime = ymd_hms(pickup_datetime),
                     month = as.factor(month(pickup_datetime)),
                     year = as.factor(year(pickup_datetime)),
                     Weekday = as.factor(wday(pickup_datetime)),
                     hour = hour(pickup_datetime),
                     day = as.factor(day(pickup_datetime)),
                     
                     hour = as.factor(hour(pickup_datetime)))

data_test <- mutate(data_test,
                    pickup_datetime = ymd_hms(pickup_datetime),
                    month = as.factor(month(pickup_datetime)),
                    year = as.factor(year(pickup_datetime)),
                    Weekday = as.factor(wday(pickup_datetime)),
                    hour = hour(pickup_datetime),
                    day = as.factor(day(pickup_datetime)),
                    
                    hour = as.factor(hour(pickup_datetime)))





#########################################Missing Valu Analysis#######################################################

Missing_Value=sum(is.na(data_train))
# No Missing Value
Missing_Value

na_count <-sapply(data_train, function(y) sum(length(which(is.na(y)))))



data_train<-na.omit(data_train)
dim(data_train)
dim(data_test)

# #####################################Splitting up pickup_datetime 


str(data_train)
str(data_test)

head(data_train,5)
head(data_test,5)


############################################### Outlier Detection#######################################################

# Fare cant be Zero, 
str(data_train)
dim(data_train)
data_train <- data_train[data_train$fare_amount > 0, ]  
data_train <- data_train[data_train$fare_amount <= 300, ]  

data_train <- data_train[data_train$passenger_count > 0, ]
data_train <- data_train[data_train$passenger_count < 7, ]


# Remove the lat/lon from the training data that are outside of what's present in the data_test data
data_train <- data_train[data_train$pickup_latitude < max(data_test$pickup_latitude), ]
data_train <- data_train[data_train$pickup_latitude > min(data_test$pickup_latitude), ]
data_train <- data_train[data_train$dropoff_latitude < max(data_test$dropoff_latitude), ]
data_train <- data_train[data_train$dropoff_latitude > min(data_test$dropoff_latitude), ]
data_train <- data_train[data_train$pickup_longitude > min(data_test$pickup_longitude), ]
data_train <- data_train[data_train$pickup_longitude < max(data_test$pickup_longitude), ]
data_train <- data_train[data_train$dropoff_longitude > min(data_test$dropoff_longitude), ]
data_train <- data_train[data_train$dropoff_longitude < max(data_test$dropoff_longitude), ]



#####################################3 Calculating Distance################################################33


head(data_train,5)

data_train$distance<-(distHaversine(data_train[,3:4], data_train[,5:6]))/1000
data_test$distance<-(distHaversine(data_test[,2:3], data_test[,4:5]))/1000

head(data_test,5)
head(data_test,5)

summary(data_train)
summary(data_test)


head(data_train,50)
head(data_test,50)


#data_train<-na.omit(data_train)
dim(data_train)
################################################  
## Dimension Reduction

colnames(data_train)
colnames(data_test)
data_train = subset(data_train,select = -c(pickup_datetime))
data_test = subset(data_test,select = -c(pickup_datetime))
str(data_train)
str(data_test)


View(data_train)
View(data_test)

dim(data_train)
dim(data_test)

################################### Linear Regression####################################333

#Linear Regression
#install.packages("usdm")
library(usdm)

train_index = sample(1:nrow(data_train), 0.8 * nrow(data_train))
train = data_train[train_index,]
test = data_train[-train_index,]


#run regression model

lm_model = lm(fare_amount ~., data = train)



#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, test[,-1])

#Calculate MAPE

#MAPE
#calculate MAPE
MAPE = function(y,yhat){
mean(abs((y - yhat)/y)*100)
  }
Error=MAPE(test[,1],predictions_LR)
Accuracy=100-Error

# Error=30.78
# Accuracy=69.21
#Multiple R-squared:  0.6228,	Adjusted R-squared:  0.6203 
#F-statistic: 248.8 on 82 and 12360 DF,  p-value: < 2.2e-16

#######################################################################Decision TRee#################################################################

##Decision Tree

install.packages('rpart')
install.packages('MASS')
library(rpart)
library(MASS)

head(data_train,5)

#Divide the data into train and test
#set.seed(123)

train_index = sample(1:nrow(data_train), 0.8 * nrow(data_train))
train = data_train[train_index,]
test = data_train[-train_index,]

# ##rpart for regression
fit = rpart(fare_amount ~ ., data = train, method = "anova")

#Predict for new test cases

predictions_DT = predict(fit, test[,-1])

#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y)*100)
  
  
}

Error=MAPE(test[,1], predictions_DT)

Accuracy=100-Error

#Error= 26.87%
#Accuracy = 73.12%

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

rpart.plot(fit)

# prune the tree 
pfit<- prune(fit, cp=0.010000) # from cptable 
predictions_pfit = predict(pfit, test[,-1])

Error1=MAPE(test[,1], predictions_pfit)
rpart.plot(pfit)

#Error= 26.87%
#Accuracy = 73.12%

############################################ Random Forest ############################################################
install.packages("randomForest")

#Divide the data into train and test
#set.seed(123)

train_index = sample(1:nrow(train), 0.8 * nrow(data_train))
train = data_train[train_index,]
test = data_train[-train_index,]

# ##rpart for regression
fit=randomForest(fare_amount ~ ., data = train)


#Predict for new test cases

predictions_RF = predict(fit, test[,-1])

#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y)*100)
  
  
}

Error=MAPE(test[,1], predictions_RF)

Accuracy=100-Error

#Error= 19.87%
#Accuracy = 80.12%






