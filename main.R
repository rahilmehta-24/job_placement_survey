## Importing the required libraries

library(reticulate)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(rpart)
library(caret)
library(randomForest)
library(huxtable)
require(FactoMineR)
## Setting up the working directory

setwd("D:/Projects/R/job_placement_survey")

## Loading the csv file

job <- read.csv("D:/Projects/R/job_placement_survey/Job_Placement_Data.csv", sep = ',')
# View(job)

## Creating factors

job$gender<- as.factor(job$gender)
job$ssc_board<- as.factor(job$ssc_board)
job$hsc_board<- as.factor(job$hsc_board)
job$hsc_subject<- as.factor(job$hsc_subject)
job$undergrad_degree<- as.factor(job$undergrad_degree)
job$work_experience<- as.factor(job$work_experience)
job$specialisation<- as.factor(job$specialisation)
job$status<- as.factor(job$status)

str(job)

### 1 Logistic Regression

#divide the dataset in train set and test set
nrow(job)
nrow(job)*0.80

# setseed to keep the same sample and therefore compare models
set.seed(1)
job.idx = sample(215,172)

job.train<-job[job.idx,]
job.test<-job[-job.idx,]

# Define status as numeric binomial variable with 1 = placed and 0 = not placed
job.train$status<-ifelse(job.train$status=="Placed",1,0)
job.test$status<-ifelse(job.test$status=="Placed",1,0)

as.numeric(job.train$status)
as.numeric(job.test$status)

# Train the logistic model
logistic.fit <- glm(status~. , data = job.train, family = "binomial")

# Test the logistic model
logistic.test <- predict(logistic.fit, newdata = job.test, type = "response")

# Accuracy
logistic.pred <- ifelse(logistic.test > 0.5, "1", "0")
t <- table(logistic.pred, job.test$status)
logistic_accuracy = sum(diag(t)/sum(t))
logistic_accuracy


### 2 Decision Tree 

set.seed(1)
job.idx = sample(215,172)

job.train<-job[job.idx,]
job.test<-job[-job.idx,]

summary(job.train)

job.dt.1 = rpart(status~. , data = job.train)
rpart.plot(job.dt.1, extra = 101)

job.predict = predict(job.dt.1, job.test, type = 'class')
accuracy_table = table(job.test$status, job.predict)

accuracy = sum(diag(accuracy_table)/sum(accuracy_table))
accuracy # 0.6976744

accuracy_table

################ Improving Decision Tree #####################

control <- rpart.control(cp = 0, maxdepth = 3)

job.dt.2 <- rpart(status~. -emp_test_percentage -hsc_subject, data = job.train, method = "class", control = control)
rpart.plot(job.dt.2, extra = 101)

#accuracy decision tree improved
job.predict = predict(job.dt.2, job.test, type='class')

prediction_table_impr= table(job.test$status, job.predict)

acc_imp= sum(diag(prediction_table_impr)/sum(prediction_table_impr))
acc_imp # 0.8139535
prediction_table_impr

#############################################################

### 3 Random Forest

#same training and test data
nrow(job)
215*0.8

set.seed(1)                                                 
job.idx = sample(215, 172)

job.train<-job[job.idx,]
job.test<- job[-job.idx,]

#build the random forest
as.numeric(job.train$status)
job.rf = randomForest(status ~ ., data=job.train, ntree = 500)

#random forest accuracy
job.rf.pred = predict(job.rf, job.test)

forest_table=table(job.test$status, job.rf.pred)

acc_forest= sum(diag(forest_table)/sum(forest_table))
acc_forest # 0.8837209

################ Improving Random Forest #####################

#check the numbers of variables and the out of bag error
job.rf

oob.values<-vector(length=10)

for(i in 1:10){
  job.rf2 = randomForest(status ~., data=job.train, mtry= i, ntree= 500)
  oob.values[i]<-job.rf2$err.rate[nrow(job.rf2$err.rate),1]
}
oob.values

job.rf3 = randomForest(status ~., data=job.train, mtry= 2, ntree= 500)

job.rf3.pred = predict(job.rf3, job.test)

forest_table3 = table(job.test$status, job.rf3.pred)
acc_forest3= sum(diag(forest_table3)/sum(forest_table3))
acc_forest3 # 0.9069767