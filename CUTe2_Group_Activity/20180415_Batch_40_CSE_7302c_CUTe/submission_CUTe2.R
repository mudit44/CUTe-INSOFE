rm(list=ls(all=TRUE))

#read the data
data <- read.csv('Data_20180408.csv',header = TRUE,na.strings = c(-99,NA,"NA","NA "," NA"))

#EDA(Explorative Data Analysis)
str(data)
summary(data)
dim(data)

#step 1 - check for NA's in the dataset.
sum(is.na(data))

#step 2 - check for NA's in target field. (here 0)
sum(is.na(data$Target))

#step 3 - check for NA's in all the columns, count them up.
sapply(data, function(x) sum(is.na(x)))

#step 4 - check for unique values/levels in the columns for checking whether a 
#field is numerical or categorical.
sapply(data, function(x) range(x))

#step 5 - convert variables to factors.
data$A11 <- as.factor(data$A11)
data$A13 <- as.factor(data$A13)
data$A17 <- as.factor(data$A17)
data$A18 <- as.factor(data$A18)
data$A19 <- as.factor(data$A19)
data$A20 <- as.factor(data$A20)
data$A22 <- as.factor(data$A22)
data$Target <- as.factor(data$Target)

#step 6 - binning the data.
summary(data)

library(infotheo)
bin_freq <- discretize(data$A21,disc = "equalfreq",nbins = 4)
table(bin_freq)

tapply(data$A21,bin_freq,min)
tapply(data$A21,bin_freq,max)

bin_wid <- discretize(data$A21,disc = "equalwidth",nbins = 4)
table(bin_wid)

tapply(data$A21,bin_wid,min)z
tapply(data$A21,bin_wid,max)

#aggregate numeric and factors in different data frames.
#data_cat <- data[,sapply(data,is.factor)]
#data_num <- data[,sapply(data,is.numeric)]

#impute the data.
library(DMwR)
data <- centralImputation(data)

#cross checking.
sum(is.na(data))

#making ID equal to 0.
data$ID <- NULL

#split the data into train and test.
rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,round((70*nrow(data))/100))
train = data[trainRows,] 
test = data[-trainRows,]

summary(train)
names(train)

# log_reg <- glm(Target~., data = train, family = binomial)
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
# contrasts can be applied only to factors with 2 or more levels
# This type of error will be thrown in case of a variable which has only one value.
# Here A21, it has value of 27 as a whole, so we will be dropping it.
train$A11 <- NULL
test$A11 <- NULL

# #building a logistic regression on top of the preprocessed dataset.
# log_reg = glm(Target ~ ., data = train, family=binomial)
# summary(log_reg)

# throwing some warning
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred
# come up with google search
# https://stackoverflow.com/questions/8596160/why-am-i-getting-algorithm-did-not-converge-and-fitted-prob-numerically-0-or

#from this particular link there is a rare case where result is positive in A13,A18,A22
#storing the variables in another variable and dropping it from train and test set

tr13 <- train$A13
tr18 <- train$A18
tr22 <- train$A22

te13 <- test$A13
te18 <- test$A18
te22 <- test$A22

#dropping attributes from train and test.

train$A13 <- NULL
train$A18 <- NULL
train$A22 <- NULL

test$A13 <- NULL
test$A18 <- NULL
test$A22 <- NULL

#re-running the model after tuning the data
log_reg = glm(Target ~ ., data = train, family=binomial)
summary(log_reg)

# Prediction on train data
prob_train <- predict(log_reg, type = "response")
# Predicting on test data
prob_test <- predict(log_reg, test, type = "response")

#build ROC
library(ROCR) 
pred <- prediction(prob_train, train$Target)
perf <- performance(pred, measure="tpr", x.measure="fpr")

#Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)

pred_class <- ifelse(prob_train > 0.35, 1, 0)
table(train$Target,pred_class)

prob_val <- predict(log_reg, test, type = "response")
preds_val <- ifelse(prob_val > 0.35, 1, 0)
table(preds_val)


conf_matrix <- table(test$Target, preds_val)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)

library(caret)
# Using the argument "Positive", we can get the evaluation metrics according to our positive #referene level
confusionMatrix(preds_val, test$Target,positive="1")

summary(log_reg)

#Use vif to find any multi-collinearity
library(car)
log_reg_vif = vif(log_reg)
log_reg_vif

#dropping high vif for removing multi-collinearity
drops <- c("A5","A6","A7","A9","A10","A12")
train <- train[ , !(names(train) %in% drops)]
test <- test[ , !(names(train) %in% drops)]

#re-running the model after tuning the data
log_reg = glm(Target ~ ., data = train, family=binomial)
summary(log_reg)

#Improve the model using stepAIC
library(MASS)
log_reg_step = stepAIC(log_reg, direction = "both")
summary(log_reg_step)

# Prediction on train data
prob_train <- predict(log_reg, type = "response")
# Predicting on test data
prob_test <- predict(log_reg, test, type = "response")

#build ROC
library(ROCR) 
pred <- prediction(prob_train, train$Target)
perf <- performance(pred, measure="tpr", x.measure="fpr")

#Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)

pred_class <- ifelse(prob_train > 0.35, 1, 0)
table(train$Target,pred_class)

prob_val <- predict(log_reg, test, type = "response")
preds_val <- ifelse(prob_val > 0.35, 1, 0)
table(preds_val)

conf_matrix <- table(test$Target, preds_val)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)

library(caret)
# Using the argument "Positive", we can get the evaluation metrics according to our positive #referene level
confusionMatrix(preds_val, test$Target,positive="1")

# by using preProcess function in caret library, will preprocess the train and test
# and then rebuild the model

# so split the data into train and test and then by using preProcess re-build a model
# split the data into train and test.
table(train$Target)
table(test$Target)
confusionMatrix(preds_val, test$Target,positive="1")

# So, after reading the results reading the data in other variable named data2
data2 <- read.csv('Data_20180408.csv',header=T,na.strings = c(-99,"NA"," ","NA "," NA"))

sum(is.na(data2))

data2$A11 <- as.factor(data2$A11)
data2$A13 <- as.factor(data2$A13)
data2$A17 <- as.factor(data2$A17)
data2$A18 <- as.factor(data2$A18)
data2$A19 <- as.factor(data2$A19)
data2$A20 <- as.factor(data2$A20)
data2$A22 <- as.factor(data2$A22)
data2$Target <- as.factor(data2$Target)

data2$ID <- NULL
data2$A11 <- NULL
data2$A13 <- NULL
data2$A18 <- NULL
data2$A22 <- NULL

rm('train3','test3','train_rows2')

# splitting the data into train and test using caret package
library(caret)
set.seed(123)
train_rows2 <- createDataPartition(data2$Target, p = 0.7, list = F)
train3 <- data2[train_rows2, ]
test3 <- data2[-train_rows2, ]
str(train3)

#seperate the categorical and numerical in train set
train_cat <- train3[,sapply(train3,is.factor)]
train_num <- train3[,sapply(train3,is.numeric)]

#seperate the categorical and numerical in test set
test_cat <- test3[,sapply(test3, is.factor)]
test_num <- test3[,sapply(test3,is.numeric)]

#check for nulls in train (cat/num)
sum(is.na(train_num))
sum(is.na(train_cat))

#check for nulls in test (cat/num)
sum(is.na(test_num))
sum(is.na(test_cat))

# # You don't need mode because categorical ones dont have NA values.
# # Simply impute the mean of train set into test set.
# # #finding mode function
# # getmode <- function(v) {
# #   uniqv <- unique(v)
# #   uniqv[which.max(tabulate(match(v, uniqv)))]
# # }
 
# imputing the NA's in columns of train2 with means of train2.

m1 <- mean(train_num$A2 , na.rm = T)
m2 <- mean(train_num$A15 , na.rm = T)
m3 <- mean(train_num$A16 , na.rm = T)

train_num$A2[which(is.na(train_num$A2))]   <- m1
train_num$A15[which(is.na(train_num$A2))]   <- m2
train_num$A16[which(is.na(train_num$A2))]   <- m3

# imputing the NA's of test with mean of train.
test_num$A2[which(is.na(test_num$A2))]   <- m1
test_num$A15[which(is.na(test_num$A2))]   <- m2
test_num$A16[which(is.na(test_num$A2))]   <- m3

# now combine the train_cat and train_num, do the same with test_num, test_cat
train3 <- data.frame(train_num,train_cat)
test3 <- data.frame(test_num,test_cat)

rm('trainRows','trainRows2','train_mean')


# SMOTE technique but the results were messed up
# commenting SMOTE application as a whole so that
# it doesn't mess up with my actual code

# rm(list = ls(all=TRUE))
# 
# #read the data
# data <- read.csv('Data_20180408.csv',header=TRUE,na.strings = c(-99,"NA"))
# 
# #EDA
# summary(data)
# str(data)
# dim(data)
# 
# #check NA's in data
# sum(is.na(data))
# 
# #check NA's in each column
# sapply(data, function(x)sum(is.na(x)))
# 
# #check unique values for each column
# sapply(data, function(x) unique(x))
# 
# #change the variables to factors
# data$A11 <- as.factor(data$A11)
# data$A13 <- as.factor(data$A13)
# data$A17 <- as.factor(data$A17)
# data$A18 <- as.factor(data$A18)
# data$A19 <- as.factor(data$A19)
# data$A20 <- as.factor(data$A20)
# data$A22 <- as.factor(data$A22)
# data$Target <- as.factor(data$Target)
# 
# #split the data into train and test
# set.seed(123)
# rows=seq(1,nrow(data),1)
# set.seed(123)
# train_rows=sample(rows,round((70*nrow(data))/100))
# train = data[train_rows,]
# test = data[-train_rows,]
# 
# #impute the train set and test set
# m1 <- mean(train$A2 , na.rm = T)
# m2 <- mean(train$A15 , na.rm = T)
# m3 <- mean(train$A16 , na.rm = T)
# 
# train$A2[which(is.na(train$A2))]   <- m1
# train$A15[which(is.na(train$A2))]   <- m2
# train$A16[which(is.na(train$A2))]   <- m3
# 
# test$A2[which(is.na(test$A2))]   <- m1
# test$A15[which(is.na(test$A2))]   <- m2
# test$A16[which(is.na(test$A2))]   <- m3 
# 
# #use SMOTE
# library(DMwR)
# train <- SMOTE(Target ~ ., train, perc.over = 100, perc.under=150)
# summary(train$Target)
# #perfect sample 0:1 <-> 11869:15826
# 
# #drop some of the columns which have rare occurences
# train$A11 <- NULL
# train$A13 <- NULL
# train$A18 <- NULL
# train$A22 <- NULL
# 
# test$A11 <- NULL
# test$A13 <- NULL
# test$A18 <- NULL
# test$A22 <- NULL
# 
# #train you model and then test it on unseen data
# log_reg = glm(Target ~ ., data = train, family=binomial)
# summary(log_reg)
# 
# # Prediction on train data
# prob_train <- predict(log_reg, train, type = "response")
# # Predicting on test data
# prob_test <- predict(log_reg, test, type = "response")
# 
# #build AUC
# library(ROCR) 
# pred <- prediction(prob_train, train$Target)
# perf <- performance(pred, measure="tpr", x.measure="fpr")
# 
# #Plot the ROC curve using the extracted performance measures (TPR and FPR)
# plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
# perf_auc <- performance(pred, measure="auc")
# auc <- perf_auc@y.values[[1]]
# print(auc)
# 
# pred_class <- ifelse(prob_train > 0.31, 1, 0)
# table(train$Target,pred_class)
# 
# prob_val <- predict(log_reg, test, type = "response")
# preds_val <- ifelse(prob_val > 0.31, 1, 0)
# table(preds_val)
# 
# conf_matrix <- table(test$Target, preds_val)
# print(conf_matrix)
# specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
# sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
# accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
# 
# library(caret)
# # Using the argument "Positive", we can get the evaluation metrics according to our positive #referene level
# confusionMatrix(preds_val, test$Target,positive="1")
# 
# 
# # run Naive Bayes model and check the results
# library(e1071)
# model = naiveBayes(Target ~ ., data = train3)
# model
# 
# pred = predict(model, train3)
# table(pred, train3$Target)
# 
# pred = predict(model, test3)
# table(pred, test3$Target)

#Finally applied SMOTE over new data and found interesting results.
############################# Final 1 approach ################################

#Applied SMOTE, didn't work out well

rm(list = ls(all=TRUE))

#read the data
data <- read.csv('Data_20180408.csv',header=TRUE,na.strings = c(-99,"NA"))

#EDA
summary(data)
str(data)
dim(data)

#check NA's in data
sum(is.na(data))

#check NA's in each column
sapply(data, function(x)sum(is.na(x)))

#change the variables to factors
data$A11 <- as.factor(data$A11)
data$A13 <- as.factor(data$A13)
data$A17 <- as.factor(data$A17)
data$A18 <- as.factor(data$A18)
data$A19 <- as.factor(data$A19)
data$A20 <- as.factor(data$A20)
data$A22 <- as.factor(data$A22)
data$Target <- as.factor(data$Target)

#split the data into train and test
set.seed(123)
rows=seq(1,nrow(data),1)
set.seed(123)
train_rows=sample(rows,round((70*nrow(data))/100))
train = data[train_rows,]
test = data[-train_rows,]

#impute the train set and test set
# m1 <- mean(train$A2) # when numeric else mode function

m1 <- mean(train$A2, na.rm = T)
m2 <- mean(train$A15 , na.rm = T)
m3 <- mean(train$A16 , na.rm = T)

train$A2[which(is.na(train$A2))]   <- m1
train$A15[which(is.na(train$A15))]   <- m2
train$A16[which(is.na(train$A16))]   <- m3

test$A2[which(is.na(test$A2))]   <- m1
test$A15[which(is.na(test$A15))]   <- m2
test$A16[which(is.na(test$A16))]   <- m3 

#use SMOTE
library(DMwR)
set.seed(123)
train <- SMOTE(Target ~ ., train, perc.over = 200, perc.under=100)
summary(train$Target)
#perfect sample 0:1 <-> 11869:15826

#drop some of the columns which have rare occurences
train$A11 <- NULL
train$A13 <- NULL
train$A18 <- NULL
train$A22 <- NULL

test$A11 <- NULL
test$A13 <- NULL
test$A18 <- NULL
test$A22 <- NULL

#train your model and then test it on unseen data
log_reg = glm(Target ~ ., data = train, family=binomial)
summary(log_reg)

# Prediction on train data
prob_train <- predict(log_reg, train, type = "response")
# Predicting on test data
prob_test <- predict(log_reg, test, type = "response")

#build AUC
library(ROCR) 
pred <- prediction(prob_train, train$Target)
perf <- performance(pred, measure="tpr", x.measure="fpr")

# #
# library(ROCR) 
# pred <- prediction(prob_train, train_data$y)
# perf <- performance(pred, measure="tpr", x.measure="fpr")
# #

#Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)

pred_class <- ifelse(prob_train > 0.50, 1, 0)
table(train$Target,pred_class)

prob_val <- predict(log_reg, test, type = "response")
preds_val <- ifelse(prob_val > 0.50, 1, 0)
table(preds_val)

conf_matrix <- table(test$Target, preds_val)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)

library(caret)
# Using the argument "Positive", we can get the evaluation metrics according to our positive #referene level
confusionMatrix(preds_val, test$Target,positive="1")

#Use vif to find any multi-collinearity
library(car)
log_reg_vif = vif(log_reg)
log_reg_vif

#removing the columns of high vif
drops <- c("A5","A6","A7","A8","A9","A10","A12")
train <- train[ , !(names(train) %in% drops)]
test <- test[ , !(names(train) %in% drops)]

#re-running the model after tuning the data
log_reg = glm(Target ~ ., data = train, family=binomial)
summary(log_reg)

# Prediction on train data
prob_train <- predict(log_reg, train, type = "response")
# Predicting on test data
prob_test <- predict(log_reg, test, type = "response")

#build AUC
library(ROCR) 
pred <- prediction(prob_train, train$Target)
perf <- performance(pred, measure="tpr", x.measure="fpr")

#Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)

#perdict
pred_class <- ifelse(prob_train > 0.50, 1, 0)
table(train$Target,pred_class)

prob_val <- predict(log_reg, test, type = "response")
preds_val <- ifelse(prob_val > 0.50, 1, 0)
table(preds_val)

#confusion matrix
library(caret)
confusionMatrix(preds_val, test$Target,positive="1")



#####################################################################################################


###------------------------------Lasso Regression------------------------------#############
#Clearing the environment
rm(list=ls(all=TRUE))
setwd()
# Reading the data 
data<-read.csv("Data_20180408.csv",header=T)

# Analysing the data structures and statistics
str(data)
summary(data)

#Dropping the rows whose values are >1000 as they seems to be outliers
data<-data[(data$A5<1000 & data$A6<1000 & data$A7<1000 &
              data$A8<1000 & data$A9<1000 & data$A10<1000 & data$A12<1000),]
dim(data)



#checking the Initial proportions of the target
prop.table(table(data$Target))

# Data Preparation
data = data[,-1]# remove CustomerID column
data = data[,-12]# remove A11 column

### considering -99 as NA
data[data=="-99"]<-NA
sum(is.na(data))

# Identifying the categorical data  
data_cat<-subset(data,select=c(A13,A17,A18,A19,A20,A22,Target))
names(data_cat)
data_num<-subset(data,select= -c(A13,A17,A18,A19,A20,A22,Target))
names(data_num)

#Factorising the data as of levels 
data_cat<-data.frame(apply(data_cat,2,function(x){as.factor(x)}))
str(data_cat)
Data<-data.frame(c(data_num,data_cat))
names(Data)
str(Data)


#Seperating the target  
y<- subset(data,select="Target")

#Converting categorical attributes into dummy variables
xfactors <- model.matrix(data$Target ~ data$A13 
                         + data$A17 + 
                           data$A18 +data$A19
                         +data$A20 +data$A22)[,-1]



#Split the data into train and validation data sets
set.seed(123)
rows=seq(1,nrow(data),1)
trainRows=sample(rows,(80*nrow(data))/100)
train1 = data.frame(data_num, 
                    xfactors,Target=data$Target)[trainRows,]
validation1 = data.frame(data_num, 
                         xfactors,Target=data$Target)[-trainRows,]
sapply(train1, function(x) sum(is.na(x)))
sapply(validation1, function(x) sum(is.na(x)))
data2 <- as.matrix(data.frame(data_num, xfactors))
train = data2[trainRows,] 
validation = data2[-trainRows,]


# working with missing values
#Imputing the train data
library(DMwR)
train[,c(3,14,15)] <- centralImputation(train[,c(3,14,15)])
summary(train)

#Imputing the test data
j <- c(3,14,15)
for(i in j) {
  validation[is.na(validation[,i]), i] <- median(train[,i], na.rm =FALSE)
}

#Target Varaible
y=data$Target[trainRows]
yvalidation = data$Target[-trainRows]
summary(train1)
train1$Target <- NULL
class(train)


# Lasso Regression  using glmnet - L1 norm
library(glmnet)
# fit model
fit1 <- glmnet(train,y,family = "binomial", alpha=1)
coef(fit1)
plot(fit1,xvar="lambda",label=TRUE)
plot(fit1,xvar="dev",label=TRUE)

#Model Selection
coef(fit1)
cv.lasso=cv.glmnet(train,y,family="binomial")
plot(cv.lasso)
coef(cv.lasso)

prob <- predict(cv.lasso,train, type='response')
min(prob)
max(prob)
prob_val <- predict(cv.lasso, validation, type="response") # Predicting on test data
min(prob_val)
max(prob_val)

library(ROCR) 
pred <- prediction(prob, y)
perf <- performance(pred, measure="tpr", x.measure="fpr")


#Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc2 <- perf_auc@y.values[[1]]
print(auc2)

pred_train <- ifelse(prob > 0.35, 1, 0)
conf_matrix <- table(y, pred_train)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
sensitivity
specificity
accuracy


preds_val2 <- ifelse(prob_val > 0.35, 1, 0)
table(preds_val2)
conf_matrix <- table(yvalidation, preds_val2)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
sensitivity
specificity
accuracy

#---------------- Ridge Regression  using glmnet  - L2 norm-------------------------------####
library(glmnet)
# fit model
fit2 <- glmnet(train,y,family = "binomial", alpha=0)
plot(fit2,xvar="lambda",label=TRUE)

#Model Selection
coef(fit2) 
cv.ridge=cv.glmnet(train,y,family="binomial")
plot(fit2,xvar="lambda",label=TRUE)

plot(cv.ridge)
coef(cv.ridge)

prob <- predict(cv.ridge,train, type='response')
min(prob)
max(prob)
prob_val <- predict(cv.ridge, validation, type="response") # Predicting on test data
min(prob_val)
max(prob_val)

library(ROCR) 
pred <- prediction(prob, y)
perf <- performance(pred, measure="tpr", x.measure="fpr")


#Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc3 <- perf_auc@y.values[[1]]
print(auc3)

pred_train <- ifelse(prob > 0.3, 1, 0)
conf_matrix <- table(y, pred_train)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
sensitivity
specificity
accuracy


preds_val2 <- ifelse(prob_val > 0.3, 1, 0)
table(preds_val2)
conf_matrix <- table(yvalidation, preds_val2)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
sensitivity
specificity
accuracy



####---------------------------------Elastic net model---------------------------------------####
fit.glm=glmnet(train,y,family="binomial",alpha = 0.5)
cvfit=cv.glmnet(train, y,family="binomial",alpha = 0.5)
plot(fit.glm,xvar="lambda",label=TRUE)
plot(cvfit)
prob <- predict(cvfit,train, type='response')
min(prob)
max(prob)
prob_val <- predict(cvfit, validation, type="response") # Predicting on test data
min(prob_val)
max(prob_val)

library(ROCR) 
pred <- prediction(prob, y)
perf <- performance(pred, measure="tpr", x.measure="fpr")


#Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc4 <- perf_auc@y.values[[1]]
print(auc4)

pred_train <- ifelse(prob > 0.35, 1, 0)
conf_matrix <- table(y, pred_train)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
sensitivity
specificity
accuracy


preds_val2 <- ifelse(prob_val > 0.35, 1, 0)
table(preds_val2)
conf_matrix <- table(yvalidation, preds_val2)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
sensitivity
specificity
accuracy

##REPORT
#Here we found the outliers, and dropped them.1909 records(5.5 % of given dataset).
#And also penalised the co-efficients and finally we come up with no warnings,at the same time 
#                                         having good sensitivity,accuracy and AUC

