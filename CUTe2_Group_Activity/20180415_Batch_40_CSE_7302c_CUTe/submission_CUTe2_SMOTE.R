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

#check unique values for each column
sapply(data, function(x) unique(x))

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

pred_class <- ifelse(prob_train > 0.6, 1, 0)
table(train$Target,pred_class)

prob_val <- predict(log_reg, test, type = "response")
preds_val <- ifelse(prob_val > 0.6, 1, 0)
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
