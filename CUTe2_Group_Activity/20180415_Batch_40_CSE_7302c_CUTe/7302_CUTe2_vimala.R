###------------------------------Lasso Regression------------------------------#############
#Clearing the environment
rm(list=ls(all=TRUE))

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
rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,(75*nrow(data))/100)
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
auc3 <- perf_auc@y.values[[1]]
print(auc3)

pred_train <- ifelse(prob > 0.32, 1, 0)
conf_matrix <- table(y, pred_train)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
sensitivity
specificity
accuracy


preds_val2 <- ifelse(prob_val > 0.32, 1, 0)
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
#Here we found the outliers, and dropped them.1909 records(0.05 % of given dataset).
#And also penalised the co-efficients and finally we come up with no warnings,at the same time 
#                                         having good sensitivity,accuracy and AUC

