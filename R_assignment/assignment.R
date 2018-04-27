rm(list=ls())
setwd('C:/Users/Mudit/Desktop/INSOFE/R_assignment')
exam = read.csv('Exam.csv',header = TRUE,na.strings = c('',' ',NA,'#','?'))
marks = read.csv('Marks.csv',header = TRUE,na.strings = c('',' ',NA,'#','?'))

#Sum of NA
sum(is.na(exam))
sum(is.na(marks))

MergedData<-merge(exam,marks,by.x="Student.ID",by.y="Studen.ID")

MergedData$Exam <- trimws(MergedData$Exam)
#To trim Whitespaces
table(MergedData$Exam)

str(MergedData)

num_data <- subset(MergedData,select = -c(Student.ID,Exam))
cat_data <- subset(MergedData,select = c(Student.ID,Exam))

library(DMwR)
num_data_mod<-centralImputation(num_data) #Cenral Imputation
sum(is.na(num_data_mod))

table(MergedData$Exam)
num_data_mod$ID <- seq.int(nrow(num_data))
MergedData2 <- merge(cat_data,num_data_mod,by.x='Student.ID',by.y='ID')

str(MergedData2)
MergedData2$Student.ID <- as.factor(MergedData2$Student.ID)
MergedData2$Subject.B <- as.integer(MergedData2$Subject.B)

MergedData2

MergedData2$Exam[is.na(MergedData2$Exam)] <- "March"

MergedData2$total = apply(MergedData2[,3:7], 1, sum)

MergedData2$percent = MergedData2$total / 5

sum((MergedData2$Exam == 'September') & (MergedData2$percent > 60))

#GroupBy kind of
tapply(MergedData2$percent,MergedData2$Exam,mean)

#tapply(MergedData2$percent,MergedData2$Exam,mean)
#March September 
#58.90526  61.69091  

MergedData2$class <- 0
for (i in 1:nrow(MergedData2))
{
  if(MergedData2$percent[i] > 75 && MergedData2$Exam == "March")
    MergedData2$class[i] = "Distinction"
  else if (MergedData2$percent[i] > 75 && MergedData2$Exam == "September")
    MergedData2$class[i] = "dist_supplementary"
  else if (MergedData2$percent[i] > 60 && MergedData2$Exam == "March")
    MergedData2$class[i] = "I Class"
  else if (MergedData2$percent[i] < 60 && MergedData2$Exam == "March")
    MergedData2$class[i] = "Fail"
  else if (MergedData2$percent[i] < 60 && MergedData2$Exam == "September")
    MergedData2$class[i] = "Detained"
}

#library(ggplot2)

#ggplot(MergedData2,aes(x=Exam,y=percent)) +
#  geom_boxplot() +
# geom_hline(yintercept = mean(MergedData2$percent))
#  theme_classic()

plot(x=MergedData2$Exam,y=MergedData2$percent)
abline(h = mean(MergedData2$percent),col='red')

library(vegan)

num_data_2 <- subset(MergedData2,select = -c(class,Student.ID,Exam))
num_data_2 <- decostand(num_data_2,"standardize")
num_data_2

#################################################################

idata <- iris
idata

idata_num <- subset(idata,select = -c(Species))
idata_cat <- subset(idata,select = c(Species))

idata_num

pivot1 <- matrix(nrow = 4,ncol = 4,byrow = TRUE,dimnames = list(c("Min","Max","Avg","sd"),c(names(idata_num))))

pivot1[1,] <- apply(idata_num[,1:4],2,min)
pivot1[2,] <- apply(idata_num[,1:4],2,max)
pivot1[3,] <- apply(idata_num[,1:4],2,mean)
pivot1[4,] <- apply(idata_num[,1:4],2,sd)

#list=ls()
#list
#rm('pivot2','x')

pivot2 <- matrix(nrow = 4,ncol = 4,byrow = TRUE,dimnames = list(c("Min","Max","Avg","sd"),c(names(idata_num))))

v <- matrix(data=NA,nrow=4,ncol=1,byrow=TRUE)

#pivot2[1,] <- apply(idata_num[,1:4],2,function(x){v[1,] <- min(x)
#v[2,] <- max(x)
#v[3,] <- mean(x)
#v[4,] <- sd(x)

#return(v)
#})
pivot2[1,] <- apply(idata_num[,1:4],2,function(x){return(min(x))})
pivot2[2,] <- apply(idata_num[,1:4],2,function(x){return(max(x))})
pivot2[3,] <- apply(idata_num[,1:4],2,function(x){return(mean(x))})
pivot2[4,] <- apply(idata_num[,1:4],2,function(x){return(sd(x))})

pivot2

max_character <- data.frame(Sepal.length=tapply(iris$Sepal.Length,iris$Species,max),
                     Sepal.width=tapply(iris$Sepal.Width,iris$Species,max),
                     Petal.length=tapply(iris$Petal.Length,iris$Species,max),
                     Petal.width=tapply(iris$Petal.Width,iris$Species,max))

max_character

str(iris)

max_character2 <- aggregate(cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)~Species,data = iris,FUN=max)

iris$Rand <- 0

iris$Rand = sample(1:3,3,replace = T)

iris$Rand <- as.factor(iris$Rand)

str(iris)

max_character3 <- aggregate(cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)~Species+Rand,data = iris, FUN = mean)

mean_character <- aggregate(cbind(Petal.Length,Sepal.Width)~Species,data=iris,FUN = function(x) c(mn = mean(x), sd = length(x)))

prod <- read.csv('Product_Purchase_Info.csv',header = TRUE)
#prod$Date <- format(as.Date(prod$Date), "%d/%m/%Y")
library(lubridate)
prod$Date <- dmy(prod$Date)
prod

#library(reshape2)
#reshape(prod,direction = "wide",sep="-",idvar = "Price",timevar = "Date",v.names = "Product")

v1 <- iris[1:120,]
v2 <- iris[121:150,]

v1$Species <-  NULL
v2$Species <-  NULL

v1_mean <- colMeans(iris[sapply(iris, is.numeric)])
v1_sd <- apply(v1[,1:4],2,sd)

library(vegan)
v1 <- decostand(iris[,1:4],method='standardize',1)

v2 <- scale(v2,center = v1_mean,scale = v1_sd)
