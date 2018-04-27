rm(list=ls())


col1 <- read.csv('COL1.csv',header=TRUE)
sum(is.na(col1)) # NA -> 9
col2 <- read.csv('COL2.csv',header=TRUE)
sum(is.na(col2)) # NA -> 6
col3 <- read.csv('COL3.csv',header=TRUE)
sum(is.na(col3)) # NA -> 19
col4 <- read.csv('COL4.csv',header=TRUE)
sum(is.na(col4)) # NA -> 9
col5 <- read.csv('COL5.csv',header=TRUE)
sum(is.na(col5)) # NA -> 9

consolidated_data <- rbind(col1,col2,col3,col4,col5) #Consolidating all the values

sum(is.na(consolidated_data)) #NA -> 52

names(consolidated_data)

hist(consolidated_data$Overall_Score) #Skewed Dataset
hist(consolidated_data$Acad_Score) #Skewed Dataset

consolidated_data$ID <- seq.int(nrow(consolidated_data)) #making a unique ID

consolidated_data <- centralImputation(consolidated_data) #imputing the NA values of the dataset

plot(consolidated_data$Behavior_type)#Behaviour type is right skewed
plot(consolidated_data$Extra.Curr) #Extra.Curr is equally distributed

placement <- read.csv('Placements.csv',header = TRUE)

placement

library(stringr)
consolidated_data$StudentID <- str_split_fixed(consolidated_data$StudentID, "_", 2)[,2]

as.factor(consolidated_data$StudentID)

names(placement)
placement$StudentID <- str_split_fixed(placement$StudentID,"_",2)[,2]
as.factor(placement$StudentID)

placement <- data.frame(placement,dummy(placement$Placed))
placement$Placed <- NULL

placement

head(consolidated_data)

library(reshape2)
placement
melt(placement$CollegeID)


consolidated_data$isPlaced <- 0
for (i in 1:nrow(consolidated_data))
{
  if(consolidated_data$placementboth[i] == 1)
    consolidated_data$isPlaced = 1
  else if (consolidated_data$placementprivate[i] == 1)
    consolidated_data$isPlaced = 1
  else if (consolidated_data$placementpublic[i] == 1)
    consolidated_data$isPlaced = 1
  else
    consolidated_data$isPlaced = 0
}

plot(x=consolidated_data$CollegeID.x,y=consolidated_data$placementprivate)

