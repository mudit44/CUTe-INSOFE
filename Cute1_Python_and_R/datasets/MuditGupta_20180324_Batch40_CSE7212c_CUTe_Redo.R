col1 = read.csv('COL1.csv',header = TRUE)
col2 = read.csv('COL2.csv',header = TRUE)
col3 = read.csv('COL3.csv',header = TRUE)
col4 = read.csv('COL4.csv',header = TRUE)
col5 = read.csv('COL5.csv',header = TRUE)

consolidated_data <- rbind(col1,col2,col3,col4,col5)
nrow(consolidated_data)

sum(is.na(consolidated_data))

consolidated_data <- knnImputation(consolidated_data)

library(DMwR)
consolidated_data <- centralImputation(consolidated_data)
consolidated_data

names(consolidated_data)

sw = shapiro.test(consolidated_data$Acad_Score)

hist(sw$statistic, breaks = 50)
hist(sw$p.value, breaks = 50)

hist(consolidated_data$Acad_Score,breaks = 50,freq = FALSE)
lines(density(consolidated_data$Acad_Score))

hist(consolidated_data$Overall_Score, breaks = 10,freq = FALSE)
lines(density(consolidated_data$Overall_Score))

placements <- read.csv('Placements.csv',header = TRUE)

consolidated_data <- merge(consolidated_data,placements,by.x = c("CollegeID","StudentID"),by.y = c("CollegeID","StudentID"),all.x = TRUE,all.y = TRUE)
rm("mergedData")

plot(consolidated_data$Placed)

sum(is.na(consolidated_data$Placed))

#Replacing factors which have NA values with some other value(here) is None.
levels(consolidated_data$Placed)<-c(levels(consolidated_data$Placed),"None")
consolidated_data$Placed[is.na(consolidated_data$Placed)] <- "None"

plot(consolidated_data$Placed)

consolidated_data$isPlaced <- 0

#add another column isPlaced with values satisfying conditions according to Placed
consolidated_data$isPlaced <- ifelse(consolidated_data$Placed == "None", 1, 2)

#no. of students who from different colleges and are placed either private/public.
sum((consolidated_data$CollegeID == "CID_1") & (consolidated_data$isPlaced == 2))
sum((consolidated_data$CollegeID == "CID_2") & (consolidated_data$isPlaced == 2))
sum((consolidated_data$CollegeID == "CID_3") & (consolidated_data$isPlaced == 2))
sum((consolidated_data$CollegeID == "CID_4") & (consolidated_data$isPlaced == 2))
sum((consolidated_data$CollegeID == "CID_5") & (consolidated_data$isPlaced == 2))

hist(consolidated_data$Overall_Score)