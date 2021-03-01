
## download files
urlink <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(urlink, destfile = "C:/Users/xianw_000/SkyDrive/Documents/R/datacleaning/rowdata.zip")
## unzip the zip raw data
unzip("rowdata.zip")
library(tidyverse)
## read in the features according to the Readme file with read_table2 function.
features <- read_table2("UCI HAR Dataset/features.txt", col_names = c("index","measure"))
trainSubject <- read_table("UCI HAR Dataset/train/subject_train.txt",col_names = "subject")

x_train <- read_table2("UCI HAR Dataset/train/X_train.txt", col_names = features$measure)
y_train <- read_table("UCI HAR Dataset/train/y_train.txt", col.names = 'activity')

testSubject <- read_table("UCI HAR Dataset/test/subject_test.txt", col_names = "subject")

x_test<- read_table2("UCI HAR Dataset/test/X_test.txt", col_names = features$measure)
y_test<- read_table("UCI HAR Dataset/test/y_test.txt", col_names = "activity")

##after the data is read in. merge the train and test data.
x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
totaSubject <- rbind(trainSubject,testSubject)

##merging all the data into one tibble
rawData <- cbind(totaSubject,y_data,x_data)

## x_data has unnessary data needs to be cleaned. we only interested in mean and std.
cleanData <- rawData %>%
  select(subject,activity,contains("mean"), contains("std"))
## the columns of raw data has been reduced to 88 from 563.

## use descriptive activity names.
## first readin labels
activity_label <- read_table2("UCI HAR Dataset/activity_labels.txt", col_names = c('code','names'))
## then modify the activities according to the activity labels.
cleanData2<- cleanData %>%
  mutate(activity = activity_label$names[match(y_data$activity, activity_label$code)])

## label with descriptive variable names.
cleanData2 <- rename_with(cleanData2, ~gsub("Acc", " Acceleration", names(cleanData2)))
cleanData2 <- rename_with(cleanData2, ~gsub("tBody", "Time Body", names(cleanData2))) 
cleanData2 <- rename_with(cleanData2, ~gsub("tGravity", "Time Gravity",names(cleanData2)))
cleanData2 <- rename_with(cleanData2, ~gsub("fBody", "Frequency Body",names(cleanData2))) 
cleanData2 <- rename_with(cleanData2, ~gsub("Gyro", " Gyroscope", names(cleanData2)))
cleanData2 <- rename_with(cleanData2, ~gsub("Mag", " Magnitube", names(cleanData2))) 
cleanData2 <- rename_with(cleanData2, ~gsub("fGravity","Frequency Gravity", names(cleanData2)))
cleanData2 <- rename_with(cleanData2, ~gsub("Jerk", " Jerk", names(cleanData2)))
cleanData2 <- rename_with(cleanData2, ~gsub("BodyBody", "Body", names(cleanData2)))

## create independent tidy data set with the average of each variable for each activity and each subject
cleanData3 <- cleanData2 %>%
  group_by(subject,activity) %>%
  summarize(across(.cols=everything(),mean))

##export the dataset
write.csv(cleanData3, "CleanData.csv", row.names = FALSE)
