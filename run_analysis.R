#loading packages
library(dplyr)

filename <- "Coursera_Final.zip"

#we will download the file and before that will check if it already exists
if(!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}

#we will unzip the downloaded file
if(!file.exists("UCI HAR Dataset")){
  unzip(filename)
}

#reading files

#we will read test data
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

#we will read train data
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

#reading feature vector
features <- read.table("UCI HAR Dataset/features.txt")

#reading activity labels
activityLabels = read.table("UCI HAR Dataset/activity_labels.txt")

#assigning column names

colnames(x_train) <- features[,2]
colnames(y_train) <- "activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId', 'activityType)

#step 1 Merges the training and the test data sets to create one data set.
 
mrg_train <- cbind(y_train, subject_train, x_train)
mrg_test <- cbind(y_test, subject_test, x_test)
MergedData <- rbind(mrg_train, mrg_test)

#step 2 Extracts only the measurements on the mean and the standard deviation for each measurement.
 
#reading column names

colNames <- colnames(MergedData)

#creating vector
Mean_Std <- (grepl("activityId", colNames) |
             grepl("subjectId", colNames) |
             grepl("mean..", colNames) |
             grepl("std..", colNames)
)

#subsetting it
 
Set <- MergedData[, Mean_Std == TRUE]

#step 3 Uses descriptive activity names to name the activities in the data set

#labeling activities
SetActivity <- merge(Set, activityLabels)

#step 4 Appropriately lables the data set with descriptive variable names.


#step 5 From the data set in step 4,create a second,independent tidy data set with the average of each variable for each activity and each subject.
TidyData1 <- aggregate(. ~subjectId + activityId, SetActivity, mean)
TidyData1 <- TidyData1[order(TidyData1$subjectId, TidyData1$activityId),]
write.table(TidyData1, "TidyData1.txt", row.name=FALSE)
str(TidyData1)

#looking at it
TidyData1
