#loading packages

library(data.table)
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
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)

#we will read train data
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)

#reading feature vector
featureNames <- read.table("UCI HAR Dataset/features.txt")

#reading activity labels
activityLabels = read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)


#step 1 Merges the training and the test data sets to create one data set.

mrg_subject <- rbind(subject_train, subject_test)
mrg_activity <- rbind(y_train, y_test)
mrg_features <- rbind(x_train, x_test)

   ##Naming the columns
        colnames(mrg_features) <- t(featureNames[2])
        colnames(mrg_activity) <- "Activity"
        colnames(mrg_subject) <- "Subject"
        compl_data <- cbind(mrg_features,mrg_activity,mrg_subject)

        
#step 2 Extracts only the measurements on the mean and the standard deviation for each measurement.

#creating vector
Mean_Std <- grep(".*Mean.*|.*Std.*", names(compl_data), ignore.case=TRUE)

#adding some columns
req_colu <- c(Mean_Std, 562, 563)
dim(compl_data)

extr_data <- compl_data[, req_colu]

#again checking dim
dim(extr_data)

#step 3 Uses descriptive activity names to name the activities in the data set

extr_data$Activity <- as.character(extr_data$Activity)
for (i in 1:6){
  extr_data$Activity[extr_data$Activity == i] <- as.character(activityLabels[i,2])
}

#factor the data
extr_data$Activity <- as.factor(extr_data$Activity)

#step 4 Appropriately lables the data set with descriptive variable names.
names(extr_data)
names(extr_data)<-gsub("Acc", "Accelerometer", names(extr_data))
names(extr_data)<-gsub("Gyro", "Gyroscope", names(extr_data))
names(extr_data)<-gsub("BodyBody", "Body", names(extr_data))
names(extr_data)<-gsub("Mag", "Magnitude", names(extr_data))
names(extr_data)<-gsub("^t", "Time", names(extr_data))
names(extr_data)<-gsub("^f", "Frequency", names(extr_data))
names(extr_data)<-gsub("tBody", "TimeBody", names(extr_data))
names(extr_data)<-gsub("-mean()", "Mean", names(extr_data), ignore.case = TRUE)
names(extr_data)<-gsub("-std()", "STD", names(extr_data), ignore.case = TRUE)
names(extr_data)<-gsub("-freq()", "Frequency", names(extr_data), ignore.case = TRUE)
names(extr_data)<-gsub("angle", "Angle", names(extr_data))
names(extr_data)<-gsub("gravity", "Gravity", names(extr_data))

#looking at it
names(extr_data)


#step 5 From the data set in step 4,create a second,independent tidy data set with the average of each variable for each activity and each subject.

#factor Subject
extr_data$Subject <- as.factor(extr_data$Subject)
extr_data <- data.frame(extr_data)

#create tidydata

tidyData1 <- aggregate(. ~Subject + Activity, extr_data, mean)
tidyData1 <- tidyData1[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData1, file="Tidy.txt", row.names = FALSE)

#looking at it

tidyData1
