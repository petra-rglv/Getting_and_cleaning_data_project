# A course project script.
# run_analysis.R finds the average of each variable for each activity and 
# each subject in both train ant test subsets of UCI HAR Dataset. 
# It is assumed that the original dataset is in the current working directory.
# (If not - use the corresponding part of the script also to downoald the dataset 
# and unzip it.)
#
# The script does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Appropriately labels the data set with descriptive variable names.
# 3. Extracts only the measurements on the mean and standard deviation for each 
#    measurement. 
# 4. Uses descriptive activity names to name the activities in the data set
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

library(dplyr)
library(reshape2)

## Download and unzip the dataset in the current working directory
# fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download.file(fileUrl, destfile = "./zipfile.zip")
# unzip("./zipfile.zip")

# Read the data 
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt") 
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt") 
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

# Step 1. Merges the training and the test sets to create one data set.
MergedData <- rbind(cbind(subject_train,y_train,X_train),
                    cbind(subject_test,y_test,X_test))
# Step 2. Appropriately labels the data set with descriptive variable names.
colnames(MergedData) <- c("subject", "activity", paste(features$V2))

# Step 3. Extracts only the measurements on the mean and standard deviation for 
# each measurement. 
SelectedData <- cbind(MergedData[1:2],
                      MergedData[grepl("std|mean",names(MergedData))]) 
# Extract mean(), std(), and meanFreq(). 
# use grepl("std|Mean|mean",names(MergedData)) if you also want the 
# angle(,.Mean) features

# Step 4. Uses descriptive activity names to name the activities in the data set
for (i in 1:6){
    SelectedData$activity[SelectedData$activity == i] <- 
        paste(activity_labels[i,2])
}

# Step 5. From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject.
grouped <- group_by(SelectedData, subject, activity)

# Create wide tidy data set
tidy_data <- summarise_each(grouped,funs(mean))

# The tidy data set transformed into a long format,
# sorted by subject and activity
tidy_data_long <- arrange(melt(tidy_data, id.vars=c("subject","activity")), 
                          subject, activity)

# Write the data
write.table(tidy_data_long, file = "tidy_data_long.txt", row.name=FALSE)

# use this if you want to write the wide data set instead
#write.table(tidy_data, file = "tidy_data.txt", row.name=FALSE)