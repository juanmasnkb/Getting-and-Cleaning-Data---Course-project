library(dplyr)

#### Getting data ####

url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

zipFile<- "UCI HAR Dataset.zip"

if(!file.exists("zipFile")) {
  download.file(url = url, zipFile)
}

#### unzip zip file containing data ####

data<- "UCI HAR Dataset"

if (!file.exists(data)) {
  unzip(zipFile)
}

#### Reading Data ####

## training data ##

trainingSubjects <- read.table(file.path(data, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(data, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(data, "train", "y_train.txt"))

## read test data  ##

testSubjects <- read.table(file.path(data, "test", "subject_test.txt"))
testValues <- read.table(file.path(data, "test", "X_test.txt"))
testActivity <- read.table(file.path(data, "test", "y_test.txt"))

## read features, don't convert text labels to factors ##

features <- read.table(file.path(data, "features.txt"), as.is = TRUE)

## read activity labels ##

activities <- read.table(file.path(data, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

##############################################################################
# Step 1 - Merge the training and the test sets to create one data set
##############################################################################



## Concatenate individual data tables to make single data table ##

Data_merge <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)
## assign column names ##
colnames(Data_merge) <- c("subject", features[, 2], "activity")



##############################################################################
# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement
##############################################################################

## determine columns of data set to keep based on column name ##

columnsToKeep <- grepl("subject|activity|mean|std", colnames(Data_merge))

## keep data in these columns only ##

Data_merge <- Data_merge[, columnsToKeep]



##############################################################################
# Step 3 - Use descriptive activity names to name the activities in the data
#          set
##############################################################################

# replace activity values with named factor levels
Data_merge$activity <- factor(Data_merge$activity, 
                                 levels = activities[, 1], labels = activities[, 2])



##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

## get column names ##

Data_merge_cols <- colnames(Data_merge)

## remove special characters ##

Data_merge_cols <- gsub("[\\(\\)-]", "", Data_merge_cols)

## expand abbreviations and clean up names ##

Data_merge_cols <- gsub("^f", "frequencyDomain", Data_merge_cols)
Data_merge_cols <- gsub("^t", "timeDomain", Data_merge_cols)
Data_merge_cols <- gsub("Acc", "Accelerometer", Data_merge_cols)
Data_merge_cols <- gsub("Gyro", "Gyroscope", Data_merge_cols)
Data_merge_cols <- gsub("Mag", "Magnitude", Data_merge_cols)
Data_merge_cols <- gsub("Freq", "Frequency", Data_merge_cols)
Data_merge_cols <- gsub("mean", "Mean", Data_merge_cols)
Data_merge_cols <- gsub("std", "StandardDeviation", Data_merge_cols)

## correct typo ##

Data_merge_cols <- gsub("BodyBody", "Body", Data_merge_cols)

## use new labels as column names ##

colnames(Data_merge) <- Data_merge_cols



############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

## group by subject and activity and summarise using mean ##

Data_merge_Means <- Data_merge %>% 
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

## output to file "tidy_data.txt" ##

write.table(Data_merge_Means, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
