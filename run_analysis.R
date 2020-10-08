#loading packages

library(tidyverse)
library(data.table)

### 1. Merges the training and the test sets to create one data set.

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c('activity_ID', 'activity_Name'), quote = "")
features <- read.table('./UCI HAR Dataset/features.txt', col.names = c('feature_ID', 'feature_Name'), quote = "")


#Test data manipulation

subject_test <- read.table('./UCI HAR Dataset/test/subject_test.txt', col.names = c('subject_ID'))

x_test <-  read.table('./UCI HAR Dataset/test/X_test.txt')
y_test <-  read.table('./UCI HAR Dataset/test/y_test.txt')

colnames(x_test) <- features$feature_Name
colnames(y_test) <- c('activity_ID')

test_data <- cbind(subject_test, x_test, y_test)


#Training data manipulation

subject_train <- read.table('./UCI HAR Dataset/train/subject_train.txt', col.names = c('subject_ID'))

x_train <-  read.table('./UCI HAR Dataset/train/X_train.txt')
y_train <-  read.table('./UCI HAR Dataset/train/y_train.txt')

colnames(x_train) <- features$feature_Name
colnames(y_train) <- c('activity_ID')

train_data <- cbind(subject_train, x_train, y_train)


#Put Train and Test data together
Data <- rbind(train_data, test_data)


### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

mean_sd_data <- Data[, c(1, grep(pattern = 'mean\\(\\)|std\\(\\)', x = names(Data)), 563)]


### 3. Uses descriptive activity names to name the activities in the data set

mean_sd_data$subject_ID <- as.factor(mean_sd_data$subject_ID)
mean_sd_data$activity_ID <- factor(mean_sd_data$activity_ID,levels = activity_labels$activity_ID,labels = activity_labels$activity_Name)

#except 4-5 !!
mean_sd_data <- mean_sd_data[, -68]


### 4. Appropriately labels the data set with descriptive variable names.

colnames(mean_sd_data) <- gsub(pattern = '\\(\\)', replacement = "", x = names(mean_sd_data))
mean_sd_data <- mean_sd_data[, c(1, 68, 2:67)]

write.table(mean_sd_data, file = 'tidy_data.txt', row.names = F, quote = F, sep = "\t")


### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

mean_sd_data_activity <- group_by(mean_sd_data, mean_sd_data$subject_ID, mean_sd_data$activity_ID ) %>% summarise_all(funs(mean))
write.table(mean_sd_data_activity, file = 'tidy_dataset_final.txt', row.names = F, quote = F, sep = "\t")
