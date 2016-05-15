## data for the project downloaded from the link and unzipped into working directory
## 
## Create variables to read data files
##
features <- read.delim("./UCI HAR Dataset/features.txt", header = FALSE)
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
x_train <- read.delim("./UCI HAR Dataset/train/X_train.txt", sep = "", header = FALSE)
y_train <- read.delim("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
x_test <- read.delim("./UCI HAR Dataset/test/X_test.txt", sep = "", header = FALSE)
y_test <- read.delim("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

## Assign feature vector names as column names in x_test and x_train
colnames(x_test) <- features$V1
colnames(x_train) <- features$V1

## combine the test and train files into a single data frame
x_both <- rbind(x_test,x_train)
y_both <- rbind(y_test,y_train)
subject_both <- rbind(subject_test,subject_train)

## set the activity data to the name of the activity rather than just a number
## and name the columns of data
## Note this requires dplyr package
library(dplyr)
y_both <- mutate(y_both,activity_labels$V2[y_both$V1])
colnames(y_both) <- c("activitynum","activity")
colnames(subject_both) <- "subject"

## pull only the columns with mean and standard deviation
x_meanstd <- x_both[,c(grep("std",colnames(x_both)),grep("mean",colnames(x_both)))]

## remove the numbers from column names and 
splitNames <- strsplit(names(x_meanstd),"\\ ")
secondElement <- function(x) {x[2]}
splitNames <- sapply(splitNames,secondElement)
colnames(x_meanstd) <- splitNames

## combine the subject and activity labels with the data into a single data frame
full_data <- cbind(subject_both,y_both,x_meanstd)
## put variable names in lower case
## strip out parentheses and dashes from column names
colnames(full_data) <- tolower(names(full_data))
colnames(full_data) <- gsub("\\(\\)","",names(full_data))
colnames(full_data) <- gsub("\\-","",names(full_data))

## create the second, independent tidy data set with the average of each variable 
## for each activity and each subject

## change subject variable to factors in order to group
full_data$subject <- as.factor(full_data$subject)
full_data <- group_by(full_data,subject,activity)

## initiate average data frame with first summary data
x <- as.name(colnames(full_data[4]))
average_data <- summarise(full_data, average = mean(x))
colnames(average_data) <- c("subject", "activity", colnames(full_data[4]))

## add a column to average data frame for every column in full_data
for(i in 5:length(colnames(full_data))) {
    x <- as.name(colnames(full_data[i]))
    temp_avg <- summarise(full_data, average = mean(x))
    colnames(temp_avg) <- c("subject", "activity", colnames(full_data[i]))
    average_data <- cbind(average_data,temp_avg[3])
}
