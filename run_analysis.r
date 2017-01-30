## 0. Download and unzip the dataset, set up variables and load libraries

library(dplyr)
library(reshape2)

filename <- "getdata.zip" 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 

if (!file.exists(filename)){ 
  download.file(fileUrl, filename,  mode='wb') 
}   

if (!file.exists("UCI HAR Dataset")) {  
  unzip(filename)  
} 
 
## 1. Merges the training and the test sets to create one data set.

## read training dataset, subjects and activities, combine all to on dataset
train <- read.table("UCI HAR Dataset/train/X_train.txt") 
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt") 
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt") 
train <- cbind(trainSubjects, trainActivities, train) 

## read test dataset, subjects and activities, combine all to on dataset
test <- read.table("UCI HAR Dataset/test/X_test.txt") 
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt") 
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt") 
test <- cbind(testSubjects, testActivities, test) 

## merge training and test dataset to one testing set
mergedData <- rbind(train, test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

featuresNames <- read.table("UCI HAR Dataset/features.txt") 
featuresNamesvector <- featuresNames[,2]
featuresNamesvector <- as.character(featuresNamesvector)

## 3. Uses descriptive activity names to name the activities in the data set
rename.columns=function(mergedData,featuresNamesvector){
  #renames columns of a data table
  for(i in 1:length(names(mergedData))){
        if (i == 1) {names(mergedData)[i] <- c("subject")}
        else if (i == 2) {names(mergedData)[i] <- c("activity")}
        else { names(mergedData)[i] <- featuresNamesvector[[i-2]]}
      }
  mergedData
}

mergedData <-  rename.columns(mergedData,featuresNamesvector)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

meanstd <- grep(".*mean.*|.*std.*", names(mergedData))
## duplicate names are found, create set of unique names in order to avoid select errors
valid_column_names <- make.names(names = names(mergedData), unique=TRUE, allow_ = TRUE)
names(mergedData) <- valid_column_names
## select only mean/std values + activity and subject
mergedData <- select(mergedData,c(1,2,meanstd))


## 4. Appropriately labels the data set with descriptive variable names.
## add names instead of activity index

activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
 
mergedData <- merge(mergedData, activityLabels,by.x = "activity", by.y = "V1")

mergedData <- mergedData[-1]

mergedData <- rename(mergedData, activity = V2)

## after merge the activity names column is last in the table, need to move it to the second position
arrange.vars <- function(data, vars){

    ##sort out inputs
    data.nms <- names(data)
    var.nr <- length(data.nms)
    var.nms <- names(vars)
    var.pos <- vars
    

    ##prepare output
    out.vec <- character(var.nr)
    out.vec[var.pos] <- var.nms
    out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
    stopifnot( length(out.vec)==var.nr )

    ##re-arrange vars by position
    data <- data[ , out.vec]
    return(data)
}
## I need to move the activity column to the 2-nd position with function arrange.vars
mergedData <- arrange.vars(mergedData, c("activity"=2))

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# turn activities & subjects into factors 

mergedData$activity <- as.factor(mergedData$activity) 

mergedData$subject <- as.factor(mergedData$subject) 

mergedDatamelted <- melt(mergedData, id = c("subject", "activity")) 

mergedDatamean <- dcast(mergedDatamelted, subject + activity ~ variable, mean) 

colnames(mergedDatamean) <- gsub("[..]","",names(mergedDatamean))

write.table(mergedDatamean, "submit_dataset.txt", row.names = FALSE, quote = FALSE) 
