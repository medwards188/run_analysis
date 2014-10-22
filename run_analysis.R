## Set working directory
setwd("~/Documents/Data Scientist Certification/Course 3/run_analysis")

## Unzip and load the data 
data <- "getdata-projectfiles-UCI HAR Dataset.zip"
## Check if the data exists
if ( file.exists(data) == FALSE) stop ("File not found.")

## If it exists, directly read the data
trainSubj <- read.table(unz(data,"UCI HAR Dataset/train/subject_train.txt"),header=FALSE,colClasses="integer")
trainX <- read.table(unz(data,"UCI HAR Dataset/train/X_train.txt"),header=FALSE,colClasses="numeric")
trainY <- read.table(unz(data,"UCI HAR Dataset/train/y_train.txt"),header=FALSE,colClasses="integer")

testSubj <- read.table(unz(data,"UCI HAR Dataset/test/subject_test.txt"),header=FALSE,colClasses="integer")
testX <- read.table(unz(data,"UCI HAR Dataset/test/X_test.txt"),header=FALSE,colClasses="numeric")
testY <- read.table(unz(data,"UCI HAR Dataset/test/y_test.txt"),header=FALSE,colClasses="integer")

## Merge the training and the test sets to create one data set
## Since the variable names will change, the complete block is not built here.
## Remember to rename the subject and activity, later below
subject <- rbind(trainSubj,testSubj)
featureData <- rbind(trainX,testX)
activity <- rbind(trainY,testY)

## To make memory space, remove no longer needed data tables
rm(trainSubj,trainX,trainY,testSubj,testX,testY)

## Read feature names
varNames <-read.table(unz(data,"UCI HAR Dataset/features.txt"),sep=" ")
featureNames <- as.character(varNames[,2]) # feature names are in column 2
rm(varNames) #free some memory space
names(featureData) <- featureNames
names(subject) <- c("Subject")
names(activity) <- c("Activity")

## Extract only the measurements on the mean and standard deviation for each measurement
featureXX <- featureData[, grep("mean\\()|std\\()",featureNames)]

## Rename the variables, use descriptive variable names
names(featureXX) <- gsub("^f", "frequency", names(featureXX))
names(featureXX) <- gsub("^t", "time", names(featureXX))
names(featureXX) <- gsub("Acc", "Acceleration", names(featureXX))
names(featureXX) <- gsub("Gyro", "Gyroscope", names(featureXX))
names(featureXX) <- gsub("std\\()","Std",names(featureXX))

## Convert mean() and std() to Mean and Std
names(featureXX) <- gsub("mean\\()","Mean",names(featureXX))

## Remove all "-"
names(featureXX) <- gsub("-", "", names(featureXX))

## Add the activities and subjects to the features, for the complete block
featureXX <- cbind(featureXX, subject, activity)
rm (featureData, subject, activity) #free some memory space

## Use descriptive activity names to name the activities in the data set
## Read the labels from file
actLabels <- read.table(unz(data,"UCI HAR Dataset/activity_labels.txt"),header=FALSE,stringsAsFactors=FALSE)
## Appropriately label the data set with descriptive variable names. 
featureXX$Activity <- factor(featureXX$Activity,levels=actLabels[,1],labels=actLabels[,2])

## Create a second tidy data set with the average of each variable for each activity and each subject
## Load reshape
library(reshape)
## Melt the data, using the variables for Mean and Std
measures <- grep("Mean|Std",names(featureXX),value=TRUE)
molten <- melt(featureXX,id.vars=c("Subject","Activity"),measure.vars=measures)
## Cast the the data into the wanted mold
tidydata2 <- cast(molten,Subject+Activity~variable,mean)

## Write the data to a csv file
write.csv(tidydata2,file="tidyData.csv",row.names=FALSE)

## Clean
rm(actLabels,molten,featureXX,tidydata2)
rm(data,featureNames,measures)
message("CSV file written.")
