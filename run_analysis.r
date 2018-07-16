### install if required ###

install.packages("dplyr")
install.packages("data.table")
install.packages("tidyr")



####libraries needed #####

library(dplyr)
library(data.table)
library(tidyr)


#### set a folder ###
DataFolder <- "C:/Users/carly/Documents/getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset"

### get Subject data using read table ###
SubjectTrain <- tbl_df(read.table(file.path(DataFolder, "train", "Subject_train.txt")))
SubjectTest  <- tbl_df(read.table(file.path(DataFolder, "test" , "Subject_test.txt" )))

### get Activity data using read table ###
ActivityTrain <- tbl_df(read.table(file.path(DataFolder, "train", "Y_train.txt")))
ActivityTest  <- tbl_df(read.table(file.path(DataFolder, "test" , "Y_test.txt" )))

### get test and train data using read table ###
Train <- tbl_df(read.table(file.path(DataFolder, "train", "X_train.txt" )))
Test  <- tbl_df(read.table(file.path(DataFolder, "test" , "X_test.txt" )))


### merge Activity and Subject files by row binding ####

SubjectData <- rbind(SubjectTrain, SubjectTest)
ActivityData<- rbind(ActivityTrain, ActivityTest)

#### name variables ####
setnames(SubjectData, "V1", "Subject")
setnames(ActivityData, "V1", "ActivityNumber")

### merge train and test files by row binding ####

dataTable <- rbind(Train, Test)

#### name variables according to Feature ####
FeatureData <- tbl_df(read.table(file.path(DataFolder, "Features.txt")))
setnames(FeatureData, names(FeatureData), c("FeatureNum", "FeatureName"))
colnames(dataTable) <- FeatureData$FeatureName

### name columns for Activities ### 
ActivityLabels<- tbl_df(read.table(file.path(DataFolder, "Activity_labels.txt")))
setnames(ActivityLabels, names(ActivityLabels), c("ActivityNumber","ActivityName"))

#### Merge columns to make one dataset ###
SubjectActivityData <- cbind(SubjectData, ActivityData)
dataTable <- cbind(SubjectActivityData, dataTable)

### Eextract only the mean and standard deviation of Features ####
FeatureDataMeanSD <- grep("mean\\(\\)|std\\(\\)",FeatureData$FeatureName,value=TRUE) 

#### Take only means and SD adding the Subjects and activity numbers ###

FeatureDataMeanSD <- union(c("Subject","ActivityNumber"), FeatureDataMeanSD)
dataTable<- subset(dataTable,select=FeatureDataMeanSD) 

### add name of Activity into dataTable ###
dataTable <- merge(ActivityLabels, dataTable , by="ActivityNumber", all.x=TRUE)
dataTable$ActivityName <- as.character(dataTable$ActivityName)

### create dataTable with variable means sorted by Subject and Activity ###
dataTable$ActivityName <- as.character(dataTable$ActivityName)

dataAggr<- aggregate(. ~ Subject - ActivityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,Subject,ActivityName))

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "Mean", names(dataTable))
names(dataTable)<-gsub("^t", "Time", names(dataTable))
names(dataTable)<-gsub("^f", "Frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

#### CHECK NEW NAMES ####
head(str(dataTable),6)

### average of each variable for each activity and each subject ###
write.table(dataTable, "TidyData.txt", row.name=FALSE)
