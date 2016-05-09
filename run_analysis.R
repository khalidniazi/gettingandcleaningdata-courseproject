############################### Getting and cleaning data course project ################################
## Authored : Khalid Niazi
## Dataset has been obtained from below URL
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## Getting and cleaning data course project desired Deliverables are mentioned below


## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##########################################################################################################


rm(list=ls())

## 1. Merge the training and the test sets to create one data set.

# change the current working directory to the location of UCI HAR Dataset
setwd('E:/Data Science Specialization/3 - Getting and cleaning data/Week4/Assignment/Project_data/UCI HAR Dataset');

# Read feature names from file features.txt and activity description from file activity_labels.txt
FeatureInformation <- read.table('./features.txt',header=FALSE); # fetch data from features.txt
ActivityInformation <- read.table('./activity_labels.txt',header=FALSE); # fetch data from activity_labels.txt

# Read in the Training and Testing data
SubjectTrain <- read.table('./train/subject_train.txt',header=FALSE); # fetch data from subject_train.txt
XTrain <- read.table('./train/x_train.txt',header=FALSE); # fetch data from x_train.txt
YTrain <- read.table('./train/y_train.txt',header=FALSE); # fetch data from y_train.txt
SubjectTest <- read.table('./test/subject_test.txt',header=FALSE); # fetch data from subject_test.txt
XTest <- read.table('./test/x_test.txt',header=FALSE); # fetch data from x_test.txt
YTest <- read.table('./test/y_test.txt',header=FALSE); # fetch data from y_test.txt


# Assigining column names to the fetched Data
colnames(ActivityInformation) <- c('ActivityId','ActivityType');
colnames(SubjectTrain) <- "SubjectId";
colnames(XTrain) <- FeatureInformation[,2]; 
colnames(YTrain) <- "ActivityId";
colnames(SubjectTest) <- "SubjectId";
colnames(XTest) <- FeatureInformation[,2]; 
colnames(YTest) <- "ActivityId";


# Createing the final training and testing DataSet
TrainingData <- cbind(YTrain,SubjectTrain,XTrain);
TestingData <- cbind(YTest,SubjectTest,XTest);

# Combine training and test data to create a final MergedData set
MergedData <- rbind(TrainingData,TestingData);

# Vector Containing all Feature Names
FeatureNames <- colnames(MergedData); 
##################################### deliverable 1 completed #############################

## 2. Extract only the measurements on the mean and standard deviation for each measurement. 

#Use Feature Names to extract only standard deviation and Mean Measurements
FeaturesSelected <- grep(".*mean.*|.*std.*|.*Id.*", FeatureNames ) # Mean and standard deviation Features selected
MergedData <- MergedData[FeaturesSelected]; # Dataset containing Mean and standard deviation Features only
##################################### deliverable 2 completed #############################

## 3. Use descriptive activity names to name the activities in the data set

# Including Descriptive activity Names in the Data
MergedData <- merge(MergedData,ActivityInformation,by='ActivityId',all.x=TRUE);

# Updated Feature Names Vector containing ActivityType as well
FeatureNames <- colnames(MergedData); 
##################################### deliverable 3 completed #############################

## 4. Appropriately label the data set with descriptive activity names. 

# Rephrasing Variable names to make them more readable
FeatureNames <- gsub('[-()]','', FeatureNames);
FeatureNames <- gsub('std$','StdDeviation',FeatureNames);
FeatureNames <- gsub('mean','Mean',FeatureNames);
FeatureNames <- gsub('JerkMag','JerkMagnitude',FeatureNames);
FeatureNames <- gsub('^(f)','frequency',FeatureNames);
FeatureNames <- gsub('^(t)','time',FeatureNames);
FeatureNames <- gsub('BodyBody','Body',FeatureNames);

# assigning the updated more explainatory names to the data set
colnames(MergedData) <- FeatureNames;
##################################### deliverable 4 completed #############################

## 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Removing activity type from data so that we can calculate Mean
MergedDataNoActivity <- MergedData[,names(MergedData) != 'ActivityType'];

# Reducing the data to contain only the Mean of each Feature
TidyDataInterim <- aggregate(MergedDataNoActivity[,names(MergedDataNoActivity) != c('ActivityId','SubjectId')],by=list(ActivityId=MergedDataNoActivity$ActivityId,SubjectId = MergedDataNoActivity$SubjectId),mean);

# Finally Tagging activity Type to the newly created Tidy Data
TidyData <- merge(TidyDataInterim,ActivityInformation,by='ActivityId',all.x=TRUE);

# Write the final data to a file named 'TidyData.txt'
write.table(TidyData,"TidyData.txt",row.names=TRUE,sep='\t',quote = FALSE);
##################################### deliverable 5 completed #############################
