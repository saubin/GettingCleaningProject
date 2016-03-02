#Prelim/Prep - Clear and set working directory-#
rm(list=ls())
setwd('/Users/SAubin/Documents/GettingCleaning/UCI HAR Dataset')

# Read Data sets (train, test,activity, features) #

#Trainsets--
xTrain <- read.table('./train/x_train.txt',header=FALSE)
yTrain <- read.table('./train/y_train.txt',header=FALSE)
trainSubjects <- read.table('./train/subject_train.txt',header=FALSE)

#Features, activity sets--
features <- read.table('./features.txt',header=FALSE)
activityType <- read.table('./activity_labels.txt',header=FALSE)

#Testsets--
xTest <- read.table('./test/x_test.txt',header=FALSE)
yTest<- read.table('./test/y_test.txt',header=FALSE) 
testSubjects <- read.table('./test/subject_test.txt',header=FALSE)

#Assign column names to new data sets#
colnames(activityType) <- c('activityId','activityType')
colnames(trainSubjects) <- "subjectId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <-"activityId"
colnames(testSubjects) <- "subjectId"
colnames(xTest) <- features[,2]
colnames(yTest)<-"activityId"

#Merge test/subject sets and train/subject sets
TrainData <-cbind(yTrain,trainSubjects,xTrain)
TestData <- cbind(yTest,testSubjects,xTest)

#Combine TestData and TrainData into one table
CombinedData <- rbind(TrainData,TestData)

#Identify column names in new vector used to select mean and stddev
colNames  <-colnames(CombinedData)

#Extract only mean and stddev columns
MeanStdvID <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

#Subset CombinedData to strip out only columns where MeanStdvID is TRUE
CombinedData<-CombinedData[MeanStdvID==TRUE]

#Combine activityType table with combinedData so as to include activity names
CombinedData <- merge(CombinedData,activityType,by='activityId',all.x=TRUE)

#Update colNames vector with new columns
colNames  <-colnames(CombinedData)

#Update data set with clear activity naming convention and proper capitalization
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StandardDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","Time",colNames[i])
  colNames[i] = gsub("^(f)","Freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMag",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

#Apply new column names to CombinedData
colnames(CombinedData) <- colNames

#Remove activity Type
CombinedDataNoActivityType <- CombinedData[,names(CombinedData) !='ActivityType']

#Summarize new data set to include mean on each activity and subject
finalcleanData <- aggregate(CombinedDataNoActivityType[,names(CombinedDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=CombinedDataNoActivityType$activityId,subjectId = CombinedDataNoActivityType$subjectId),mean)

#Remerge with activity type to include naming convention

finalcleanData <- merge(finalcleanData,activityType,by='activityId',all.x=TRUE)

#Save final data set

write.table(finalcleanData, './finalcleanData.txt',row.names=TRUE,sep='\t')


