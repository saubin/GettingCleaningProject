#1. Merge Data sets (train and test) into one data set #

trainDatapoints <- read.table("./data/X_train.txt")
dim(trainDatapoints) # 7352*561
head(trainDatapoints)
trainLabels <- read.table("./data/y_train.txt")
table(trainLabels)
trainSubjects <- read.table("./data/subject_train.txt")
testDatapoints <- read.table("./data/X_test.txt")
dim(testDatapoints) # 2947*561
testLabels <- read.table("./data/y_test.txt") 
table(testLabels) 
testSubjects <- read.table("./data/subject_test.txt")
joinedDatapoints <- rbind(trainDatapoints, testDatapoints)
dim(joinedDatapoints) # 10299*561
joinedLabels <- rbind(trainLabels, testLabels)
dim(joinedLabels) # 10299*1
joinedSubjects <- rbind(trainSubjects, testSubjects)
dim(joinedSubjects) # 10299*1

#2. Extracts only the measurements on the mean and standard deviation for each measurement  Read the features.txt file. This tells which variables in dt are measurements for the mean and standard deviation.# 
features <- read.table("./data/features.txt")
dim(features)  
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)
joinedDatacalc <- joinedDatapoints[, meanStdIndices]
dim(joinedDatacalc) 
names(joinedDatacalc) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) 
names(joinedDatacalc) <- gsub("mean", "Mean", names(joinedDatacalc)) 
names(joinedDatacalc) <- gsub("std", "Std", names(joinedDatacalc)) 
names(joinedDatacalc) <- gsub("-", "", names(joinedDatacalc))  

#3. Uses descriptive activity names to RENAME the activities in the data set
activitydata <- read.table("./data/activity_labels.txt")
activitydata[, 2] <- tolower(gsub("_", "", activitydata[, 2]))
substr(activitydata[2, 2], 8, 8) <- toupper(substr(activitydata[2, 2], 8, 8))
substr(activitydata[3, 2], 8, 8) <- toupper(substr(activitydata[3, 2], 8, 8))
activitydataLabel <- activitydata[joinedLabels[, 1], 2]
joinedLabels[, 1] <- activitydataLabel
names(joinedLabels) <- "activity"

#4. Appropriately labels the data set with descriptive activity names. 
names(joinedSubjects) <- "subject"
cleanData <- cbind(joinedSubjects, joinedLabels, joinedDatapoints)
dim(cleanData) 
write.table(cleanData, "cleanmergeddata.txt") 

#5. Creates a second tidy data set with the average of each variable for each activity and each subject. 
subjectLen <- length(table(joinedSubjects))
activityLen <- dim(activitydata)[1] 
columnLen <- dim(cleanData)[2]
finalresult <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
finalresult <- as.data.frame(finalresult)
colnames(finalresult) <- colnames(cleanData)
row <- 1
for(i in 1:subjectLen) {
    for(j in 1:activityLen) {
        finalresult[row, 1] <- sort(unique(joinedSubjects)[, 1])[i]
        finalresult[row, 2] <- activitydata[j, 2]
        bool1 <- i == cleanData$subject
        bool2 <- activitydata[j, 2] == cleanData$activitydata
        finalresult[row, 3:columnLen] <- colMeans(cleanData[bool1&bool2, 3:columnLen])
        row <- row + 1
    }
}
head(result)
write.table(result, "finalsetwithmeans.txt") 






