#1. Merge Data sets (train and test) into one data set #

trainData <- read.table("./data/X_train.txt")
dim(trainData) # 7352*561
head(trainData)
trainLabel <- read.table("./data/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./data/subject_train.txt")
testData <- read.table("./data/X_test.txt")
dim(testData) # 2947*561
testLabel <- read.table("./data/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./data/subject_test.txt")
joinData <- rbind(trainData, testData)
dim(joinData) # 10299*561
joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel) # 10299*1
joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject) # 10299*1

#2. Extracts only the measurements on the mean and standard deviation for each measurement  Read the features.txt file. This tells which variables in dt are measurements for the mean and standard deviation.# 
features <- read.table("./data/features.txt")
dim(features)  
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)
joinData <- joinData[, meanStdIndices]
dim(joinData) 
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) 
names(joinData) <- gsub("mean", "Mean", names(joinData)) 
names(joinData) <- gsub("std", "Std", names(joinData)) 
names(joinData) <- gsub("-", "", names(joinData))  

#3. Uses descriptive activity names to RENAME the activities in the data set
activity <- read.table("./data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

#4. Appropriately labels the data set with descriptive activity names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData) 
write.table(cleanedData, "merged_data.txt") 

#5. Creates a second tidy data set with the average of each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject))
activityLen <- dim(activity)[1] 
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
    for(j in 1:activityLen) {
        result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
        result[row, 2] <- activity[j, 2]
        bool1 <- i == cleanedData$subject
        bool2 <- activity[j, 2] == cleanedData$activity
        result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
        row <- row + 1
    }
}
head(result)
write.table(result, "data_with_means.txt") 

#make codebook#

knit("makeCodebook.Rmd", output = "codebook.md", encoding = "ISO8859-1", quiet = TRUE)



