# A. Preliminary stuff
# The code assumes that there is a folder in the working directory called 
# "UCI HAR Dataset" with the (unzipped) data files
library(plyr)
library(gsubfn)
setwd("./UCI HAR Dataset")

# B. Load Variable names from features.txt and Activity names from activity_labels.txt
varnames <- as.character(read.table("features.txt")[,2])
actnames <- tolower(as.character(read.table("activity_labels.txt")[,2]))

# C. Load Test data (X_test.txt), along with activity labels (y_test.txt) and 
# subject ID (subject_test.txt) and merge them into a single data frame
testdata <- read.table("./test/X_test.txt")
testact <- read.table("./test/y_test.txt")
testact <- as.factor(testact[,1])
testact <- mapvalues(testact, from=c(1,2,3,4,5,6), to=actnames)
testsubj <- read.table("./test/subject_test.txt")
test <- cbind(testsubj, testact, testdata)
colnames(test) <- c("subject", "activity", varnames)

# D. Load Train data (X_train.txt), along with activity labels (y_train.txt) and 
# subject ID (subject_train.txt) and merge them into a single data frame
traindata <- read.table("./train/X_train.txt")
trainact <- read.table("./train/y_train.txt")
trainact <- as.factor(trainact[,1])
trainact <- mapvalues(trainact, from=c(1,2,3,4,5,6), to=actnames)
trainsubj <- read.table("./train/subject_train.txt")
train <- cbind(trainsubj, trainact, traindata)
colnames(train) <- c("subject", "activity", varnames)

# ASSIGNMENT:
# 1. Merges the training and the test sets to create one data set
merged <- rbind(test, train)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
meanORsd <- grepl("mean",varnames) | grepl("std",varnames)
meanORsd <- c(TRUE, TRUE, as.logical(meanORsd - grepl("meanFreq",varnames)))
extract <- merged[,meanORsd]

# 3. Uses descriptive activity names to name the activities in the data set
# This was already done previously. See Parts C and D

# 4. Appropriately labels the data set with descriptive activity names.
newnames <- tolower(gsub("-mean\\(\\)-", ".mean.", colnames(extract)))
newnames <- tolower(gsub("-mean\\(\\)", ".mean", newnames))
newnames <- tolower(gsub("-std\\(\\)-", ".std.", newnames))
newnames <- tolower(gsub("-std\\(\\)", ".std", newnames))
colnames(extract) <- newnames

# 5. Creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject. 
# The final output is the data frame "tidyaverage", which is the tidy data set.
nsubject <- length(unique(extract$subject))
nactivity <- length(unique(extract$activity))
tidyaverage <- data.frame(matrix(NA, nrow=nsubject*nactivity, ncol=ncol(extract)))
colnames(tidyaverage) <- colnames(extract)
for (i in 1:nsubject) {
    ca <- 0
    for (j in unique(extract$activity)) {
        ca <- ca + 1
        tidyaverage[(nactivity*(i-1)+ca),1] <- i
        tidyaverage[(nactivity*(i-1)+ca),2] <- j
        tidyaverage[(nactivity*(i-1)+ca),3:ncol(extract)] <- 
            sapply(extract[extract$subject==i & extract$activity==j,3:ncol(extract)], mean)
    }
}
write.table(tidyaverage, file="OUTPUT.txt")