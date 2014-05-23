#check if file is already unzipped
if (!file.exists("UCI HAR Dataset")) {
  if (!file.exists("getdata_projectfiles_UCI HAR Dataset.zip")) {
    stop("was expecting HAR Dataset folder or zip file")
  } else {
    unzip("getdata_projectfiles_UCI HAR Dataset.zip")
  }
}

# Step1. Merges the training and the test sets to create one data set.

train_Data <- read.table("./data/train/X_train.txt")

train_Label <- read.table("./data/train/y_train.txt")
table(train_Label)

train_Subject <- read.table("./data/train/subject_train.txt")
test_Data <- read.table("./data/test/X_test.txt")
dim(test_Data)

test_Label <- read.table("./data/test/y_test.txt") 
table(test_Label) 

test_Subject <- read.table("./data/test/subject_test.txt")

join_Data <- rbind(train_Data, test_Data)
dim(join_Data)

join_Label <- rbind(train_Label, test_Label)
dim(join_Label)

join_Subject <- rbind(train_Subject, test_Subject)
dim(join_Subject)

# Step2. Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
features <- read.table("./data/features.txt")
dim(features)
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)
join_Data <- join_Data[, meanStdIndices]
dim(joinData)
names(join_Data) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(join_Data) <- gsub("mean", "Mean", names(join_Data)) # capitalize M
names(join_Data) <- gsub("std", "Std", names(join_Data)) # capitalize S
names(join_Data) <- gsub("-", "", names(join_Data)) # remove "-" in column names 

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("./data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activity_Label <- activity[join_Label[, 1], 2]
join_Label[, 1] <- activity_Label
names(join_Label) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(join_Subject) <- "subject"
cleaned_Data <- cbind(join_Subject, join_Label, join_Data)
dim(cleaned_Data)
write.table(cleaned_Data, "merged_data.txt")

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 

subject_Len <- length(table(join_Subject)) 
activity_Len <- dim(activity)[1]
column_Len <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subject_Len*activity_Len, ncol=column_Len) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleaned_Data)
row <- 1
for(i in 1:subject_Len) {
  for(j in 1:activity_Len) {
    result[row, 1] <- sort(unique(join_Subject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleaned_Data$subject
    bool2 <- activity[j, 2] == cleaned_Data$activity
    result[row, 3:column_Len] <- colMeans(cleaned_Data[bool1&bool2, 3:column_Len])
    row <- row + 1
  }
}
head(result)
write.table(result, "tidy_final_data.txt")
