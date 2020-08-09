setwd("C:/Users/57319/Downloads/getdata_projectfiles_UCI HAR Dataset (1)/GettingCleaningData")

  if (!file.exists("UCI HAR Dataset")) {unzip(filename)}
xtest <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
ytest <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activitylabels.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
  summarise_all(funs(mean))
  write.table(TidyData, "TidyData.txt", row.name=FALSE)
  dataDir <- downloadData()
  
  ### Merge
  
  readData <- function(path) {
    read.table(filePath(dataDir, path))
  }
  
  )

  if(is.null(Xtrain)) { Xtrain <- adData("train/X_train.txt") }
  if(is.null(Xtest))  { xest  <- readData("test/X_test.txt") }
Sub <- rbind(subjecttrain, subjecttest
x <- rbin(xtrain, xtest)
y <- rbind(ytrain, ytest)
  
  featureNames <- readData("features.txt")[, 2]
  names(merged) <- featureNames
  Merged <- cbind(sub, x,y)
 
  # Defining columns
  matches <- grep("(mean|std)\\(\\)", names(merged))
  limited <- merged[, matches]
  
  
  activityNames <-    c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
  activities <- activityNames[yMerged]
  
  names(limited) <- gsub("^t", "Time", names(limited))
  names(limited) <- gsub("^f", "Frequency", names(limited))
  names(limited) <- gsub("-mean\\(\\)", "Mean", names(limited))
  names(limited) <- gsub("-std\\(\\)", "StdDev", names(limited))
  names(limited) <- gsub("-", "", names(limited))
  names(limited) <- gsub("BodyBody", "Body", names(limited))
  
  
  tidy <- cbind(Subject = subjects, Activity = activities, limited)
  
 
  library(plyr)
  ### lumn means
  limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
  tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans)
  names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])
  

  write.table(tidyMeans, "tidyMeans.txt", row.names = FALSE)
  tidyMeans

TidyData <- completeData %>%
  group_by(subject, activity) %>%
write.table(TidyData, "TidyData.txt", row.name=FALSE)
