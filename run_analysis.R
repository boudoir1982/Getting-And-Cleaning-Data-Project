## Getting and Cleaning Data 
# # run_analysis.R

#1. Combine data
activitiesList.test = read.table(file = "UCI HAR Dataset/test/y_test.txt")
activitiesList.train = read.table(file = "UCI HAR Dataset/train/y_train.txt")
activitiesList.onedata = rbind(activitiesList.test,activitiesList.train)

# test.data = read.table(file = paste(datadir,"UCI HAR Dataset//test/X_test.txt",sep=""),row.names = is.character(activitiesList.test))
test.data = read.table(file = "UCI HAR Dataset/test/X_test.txt")
train.data = read.table(file = "UCI HAR Dataset/train/X_train.txt")
onedata.data = rbind(train.data,test.data)

#2. features subset 
featureFile <- readLines("features.txt")
feature_ind <- grep("std|mean",featureFile)
write.table(featureFile[feature_ind],file = "features_ms.txt",quote = FALSE, row.names = FALSE, col.names = FALSE)
featuredata = onedata.data[,feature_ind]
featureMat <- as.matrix(featuredata)


#3. descriptive activity names
# activitiesList.onedata[2,1]
activitiesMap = read.table(file = "./activity_labels.txt")
activitiesList.tmp <- as.matrix(activitiesList.onedata)
for (n in activitiesMap[,1]) {
  activitiesList.tmp <- gsub(activitiesMap[n,1],activitiesMap[n,2],activitiesList.tmp)
}
activitiesList.descrip <- activitiesList.tmp
# rename the row names or column names
rownames(featureMat,do.NULL = TRUE,prefix = "KQR") 
rownames(featureMat) <- activitiesList.descrip

#4. descriptive variable names for data sets
newName <- function(oldName, Mapfile) {
  # change oldName to newName
  # using Mapfile two column map file
  Map = read.table(file = Mapfile)
  rownames(Map) <- Map$V1
  tmp <- (oldName)
  for (n in Map[,1]) {
    tmp <- gsub(paste("^V",Map[as.character(n),1],"$",sep = ""),Map[as.character(n),2],tmp)
  }
  tmp
}

# Map = read.table(file = "features_ms.txt")
# rownames(Map) <- Map$V1
# tmp <- (colnames(featuredata))
# for (n in Map[,1]) {
#   tmp <- gsub(paste("^V",Map[as.character(n),1],"$",sep = ""),Map[as.character(n),2],tmp)
# }
# tmp

dataSetName <- newName(colnames(featuredata),"features_ms.txt")
# rename the col names 
colnames(featureMat,do.NULL = TRUE,prefix = "KQC") 
colnames(featureMat) <- dataSetName

#5. average per activity per subject
subjectList.test = read.table("./test/subject_test.txt")
subjectList.train = read.table("./train/subject_train.txt")
subjectList.onedata = rbind(subjectList.test,subjectList.train)

Nsubjects <- length(unique(subjectList.onedata)[[1]] )
totRow <- Nsubjects*length(activitiesMap[,2])
subject <- matrix(data=NA,nrow=totRow,ncol=length(dataSetName))
rownames(subject) <- rep(activitiesMap[,2], Nsubjects)
colnames(subject) <- dataSetName
for (n in sort(unique(subjectList.onedata)[,1]) ) {
  # for each subject 
  sub <- featureMat[subjectList.onedata==n,]
  sub_m <- matrix(data=NA,nrow=length(activitiesMap[,2]),ncol=length(dataSetName))
  # dim(sub_m) <- c(length(activitiesMap[,2]),length(dataSetName))
  rownames(sub_m) <- activitiesMap[,2]
  for (a in activitiesMap[,2]){ 
    sub_m[a,] <- colMeans(sub[rownames(sub)==a,])
  }
  subject[(1+(n-1)*length(activitiesMap[,2])):(n*length(activitiesMap[,2])), ] <- sub_m
  
}

# NOTE: it seems the data was messed up. standard deviation has negative values! e.g. column 4,5,6.
# 1 tBodyAcc-mean()-X
# 2 tBodyAcc-mean()-Y
# 3 tBodyAcc-mean()-Z
# 4 tBodyAcc-std()-X
# 5 tBodyAcc-std()-Y
# 6 tBodyAcc-std()-Z
# ...

tidydata <- subject
write.table(tidydata,file = "tidydata.txt",row.names = FALSE)