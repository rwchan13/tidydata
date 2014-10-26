library(plyr)
#Load Raw Data
X_train <- read.table('train/X_train.txt', header=FALSE, sep='')
y_train <- read.table('train/y_train.txt', header=FALSE, sep='')
s_train <- read.table('train/subject_train.txt', header=FALSE, sep='')
X_test <- read.table('test/X_test.txt', header=FALSE, sep='')
y_test <- read.table('test/y_test.txt', header=FALSE, sep='')
s_test <- read.table('test/subject_test.txt', header=FALSE,sep='')

features <- read.table('features.txt', header=FALSE, sep='')

#function for labelling activities
labelActivities <- function(x){
         tmp <- rbind('WALKING',
                      'WALKING_UPSTAIRS',
                      'WALKING_DOWNSTAIRS',
                      'SITTING','STANDING','LAYING')
         tmp[x]
}

#Combine Traing and Test Data
X <- rbind(X_train, X_test)
y <- lapply(rbind(y_train, y_test), labelActivities)
s <- rbind(s_train, s_test)

#Basic Column Labelling
names(X) <- features$V2

#Create final tidy dataset
#Add Subjects and Activity Labels
final <- data.frame(ActivityLabels = y$V1,
                    Subjects = s$V1)

#Add only Mean and SD measurements
for (s in names(X)){
  if(length(grep('*mean()*', s, ignore.case=FALSE)) > 0 &
     length(grep('*meanFreq()*', s, ignore.case=FALSE)) == 0){
    final[s] <-X[s]
  }

  if(length(grep('*std()*', s, ignore.case=FALSE)) > 0){
    final[s] <-X[s]
  }
}

#Prettify Column Names
final <- rename(final,
c("tBodyAcc-mean()-X"="MeanOfTemporalBodyAccelerationInX",
"tBodyAcc-mean()-Y"="MeanOfTemporalBodyAccelerationInY",
"tBodyAcc-mean()-Z"="MeanOfTemporalBodyAccelerationInZ",
"tBodyAcc-std()-X"="StandardDeviationOfTemporalBodyAccelerationInX",
"tBodyAcc-std()-Y"="StandardDeviationOfTemporalBodyAccelerationInY",
"tBodyAcc-std()-Z"="StandardDeviationOfTemporalBodyAccelerationInZ",
"tGravityAcc-mean()-X"="MeanOfTemporalGravityAccelerationInX",
"tGravityAcc-mean()-Y"="MeanOfTemporalGravityAccelerationInY",
"tGravityAcc-mean()-Z"="MeanOfTemporalGravityAccelerationInZ",
"tGravityAcc-std()-X"="StandardDeviationOfTemporalGravityAccelerationInX",
"tGravityAcc-std()-Y"="StandardDeviationOfTemporalGravityAccelerationInY",
"tGravityAcc-std()-Z"="StandardDeviationOfTemporalGravityAccelerationInZ",
"tBodyAccJerk-mean()-X"="MeanOfTemporalBodyJerkInX",
"tBodyAccJerk-mean()-Y"="MeanOfTemporalBodyJerkInY",
"tBodyAccJerk-mean()-Z"="MeanOfTemporalBodyJerkInZ",
"tBodyAccJerk-std()-X"="StandardDeviationOfTemporalBodyJerkInX",
"tBodyAccJerk-std()-Y"="StandardDeviationOfTemporalBodyJerkInY",
"tBodyAccJerk-std()-Z"="StandardDeviationOfTemporalBodyJerkInZ",
"tBodyGyro-mean()-X"="MeanOfTemporalBodyAngularVelocityInX",
"tBodyGyro-mean()-Y"="MeanOfTemporalBodyAngularVelocityInY",
"tBodyGyro-mean()-Z"="MeanOfTemporalBodyAngularVelocityInZ",
"tBodyGyro-std()-X"="StandardDeviationOfTemporalBodyAngularVelocityInX",
"tBodyGyro-std()-Y"="StandardDeviationOfTemporalBodyAngularVelocityInX",
"tBodyGyro-std()-Z"="StandardDeviationOfTemporalBodyAngularVelocityInZ",
"tBodyGyroJerk-mean()-X"="MeanOfTemporalBodyAngularAccelerationInX",
"tBodyGyroJerk-mean()-Y"="MeanOfTemporalBodyAngularAccelerationInY",
"tBodyGyroJerk-mean()-Z"="MeanOfTemporalBodyAngularAccelerationInZ",
"tBodyGyroJerk-std()-X"="StandardDeviationOfTemporalBodyAngularAccelerationInX",
"tBodyGyroJerk-std()-Y"="StandardDeviationOfTemporalBodyAngularAccelerationInY",
"tBodyGyroJerk-std()-Z"="StandardDeviationOfTemporalBodyAngularAccelerationInZ",
"tBodyAccMag-mean()"="MeanOfTemporalBodyAccelerationMagnitude",
"tBodyAccMag-std()"="StandardDeviationOfTemporalBodyAccelerationMagnitude",
"tGravityAccMag-mean()"="MeanOfTemporalGravityAccelerationMagnitude",
"tGravityAccMag-std()"="StandardDeviationOfTemporalGravityAccelerationMagnitude",
"tBodyAccJerkMag-mean()"="MeanOfTemporalBodyJerkMagnitude",
"tBodyAccJerkMag-std()"="StandardDeviationOfTemporalBodyJerkMagnitude",
"tBodyGyroMag-mean()"="MeanOfTemporalBodyAngularVelocityMagnitude",
"tBodyGyroMag-std()"="StandardDevationOfTemporalBodyAngularVelocityMagnitude",
"tBodyGyroJerkMag-mean()"="MeanOfTemporalBodyAngularVelocityMagnitude",
"tBodyGyroJerkMag-std()"="StandardDevationOfTemporalBodyAngularVelocityMagnitude",
"fBodyAcc-mean()-X"="MeanOfFrequencyBodyAccelerationInX",
"fBodyAcc-mean()-Y"="MeanOfFrequencyBodyAccelerationInY",
"fBodyAcc-mean()-Z"="MeanOfFrequencyBodyAccelerationInZ",
"fBodyAcc-std()-X"="StandardDeviationOfFrequencyBodyAccelerationInX",
"fBodyAcc-std()-Y"="StandardDeviationOfFrequencyBodyAccelerationInY",
"fBodyAcc-std()-Z"="StandardDeviationOfFrequencyBodyAccelerationInZ",
"fBodyAccJerk-mean()-X"="MeanOfFrequencyBodyJerkInX",
"fBodyAccJerk-mean()-Y"="MeanOfFrequencyBodyJerkInY",
"fBodyAccJerk-mean()-Z"="MeanOfFrequencyBodyJerkInZ",
"fBodyAccJerk-std()-X"="StandardDeviationOfFrequencyBodyJerkInX",
"fBodyAccJerk-std()-Y"="StandardDeviationOfFrequencyBodyJerkInY",
"fBodyAccJerk-std()-Z"="StandardDeviationOfFrequencyBodyJerkInZ",
"fBodyGyro-mean()-X"="MeanOfFrequencyBodyAngularVelocityInX",
"fBodyGyro-mean()-Y"="MeanOfFrequencyBodyAngularVelocityInX",
"fBodyGyro-mean()-Z"="MeanOfFrequencyBodyAngularVelocityInX",
"fBodyGyro-std()-X"="StandardDeviationOfFrequencyBodyAngularVelocityInX",
"fBodyGyro-std()-Y"="StandardDeviationOfFrequencyBodyAngularVelocityInY",
"fBodyGyro-std()-Z"="StandardDeviationOfFrequencyBodyAngularVelocityInZ",
"fBodyAccMag-mean()"="MeanOfFrequencyBodyAccelerationMagnitude",
"fBodyAccMag-std()"="StandardDeviationOfFrequencyBodyAccelerationMagnitude",
"fBodyBodyAccJerkMag-mean()"="MeanOfFrequencyBodyJerkMagnitude",
"fBodyBodyAccJerkMag-std()"="StandardDeviationOfFrequencyBodyJerkMagnitude",
"fBodyBodyGyroMag-mean()"="MeanOfFrequencyBodyAngularVelocityMagnitude",
"fBodyBodyGyroMag-std()"="StandardDevationOfFrequencyBodyAngularVelocityMagnitude",
"fBodyBodyGyroJerkMag-mean()"="MeanOfTemporalBodyAngularVelocityMagnitude",
"fBodyBodyGyroJerkMag-std()"="StandardDevationOfTemporalBodyAngularVelocityMagnitude"))

namean <- function(x){mean(x, na.rm=TRUE)}

#Write the aggregate (by Subject and Activity) means of Final Tidy Data
final <- aggregate(final[,-c(1,2)], list(ActivityLabels = final$ActivityLabels, Subjects = final$Subjects),
                       namean)
write.table(final, 'final.txt', row.name=FALSE)