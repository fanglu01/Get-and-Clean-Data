##setwd("C:/Users/Fang/Documents/DataScienceClass/Clean Data/Project")
install.packages("tidyr")
install.packages("dplyr")
Install.packages("reshape2")

library(tidyr)
library(dplyr)
library(reshape2)

#First read the data in R, and check their features

#Combined the "X_test.txt", "y_test.txt", and "subject_test.txt" in the test folder into "testdata", with the column ordered as Subject, Activity, and 561 features
testfile1<-read.table("../Project/test/X_test.txt")
dim(testfile1)
testfile2<-read.table("../Project/test/y_test.txt")
dim(testfile2)
testfile3<-read.table("../Project/test/subject_test.txt")
testdata<-cbind(testfile3, testfile2, testfile1)
dim(testfile3)

#Combined the "X_train.txt", "y_train.txt", and "subject_train.txt" in the train folder into "traindata", with the column ordered as Subject, Activity, and 561 features
testfile4<-read.table("../Project/train/x_train.txt")
dim(testfile4)
testfile5<-read.table("../Project/train/y_train.txt")
dim(testfile5)
testfile6<-read.table("../Project/train/subject_train.txt")
dim(testfile6)
traindata<-cbind(testfile6, testfile5, testfile4)

## Generate the compeletdata by merging the testdata and traindata, and gave the column names (Step1)
completedata<-rbind(testdata, traindata)

# Export the feature names from the feature.txt in order to generate the column names for the "completedata" set
features<-read.table("../Project/features.txt")
featureName<-as.character(features[,2])

# Label the columns in the complete data set
names(completedata)<- c("Subject", "Activity", featureName)

## Extracts only the measurements on the mean and standard deviation for each measurement (Step2)
tidydata <- completedata[, grep("Subject|Activity|mean()|std()", names(completedata))]
# Remove the -meanFreq() column from the "tidydata" to further clean the data
tidydata <- tidydata[, -grep("Freq", names(tidydata))]
# the resulting tidydata contained 66 features on the mean and std, plus the Subject and Activity


## Uses descriptive activity names to name the activities in the data set (Step3)
tidydata$Activity[tidydata$Activity==1] <- "WALKING"
tidydata$Activity[tidydata$Activity==2] <- "WALKIng_UPSTAIRS"
tidydata$Activity[tidydata$Activity==3] <- "WALKIng_DOWNSTAIRS"
tidydata$Activity[tidydata$Activity==4] <- "SITTINGS"
tidydata$Activity[tidydata$Activity==5] <- "STANDING"
tidydata$Activity[tidydata$Activity==6] <- "LAYING"

## To appropriately labels the data set with descriptive variable names (Step4) 
#I use "time" and "freq" to replace the begining "t" and "f",  remove the "()" , and change "-" to "_"in the feature names. 
names(tidydata) <- gsub("^t", "time", gsub("^f", "freq", names(tidydata) ) )
names(tidydata) <- gsub("mean", "Mean", gsub("std", "Std", names(tidydata) ) )
names(tidydata) <- gsub("\\()", "", gsub("\\-", "\\_", names(tidydata) ) )


## Base on the tidydata, creates the final_tidy_data set with the average of each variable for each activity and each subject (Step 5) 
# The final_tidy_data contains 180 rows and 68 column ( the first 2 column are "Subject" and "Postures" and the rest 66 ccolumns
# are the average of of each variable for each activity and each subject.
dm=melt(tidydata, id.var=c("Subject", "Activity"))
dmg<-group_by(dm, Subject, Activity, variable)
final_tidy_data<-dcast(dmg, Subject + Activity~variable, fun.aggregate = mean)

# The final_tidy_data was writen into "tidy_data.txt" file in the current working directory and was submitted for credits
write.table(final_tidy_data, "tidy_data.txt", row.name=FALSE)

# The "tidy_data.txt" can be viewed in R Studio using the following scripts
data <- read.table("tidy_data.txt", header = TRUE) 
View(data)
                   
# End of the run_analysis.R script




