#-------------------------------------------------------------------------------
#
# This script is for the "Getting And Cleaning Data" course project
# assignment.  It does the following:
# 
# 1. Merges the training and the test sets to create one data set.
# 
# 2. Extracts only the measurements on the mean and standard deviation
#    for each measurement. 
# 
# 3. Uses descriptive activity names to name the activities in the data set
# 
# 4. Appropriately labels the data set with descriptive variable names. 
# 
# 5. From the data set in step 4, creates a second, independent
#    tidy data set with the average of each variable for each
#    activity and each subject.
#
#-------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
#-------------------------------------------------------------------------------
# 0. Download and prepare data sources
#-------------------------------------------------------------------------------

dataDir <- getwd() %>% paste("data", sep="/")

if (!dir.exists(dataDir)) { dir.create(dataDir) }

src <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
split <- unlist(str_split(src, "/"))
zipFile <- paste(dataDir, split[length(split)], sep="/")
download.file(src, zipFile, mode="wb")
unzip(zipFile, exdir=dataDir)

#-------------------------------------------------------------------------------
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation
#    for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set.
#-------------------------------------------------------------------------------
data <- paste(dataDir, "UCI HAR Dataset", sep="/")

# get column names, activity labels

colNames <- read.table(paste(data, "features.txt", sep="/"))[,2]
activityLabels <- read.table(paste(data, "activity_labels.txt", sep="/"),
                             col.names=c("id", "label"))

# read observations into a single dataframes, assign column names

allObservations <- rbind(read.table(paste(data, "test/X_test.txt", sep="/")),
                         read.table(paste(data, "train/X_train.txt", sep="/")))
names(allObservations) <- colNames

# get subject and activity vectors, name them

allSubjects <- rbind(read.table(paste(data, "test/subject_test.txt", sep="/"),
                                col.names="subject"),
                     read.table(paste(data, "train/subject_train.txt", sep="/"),
                                col.names="subject"))

allActivities <- rbind(read.table(paste(data, "test/y_test.txt", sep="/"),
                                  col.names="activity"),
                       read.table(paste(data, "train/y_train.txt", sep="/"),
                                  col.names="activity"))

# replace the activity identifier with its corresponding label

allActivities <- lapply(allActivities, function(x) {activityLabels[x,][,2]})

# bind activities and subject vectors onto the observations dataframe

allObservations <- cbind(allSubjects, allActivities, allObservations)

# select relevant columns (mean and standard deviation data)
# Note: strict selection of only those variables with '-mean()' or '-std()'

allObservations <- select(allObservations,
                          c(subject,
                            activity,
                            str_subset(names(allObservations),
                                       "-mean\\(\\)|-std\\(\\)")))

# clean up the column names
names(allObservations) <- str_replace_all(names(allObservations), "[()]", "")

#-------------------------------------------------------------------------------
# 5. Create a new, independent tidy data set from allObservations 
#    with the average of each variable for each activity and each subject.
#-------------------------------------------------------------------------------

summarizedOutput <- group_by(allObservations, subject, activity) %>%
                    summarize_all(mean)

# save the dataset as a csv file in the ./data subfolder

write.table(summarizedOutput, "./data/summarizedOutput.csv", sep=",",
            row.names = FALSE)

#-------------------------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive variable names. 
#    Generate a simple codebook
#-------------------------------------------------------------------------------

fullNames <- names(summarizedOutput)
colClass <- sapply(summarizedOutput, class)
measureCatg <- str_split_i(fullNames, "[^f|^t]", 1) # leftmost
measureName <- ifelse(str_sub(fullNames,1,1) %in% c("f", "t"),
                      str_sub(str_split_i(fullNames, "-", 1), start=2),
                      fullNames) # the name
measureCalc <- str_split_i(fullNames, "-", 2) # the calculation
measureAxis <- str_split_i(fullNames, "-", 3) # the axis

# lookup table for variable name descriptions
lkupDesc <- cbind(sort(unique(measureName)),
                  c("activity performed by the subject (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)",
                    "body linear acceleration",
                    "body linear acceleration jerk signal",
                    "body linear acceleration jerk signal magnitude",
                    "body linear acceleration magnitude",
                    "body body linear acceleration jerk signal magnitude",
                    "body body angular velocity jerk signal magnitude",
                    "body body angular velocity magnitude",
                    "body angular velocity",
                    "body angular velocity jerk signal",
                    "body angular velocity jerk signal magnitude",
                    "body angular velocity magnitude",
                    "gravitational acceleration",
                    "gravitational acceleration magnitude",
                    "subject who performed the activity for each window sample. Its range is from 1 to 30"
                    ))

measureDesc <- lapply(measureName, function(x) {lkupDesc[which(lkupDesc[,1] == x),2]})

myCodebook <- as.data.frame(cbind(colClass,
                                  measureName,
                                  measureCatg,
                                  measureCalc,
                                  measureAxis,
                                  measureDesc)) %>%

    mutate("fullDescription" = paste("This attribute shows the",
                                     if_else (measureCalc == "mean",
                                              "mean of the",
                                              "standard deviation of the",
                                              missing=""),
                                     measureDesc,
                                     if_else (!is.na(measureAxis),
                                              paste("along the",
                                                    measureAxis,
                                                    "axis"), ""),
                                     if_else (measureCatg == "t",
                                     ". Origin is a time domain signal",
                                     ". Origin is a frequency (FFT) domain signal",
                                     missing="."))
        )

#-------------------------------------------------------------------------------
#                           --- End ---
#-------------------------------------------------------------------------------
