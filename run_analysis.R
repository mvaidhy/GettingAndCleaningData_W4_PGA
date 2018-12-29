# We will be using dplyr and data.table since we are dealing with large data sets
library(dplyr)
library(data.table)

# This file has some functions to clean the activity labels. See comments in the file for details
source("clean_label.R")

# Location of data files; to be updated as need. Content of zip file provided should be extracted into this directory
working_directory = "C:\\Users\\M.Vaidhy_LA\\Documents\\R\\GettingAndCleaningData_W4_PGA\\wearable"
setwd(working_directory)

# Read the 6 activity name and 561 measurement name labels from respective file (see README.txt for more details)
activity_labels <- read.table(file.path(working_directory, "Dataset", "activity_labels.txt"), stringsAsFactors = FALSE)
names(activity_labels) <- c("activity_id", "activity")
measurement_labels <- read.table(file.path(working_directory, "Dataset", "features.txt"), stringsAsFactors = FALSE)

# Read the measurements, subject id, and activity id from three different files for the 'test' data set
measurements_test <- fread(file=file.path(working_directory, "Dataset", "test", "x_test.txt"))
subject_factor_test <- read.table(file.path(working_directory, "Dataset", "test", "subject_test.txt"))
activity_factor_test <- read.table(file.path(working_directory, "Dataset", "test", "y_test.txt"))

# Read the measurements, subject id, and activity id from three different files for the 'train' data set
measurements_train <- fread(file=file.path(working_directory, "Dataset", "train", "x_train.txt"))
subject_factor_train <- read.table(file.path(working_directory, "Dataset", "train", "subject_train.txt"))
activity_factor_train <- read.table(file.path(working_directory, "Dataset", "train", "y_train.txt"))

# assignm the measurement names (raw) as pulled from features.txt for the two data tables
names(measurements_test) <- measurement_labels[,2]
names(measurements_train) <- measurement_labels[,2]

# create vector with descriptive column names from raw data
# step 1: make names descriptive
measurement_labels_clean <- sapply(measurement_labels[,2], clean_measurement_label, simplify = TRUE, USE.NAMES = FALSE)
# step 2: make names unique, since there are a bunch of duplicate column names
measurement_labels_clean <- make_measurement_labels_unique(measurement_labels_clean)

# Combine all subject id, activity id, and measurement into one data table for 'test' data and assign descriptive column names
all_data_test <- cbind(subject_factor_test, activity_factor_test, measurements_test)
names(all_data_test) <- c("subject_id", "activity_id", measurement_labels_clean)
all_data_test <- tbl_df(all_data_test)

# Combine all subject id, activity id, and measurement into one data table for 'train' data and assign descriptive column names
all_data_train <- cbind(subject_factor_train, activity_factor_train, measurements_train)
names(all_data_train) <- c("subject_id", "activity_id", measurement_labels_clean)
all_data_train <- tbl_df(all_data_train)

# Create first data table
# 1. Bind test and train data tables into one data table
# 2. Merge with activity name table to replace activity id with activity name
# 3. Create subset of data table which only holds the mean and std columns
all_data_subset <- rbind(all_data_test, all_data_train) %>% merge(activity_labels, by.x = "activity_id", by.y = "activity_id", all = TRUE) %>% select(-matches("angle|mean_freq")) %>% select(subject_id, activity, matches("mean|std"))

# Final summary
tidy_wearables_summary <- group_by(all_data_subset, subject_id, activity) %>% summarise_all(funs(mean)) %>% arrange(subject_id, activity)

# Saving to file
# filter data table again to hold 'mean' columns only and svae to file.
write.csv(select(tidy_wearables_summary, subject_id, activity, matches("mean")), file.path(working_directory, "tidy_wearables_summary.csv"), row.names = FALSE)

# Clean up memory
rm(list=ls())
