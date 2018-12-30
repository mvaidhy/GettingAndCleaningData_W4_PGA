clean_measurement_label <- function(activity_label) {
  
  # Strategy for making labels descriptive
  # 1. Remove cryptic prefix 'f' and 't' and use 'freq domain' and 'time domain' instead
  # 2. Chnage camel case and separate by underscores, e.g. tBodyAcc-iqr  becomes time_domain_body_acc_iqr
  # 3. Change all variables to lower case
  # 4. Remove empty parantheses, e.g., tBodyAcc-iqr()-X becomes time_domain_body_acc_iqr_x
  # 5. Fix columns 516 to 554 where we have 'BodyBody' instead of 'Body'
  # 6. Remove extra ")" in column 556 (not implemented)
  # 7. Change spaces and hyphens to underscores
  # 8. Make column names unique (see function make_measurement_labels_unique)
  if (length(grep("^angle", activity_label)) == 0) {
    #Remove empty parantheses
    activity_label <- gsub("\\(\\)", "", activity_label)
    
    #Change hyphens and commas to spaces
    activity_label <- gsub("\\,", " ", gsub("\\-", " ", activity_label))
    
    #change BodyBody to Body
    activity_label <- gsub("BodyBody", "Body", activity_label)
    
    #Separate Camel case words
    activity_label <- gsub("([A-Z])", " \\1", activity_label)
    
    #expand prefix    
    activity_label <- gsub("^f ", "freq domain ", activity_label)
    activity_label <- gsub("^t ", "time domain ", activity_label)
  }

  
  activity_label <- gsub(" ", "_", tolower(activity_label))
  activity_label <- gsub("__", "_",activity_label)
  
  activity_label
}

make_measurement_labels_unique <- function(measurement_labels) {
  # There are three sets of repeated column names, in each of these sets, the column name 
  # (e.g., fBodyGyro-bandsEnergy()) is repeated three times: once each for
  # x, y, and z values. Add the suffix x, y, and Z to make the column names unique
  for (i in 1:561) {
    measurement_labels[i] <- paste0(measurement_labels[i], 
    if ((i >=303 & i <= 316) | (i >= 382 & i <= 395) | (i >= 461 & i <= 474)) {"_x"}
      else if ((i >= 317 & i <= 330 ) | (i >= 396 & i <= 409) | (i >= 475 & i <= 488)) {"_y"}
      else if ((i >= 331 & i <= 344) | (i >= 410 & i <= 423) | (i >= 489 & i <= 502)) {"_z"}
      else {""})
  }
  
  measurement_labels
}


  