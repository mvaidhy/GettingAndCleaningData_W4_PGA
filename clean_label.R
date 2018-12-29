clean_measurement_label <- function(activity_label) {
  
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
  
  for (i in 1:561) {
    measurement_labels[i] <- paste0(measurement_labels[i], 
    if ((i >=303 & i <= 316) | (i >= 382 & i <= 395) | (i >= 461 & i <= 474)) {"_x"}
      else if ((i >= 317 & i <= 330 ) | (i >= 396 & i <= 409) | (i >= 475 & i <= 488)) {"_y"}
      else if ((i >= 331 & i <= 344) | (i >= 410 & i <= 423) | (i >= 489 & i <= 502)) {"_z"}
      else {""})
  }
  
  measurement_labels
}