run_analysis <- function() {
  
  #include libraries
  library(dplyr)

  headers <- read.csv("UCI HAR Dataset/features.txt", sep = " ", header = FALSE)
  
  #get the columns needed: mean() and std()
  cols <- grep("(mean|std)\\(\\)", headers[,2])
  
  #Tidy up the header
  #  1. Replace t by TimeDomain, f by FrequencyDomain
  #  2. Remove '.', ',', '-'
  #  3. Replace Acc with Accelerometer
  #  4. Replace Gyro with Gyroscope
  #  5. Replace Mag with Magnitude
  #  6. Replace X/Y/Z with InXAxis/InYAxis/InZAxis
  #  7. Replace mean()/std() with Mean and StandardDeviation
  #  8. Replace Jerk with JerkSignal
  tidied_header <- headers[,2]
  tidied_header <- gsub("^t(.*)", "TimeDomain\\1", tidied_header)
  tidied_header <- gsub("^f(.*)", "FrequencyDomain\\1", tidied_header)
  tidied_header <- gsub("[.|,]", "", tidied_header)
  tidied_header <- gsub("-","",tidied_header)
  tidied_header <- sub("Acc", "Accelerometer", tidied_header)
  tidied_header <- sub("Gyro", "Gyroscope", tidied_header)
  tidied_header <- sub("Mag", "Magnitude", tidied_header)
  tidied_header <- sub("X", "InXAxis", tidied_header)
  tidied_header <- sub("Y", "InYAxis", tidied_header)
  tidied_header <- sub("Z", "InZAxis", tidied_header)
  tidied_header <- sub("mean\\(\\)", "Mean", tidied_header)
  tidied_header <- sub("std\\(\\)", "StandardDeviation", tidied_header)
  tidied_header <- sub("Jerk", "JerkSignal", tidied_header)
  
  #read test and train files with tidied headers, and cbind the subject and 
  #activity from the other 2 files
  df_x_test <- read.fwf("UCI HAR Dataset/test/X_test.txt", widths = rep(16, 561), header = FALSE, col.names = tidied_header)
  df_y_test <- read.table("UCI HAR Dataset/test/Y_test.txt", col.names = "action")
  df_subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
  df_test <- cbind(df_x_test, df_y_test, df_subject_test)
  
  df_x_train <- read.fwf("UCI HAR Dataset/train/X_train.txt", widths = rep(16, 561), header = FALSE, col.names = tidied_header)
  df_y_train <- read.table("UCI HAR Dataset/train/Y_train.txt", col.names = "action")
  df_subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
  df_train <- cbind(df_x_train, df_y_train, df_subject_train)
  
  #combine the 2 data frames, and subset only the columns with mean() and std()
  #by applying column filter using cols
  #Last 2 cols (562,563) are for the subject and action
  df_merged <- rbind(df_test, df_train)
  df_final <- df_merged[,c(cols,562,563)]

  #Find the means of every measurement, grouped by subject and action
  result <- df_final %>% group_by(subject, action) %>% summarise_all(mean)
  
  #Output the file
  write.table(result, "result.txt", row.name=FALSE)
}