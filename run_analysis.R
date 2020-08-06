run_analysis <- function() {

  headers <- read.csv("UCI HAR Dataset/features.txt", sep = " ", header = FALSE)
  
  #get the columns needed: mean() and std()
  cols <- grep("(mean|std)\\(\\)", headers[,2])
  
  #Tidy up the header
  #  1. Replace t by Time, f by Frequency
  #  2. Remove '.', ',', '-', '(', ')'
  tidied_header <- headers[,2]
  tidied_header <- gsub("^t(.*)", "Time\\1", tidied_header)
  tidied_header <- gsub("^f(.*)", "Frequency\\1", tidied_header)
  tidied_header <- gsub("[.|,|\\(|\\)]", "", tidied_header)
  tidied_header <- gsub("-","",tidied_header)
  
  #read test and train files with tidied headers
  df_test <- read.fwf("UCI HAR Dataset/test/X_test.txt", widths = rep(16, 561), header = FALSE, col.names = tidied_header)
  df_train <- read.fwf("UCI HAR Dataset/train/X_train.txt", widths = rep(16, 561), header = FALSE, col.names = tidied_header)
  


}