run_analysis <- function(dataDir)
{
  ##Ensure dplyr installed and correct version
  if (!require("dplyr",character.only = TRUE))
  {
    install.packages("dplyr",dep=TRUE)
    if(!require("dplyr",character.only = TRUE)) stop("Dplyr package not found")
    if(packageVersion("dplyr") < "3.0.3") stop ("Dplyr package v3.0.3+ required")
  }
  
  ##First get all the URLs for the files required. 
  urlTestTextX <- paste(dataDir, "/test/X_test.txt", sep="")
  urlTrainTestX <- paste(dataDir, "/train/X_train.txt", sep="")
  testXLabels <- paste(dataDir, "/features.txt", sep="")
  
  ##Read the main table data 
  train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
  test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
  
  ##Add the column names from features.txt file. 
  names(train) <- readLines("./data/UCI HAR Dataset/features.txt")
  names(test) <- readLines("./data/UCI HAR Dataset/features.txt")
  
  ##Add the Subject column data from subject to each test and train data.
  Subject <- readLines("./data/UCI HAR Dataset/train/subject_train.txt")
  train <- cbind(train, Subject)
  Subject <- readLines("./data/UCI HAR Dataset/test/subject_test.txt")
  test <- cbind(test, Subject)
  
  ##Add the Activity values from the y data. 
  ActivityLabels <- readLines("./data/UCI HAR Dataset/activity_labels.txt")
  Activity <- readLines("./data/UCI HAR Dataset/train/y_train.txt")
  train <- cbind(train, Activity)
  train$Activity <- ActivityLabels[train$Activity]
  Activity <- readLines("./data/UCI HAR Dataset/test/y_test.txt")
  test <- cbind(test, Activity)
  test$Activity <- ActivityLabels[test$Activity]

  ##Combine the two data sets together.   
  comb <- rbind(test,train)

  ##Filter the data to remove any unwanted data. 
  ##Keep any columns with 'mean()', 'std()', 'Activity' or 'Subject'
  filteredCombSet <- subset(comb, select=grep("std\\(\\)|mean\\(\\)|Activity|Subject",names(comb), value=TRUE))

  ##Create new dataset and arrange columns.
  full_df <- tbl_df(filteredCombSet)
  full_df <- arrange(full_df, Activity, Subject)
  
  ##Produce summary of avg of each variable for each subject and activity. 
  grouped <- group_by(full_df, Activity, Subject)
  summaryData <- grouped %>% summarise_each(funs(mean))

  ##Return completed summary data. 
  summaryData
  ##write.table(test, file="summaryData.txt", row.name=FALSE, sep=" ")
}
