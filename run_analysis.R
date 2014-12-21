run_analysis <- function(dataDir)
{
  ##To use the function, requires "dplyr" version "3.0.3" or above. 
  ##Parameter 'dataDir' is where the 'UCI HAR Dataset' folder exists (i.e. run_analysis("./data/UCI HAR Dataset") )

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
  featuresLabels <- paste(dataDir, "/features.txt", sep="")
  
  ##Read the main table data 
  train <- read.table(urlTrainTestX)
  test <- read.table(urlTestTextX)
  
  ##Add the column names from features.txt file. 
  names(train) <- readLines(featuresLabels)
  names(test) <- readLines(featuresLabels)
  
  ##Add the Subject column data from subject to each test and train data.
  Subject <- readLines(paste(dataDir, "/train/subject_train.txt", sep=""))
  train <- cbind(train, Subject)
  Subject <- readLines(paste(dataDir, "/test/subject_test.txt", sep=""))
  test <- cbind(test, Subject)
  
  ##Add the Activity values from the y data. 
  ActivityLabels <- readLines(paste(dataDir, "/activity_labels.txt", sep=""))
  Activity <- readLines(paste(dataDir, "/train/y_train.txt", sep=""))
  train <- cbind(train, Activity)
  train$Activity <- ActivityLabels[train$Activity]
  Activity <- readLines(paste(dataDir, "/test/y_test.txt", sep=""))
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
  
  ##Comand to write table. 
  ##write.table(test, file="summaryData.txt", row.name=FALSE, sep=" ", eol="\r\n")
}
