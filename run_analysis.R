run_analysis <- function() {
    ## Script that takes raw data from the UCI HAR Dataset
    ## and creates a tiny data set for future analysis, 
    ## doing the following steps:
    
    ## 1- Merges the training and the test sets to create 
    ## one data set.
    
    ## 2- Extracts only the measurements on the mean and 
    ## standard deviation for each measurement. 
    
    ## 3- Uses descriptive activity names to name the 
    ## activities in the data set.
    
    ## 4- Appropriately labels the data set with descriptive 
    ## variable names. 
    
    ## 5- From the data set in step 4, creates a second, 
    ## independent tidy data set with the average of each 
    ## variable for each activity and each subject.
    ########################################################   
    
    
    ## Step 1:
    ## Merges the training and the test sets to create 
    ## one data set.
    ########################################################
    
    # Paths to test and train data
    path_X_test <- "test/X_test.txt"
    path_X_train <- "train/X_train.txt"
    path_subject_test <- "test/subject_test.txt"
    path_subject_train <- "train/subject_train.txt"
    path_y_test <- "test/y_test.txt"
    path_y_train <- "train/y_train.txt"
    
    # Read data from files
    X_test <- read.table(path_X_test)
    X_train <- read.table(path_X_train)
    subject_test <- read.table(path_subject_test)
    subject_train <- read.table(path_subject_train)
    y_test <- read.table(path_y_test)
    y_train <- read.table(path_y_train)
    
    # Bind Data from test and train datasets
    X_bind <- rbind(X_test, X_train)
    subject_bind <- rbind(subject_test, subject_train)
    y_bind <- rbind(y_test, y_train)
    
    # Merge data from X, subject and y
    y_subject_merge <- cbind(y_bind, subject_bind)
    names(y_subject_merge) <- c("Activity", "Subject")
    X_merge <- cbind(X_bind, y_subject_merge)
    
    
    ## Step 2:
    ## Extracts only the measurements on the mean and 
    ## standard deviation for each measurement.
    ########################################################
    
    # Find which variables correspond to the calculation
    # of the mean and standard deviation for each measure
    
    # Path to file with variable names of merged data
    path_features <- "features.txt"
    features <- read.table(path_features)
    
    # Extract vector with column numbers of mean()
    # and std() calculations
    mean_columns <- grep("-mean()", features$V2, fixed = T)
    std_columns <- grep("-std()", features$V2, fixed = T)
    activity_column <- grep("^Activity$", names(X_merge))
    subject_column <- grep("^Subject$", names(X_merge))
    
    # Merge and sort columns to preserve original dataset
    # order
    mean_std_columns <- append(mean_columns, std_columns)
    activity_subject_columns <- append(activity_column, subject_column)
    all_columns <- append(mean_std_columns, activity_subject_columns)
    all_columns <- sort(all_columns)
    
    # Subset X_merge to contain only the columns with
    # mean(), std() calculations, "Activity" and
    # "Subject"columns
    X_mean_std <- X_merge[ , all_columns]
    
    
    ## Step 3:
    ## Uses descriptive activity names to name the 
    ## activities in the data set.
    ########################################################
    
    # Read the activity_labels
    path_activity_labels <- "activity_labels.txt"
    activity_labels <- read.table(path_activity_labels)
    
    # Change class of Activity variable to factor
    X_mean_std$Activity <- as.factor(X_mean_std$Activity)
    
    # Change levels names to descriptive ones
    levels(X_mean_std$Activity) <- activity_labels[["V2"]]
    
    
    ## Step 4:
    ## Appropriately labels the data set with descriptive 
    ## variable names.
    ######################################################## 
    
    # Change column names of X_mean_std with descriptive
    # ones
    mean_std_columns <- sort(mean_std_columns)
    column_names <- features[mean_std_columns, "V2"]
    column_names <- as.character(column_names) # changing class so
                                               # append can work
    names(X_mean_std) <- append(column_names, c("Activity", "Subject"))
    
    
    ## Step 5:
    ## From the data set in step 4, creates a second, 
    ## independent tidy data set with the average of each 
    ## variable for each activity and each subject.
    ######################################################## 
    
    # In order to use "group_by" and "summarize_each" functions,
    # importing the "dplyr" package
    library(dplyr)
    
    # Group dataset by activity and subject
    X_gb_act_sub <- group_by(X_mean_std, Activity, Subject)
    
    # Calculate the mean of each variable at the group
    result <- summarize_each(X_gb_act_sub, funs(mean))
    
    # Write the result into a file
    path_result <- "result.txt"
    write.table(result, file = path_result, row.names = F)
    
}
