        

        ##1. Checks if the file exists and downloads it if it doesn't
        URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        file <- "Dataset.zip"
        
        if (!file.exists(file)){
                data <- download.file(URL, destfile = file)
        }
        #Uznips the dataset
        unzip(file)
        
        ##2. Read files
        
        #reads activity_type and activity_labels
        activity_type <- read.table(".\\UCI HAR Dataset\\features.txt", col.names = c('activityId','activityType'))
        activity_labels <- read.table(".\\UCI HAR Dataset\\activity_labels.txt", col.names = c("Activity_Code", "Label"))
        
        #reads subject_train
        subject_train <- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt", col.names = "subjectId")
        subject_test <- read.table(".\\UCI HAR Dataset\\test\\subject_test.txt", col.names = "subjectId")
        
        #reads the x_train and y_train; x_test and the x_test; 
        y_train <- read.table(".\\UCI HAR Dataset\\train\\Y_train.txt", col.names = "activityId")
        x_train <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt", col.names = activity_type[,2])
        
        #reads x_label and activity_labels
        y_test <- read.table(".\\UCI HAR Dataset\\test\\Y_test.txt", col.names = c("activityId"))
        x_test <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt", col.names = activity_type[,2])
        
        ##3. Merge sets
        #Merges the train and test sets
        train <- cbind(y_train,subject_train,x_train);
        test <- cbind(y_test,subject_test,x_test);
        #Merge and create the final set
        finalData <- rbind(train,test)
        
        # 4. Extract only the measurements on the mean and standard deviation for each measurement. 
        
        # Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
        logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
        
        # Subset finalData table based on the logicalVector to keep only desired columns
        finalData = finalData[logicalVector==TRUE];
        
        # 5. Use descriptive activity names to name the activities in the data set
        
        # Merge the finalData set with the acitivityType table to include descriptive activity names
        finalData = merge(finalData,activity_type,by='activityId',all.x=TRUE);
        
        # Updating the colNames vector to include the new column names after merge
        colNames  = colnames(finalData); 
        
        # 6. Appropriately label the data set with descriptive activity names. 
        
        # Cleaning up the variable names
        for (i in 1:length(colNames)) 
        {
                colNames[i] = gsub("\\()","",colNames[i])
                colNames[i] = gsub("-std$","StdDev",colNames[i])
                colNames[i] = gsub("-mean","Mean",colNames[i])
                colNames[i] = gsub("^(t)","time",colNames[i])
                colNames[i] = gsub("^(f)","freq",colNames[i])
                colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
                colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
                colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
                colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
                colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
                colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
                colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
        };
        
        # Reassigning the new descriptive column names to the finalData set
        colnames(finalData) = colNames;
        
        # 7. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
        
        # Create a new table, finalDataNoActivityType without the activityType column
        finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];
        
        # Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
        tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);
        
        # Merging the tidyData with activityType to include descriptive acitvity names
        tidyData    = merge(tidyData,activity_type,by='activityId',all.x=TRUE);
        
        # Export the tidyData set 
        write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');