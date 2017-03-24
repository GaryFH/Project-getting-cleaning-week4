##Project-getting-cleaning-week4 

## First step was to download required files from:
##(https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip )
##Next I unzipped the files and stored them in my working directory (most effectively done outside of R)
##Important files were:  "activity_labels.txt"   "features.txt" 
##check for txt files in working directory with:
getwd() ##returns [1] "activity_labels.txt"  [2] "features.txt" ##  [7] "subject_test.txt"                        
                ##[8] "subject_train.txt"    [10] "X_test.txt"      [11] "X_train.txt"                            
                ##[12] "y_test.txt"          [13] "y_train.txt"

##Next make a table of activity labels and a table of features from above text files.

        activity<-read.table("activity_labels.txt",stringsAsFactors = FALSE)

        features<-read.table("features.txt",stringsAsFactors = FALSE) 

##next view data/tables
activity ## shows activity dataframe
features ## shows features data.frame

##Use grep to extract wanted measurements and tidy up the data

        f_needed<-grep(".*mean.*|.*std.*", features[,2])

        names_needed<-features[f_needed,2]
        names_needed = gsub('-mean', 'Mean', names_needed)
        names_needed = gsub('-std', 'Std', names_needed)
        names_needed<-gsub('[-()]', '', names_needed)

##Next load 3 test train datasets and 3 train datasets

        testx<-read.table("X_test.txt")[f_needed]
        testy<-read.table("y_test.txt")
        testsubject<-read.table("subject_test.txt")

        trainx<-read.table("X_train.txt")[f_needed]
        trainy<-read.table("y_train.txt")
        trainsubject<-read.table("subject_train.txt")
        
##combine all test dataframes into one testall dataframe
        testall<-cbind(testsubject, testy, testx)


##combine all train dataframes into one trainall dataframe
        trainall<-cbind(trainsubject, trainy, trainx)
        
##next combine testall and trainall into one dataframe with rbind
        allinfo<-rbind(trainall,testall)
        colnames(allinfo)<-c("subject","activity",names_needed)
        allinfodf<-tbl_df(allinfo) ##I like tbl_df - easier to explore
        
        
        
##From the data above, create a second, independent tidy data set
## with the average of each variable for each activity and each subject. 
        
        
        ##sumallinfodf<-select(allinfodf,mean(c(tBodyAccMeanX:fBodyBodyGyroJerkMagMeanFreq)))
        
        ##allinfodf_mean<-select(allinfodf,mean(tBodyAccMeanX))
        
       ## allinfodf_mean <- ddply(allinfodf, .(subject, activity), function(x) colMeans(x[, 1:ncol(allinfodf)]))
        
        ##write.table(allinfodf_mean, "averages_data.txt", row.name=FALSE)
        
        
        avgdata <- aggregate(. ~subject + activity,allinfodf, mean)
        avgdata <- avgdata[order(avgdata$subject, avgdata$activity),]
        avgdatadf<-tbl_df(avgdata)##I like to explore with tbl_df
        
##write a text table called "avgdata.txt" as part of final requirement
        write.table(avgdata, "avgdata.txt", row.name=FALSE)    
        
        
        
        
        
        
       