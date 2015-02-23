classproject <- function() {
        ## Download and unzip files
        ## Copy needed files to data directory
        
        ## Load features
        ## This are the column names for all observations
        features <- read.table("features.txt")
        newnames <- as.character(features$V2)

        ## Load activity
        activity <- read.table("activity_labels.txt")
        setnames(activity, old="V1", new="activity_id")
        setnames(activity, old="V2", new="activity")
        
        ## Train
                ## Load X_Train data
                x_train <- read.table("X_train.txt")
                
                ## Relabel columns with feature names
                setnames(x_train, old=colnames(x_train), new=newnames)
                
                ## Look for variables with mean and std
                targetcols <- c(matchcols(x_train, with=c("mean")),matchcols(x_train, with=c("std")))
                
                ## Redefine x_train to only keep target columns
                x_train <- x_train[,targetcols]
                
                ## Load train subjects
                subject_train <- read.table("subject_train.txt")
                setnames(subject_train, old="V1", new="subject")
                
                ## Load train activities
                y_train <- read.table("y_train.txt")
                setnames(y_train, old="V1", new="activity_id")
                y_train <- merge(y_train,activity,by.x="activity_id",by.y="activity_id")
        
                ## Column bind 
                ## y_train and subject
                train <- cbind(y_train,subject_train)
        
                ## train and observations
                train <- cbind(train,x_train)
        
                ## remove activity_id column
                train <- train[,colnames(train) != "activity_id"]
                train$subject <- factor(train$subject)

        ## Test
                ## Load X_test data
                x_test <- read.table("X_test.txt")
                
                ## Relabel columns with feature names
                setnames(x_test, old=colnames(x_test), new=newnames)
                
                ## Look for variables with mean and std
                targetcols <- c(matchcols(x_test, with=c("mean")),matchcols(x_test, with=c("std")))
                
                ## Redefine x_test to only keep target columns
                x_test <- x_test[,targetcols]
                
                ## Load test subjects
                subject_test <- read.table("subject_test.txt")
                setnames(subject_test, old="V1", new="subject")
                
                ## Load test activities
                y_test <- read.table("y_test.txt")
                setnames(y_test, old="V1", new="activity_id")
                y_test <- merge(y_test,activity,by.x="activity_id",by.y="activity_id")
                
                ## Column bind 
                ## y_test and subject
                test <- cbind(y_test,subject_test)
                
                ## test and observations
                test <- cbind(test,x_test)
                
                ## remove activity_id column
                test <- test[,colnames(test) != "activity_id"] 
                test$subject <- factor(test$subject)
        
        combined <- rbind(test,train)
        combined <- data.table(combined)
        results <- combined[,lapply(.SD,mean),by=.(activity,subject)]
        write.table(results, "results.txt",row.names = F, col.names = F)
}
